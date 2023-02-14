#### SETUP ####
here::i_am("1_Scripts/0_data_cleaning_state.R")
source("global_options.R")

#### STATE POPULATION DATA ####
data(state_census)
data(hhs_regions)
filter_date = "2021-01-01"

#### HOSPITALIZATIONS ####
h = read.csv(here("0_Data", "Raw", "hosps.csv")) %>% group_by(state) %>%
  mutate(date = as.Date(date, format = "%Y/%m/%d")) %>%
  arrange(date) %>%
  mutate(
    # confirmed admissions
    admits_confirmed = previous_day_admission_adult_covid_confirmed + previous_day_admission_pediatric_covid_confirmed,
    admits_confirmed_avg = rollmean(admits_confirmed, k = 7, align = "right", na.pad = TRUE, na.rm = T),
    
    # confirmed ICU patients
    icu_confirmed_avg = rollmean(staffed_icu_adult_patients_confirmed_covid, k = 7, align = "right", na.pad = TRUE, na.rm = T),
    
    # percentage of inpatient beds with confirmed or suspected COVID COVID
    perc_covid_ALL = rollmean(percent_of_inpatients_with_covid, k = 7, align = "right", na.pad = TRUE, na.rm = T),
    
    # percentage of COVID inpatients that are confirmed
    denom_factor = ifelse(total_adult_patients_hospitalized_confirmed_and_suspected_covid >= 0, total_adult_patients_hospitalized_confirmed_and_suspected_covid, NA) + 
           ifelse(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid >= 0, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid, NA),
    num_factor = ifelse(total_adult_patients_hospitalized_confirmed_covid >= 0, total_adult_patients_hospitalized_confirmed_covid, NA) + 
           ifelse(total_pediatric_patients_hospitalized_confirmed_covid >= 0, total_pediatric_patients_hospitalized_confirmed_covid, NA),
    
    # align NAs
    num_factor = ifelse(is.na(denom_factor), NA, num_factor),
    denom_factor = ifelse(is.na(num_factor), NA, denom_factor),
    
    # set up and clean average factor
    factor_base = ifelse(!is.na(num_factor) & !is.na(denom_factor) & denom_factor!=0, num_factor/denom_factor, 1),
    factor_count = factor_base>1, 
    factor = ifelse(factor_base > 1, 1, factor_base),
    perc_covid = rollmean(percent_of_inpatients_with_covid*factor, k = 7, align = "right", na.pad = TRUE, na.rm = T),
    
    # explore sensitivity to different factors
    factor_avg1 = rollmean(num_factor, k = 7, align = "right", na.pad = TRUE, na.rm = F)/rollmean(denom_factor, k = 7, align = "right", na.pad = TRUE, na.rm = F),
    factor_avg1 = ifelse(is.na(factor_avg1) | factor_avg1 > 1, 1, factor_avg1),
    factor_avg2 = rollmean(num_factor, k = 7, align = "right", na.pad = TRUE, na.rm = T)/rollmean(denom_factor, k = 7, align = "right", na.pad = TRUE, na.rm = T),
    factor_avg2 = ifelse(is.na(factor_avg2) | factor_avg2 > 1, 1, factor_avg2),
    perc_covid_v2 = perc_covid_ALL*factor_avg2,
    
    ) %>%
  
  # filter on date
  filter(date>=filter_date) %>%
  
  # select variables for easier data checks
  dplyr::select(state, date, previous_day_admission_adult_covid_confirmed, previous_day_admission_pediatric_covid_confirmed, 
                admits_confirmed, admits_confirmed_avg, staffed_icu_adult_patients_confirmed_covid, icu_confirmed_avg,
                total_adult_patients_hospitalized_confirmed_and_suspected_covid, total_pediatric_patients_hospitalized_confirmed_and_suspected_covid,
                total_adult_patients_hospitalized_confirmed_covid, total_pediatric_patients_hospitalized_confirmed_covid,
                num_factor, denom_factor, factor_base, factor, factor_count, factor_avg1, factor_avg2, perc_covid_ALL, perc_covid, perc_covid_v2)


## CHECKS ##
# NAs/negatives
h %>% 
  gather(var, value, previous_day_admission_adult_covid_confirmed, 
         previous_day_admission_pediatric_covid_confirmed, 
         staffed_icu_adult_patients_confirmed_covid, factor, factor_avg2) %>%
  group_by(var) %>%
  summarize(min(value, na.rm = T), max(value, na.rm = T), sum(is.na(value)))

# check on factor issues
mean(h$factor_count==1)
h_chk = h %>% filter(factor_count)
length(unique(h_chk$date))
table(h_chk$state) # assume anomalies in LA (reversed definitions?), but just using 1 for the ratio of confirmed:all in this case

# check on rolling averages
h %>% filter(date >= ("2022-02-01") & date <= "2022-02-07") %>%
  group_by(state) %>% summarize(a = mean(admits_confirmed), b = admits_confirmed_avg[7]) %>%
  ungroup() %>% summarize(mean(a==b))

# plot to eyeball
ggplot(h %>% filter(state=="MA") %>% gather(var, value, admits_confirmed_avg, icu_confirmed_avg),
       aes(x = date, y = value)) + facet_wrap(.~var) + geom_line()

#### ANOMALOUS MD DATA ####
# dates of missing data
dates = seq(as.Date("2021-12-05"), as.Date("2021-12-19"), "days")

# pull in data from the state
m = read.csv(here("0_Data", "Raw", "MDCOVID19_CasesPer100KpopulationStatewide.csv")) %>%
  separate(ReportDate, into = c("Date", "Time"), sep = "\ ") %>%
  mutate(date = as.Date(Date, format = "%Y/%m/%d"),
         state = "Maryland") %>% filter(date %in% dates)

#### CASE DATA ####
df = read.csv(here("0_Data", "Raw", "us-states.csv")) %>% 
  
  # join to state data
  left_join(state_census %>% dplyr::select(NAME, POPESTIMATE2019, ABBR), c("state"="NAME")) %>%
  filter(!is.na(POPESTIMATE2019)) %>%
  
  # group by state
  group_by(state) %>% 
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-")) %>% 
  
  arrange(ymd, .by_group = TRUE) %>%
  
  # filter out PR & Virgin Islands & arrange
  filter(!state%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "U.S. Virgin Islands", 
                     "United States")) %>%
  arrange(state, ymd) %>%
  
  # join to hospital data
  left_join(h %>% dplyr::select(date, state,
                                admits_confirmed_avg, perc_covid, icu_confirmed_avg), 
            c("ABBR"="state", "ymd"="date")) %>%
  mutate(admits_confirmed_100K = admits_confirmed_avg/POPESTIMATE2019*100000,
         icu_100K = icu_confirmed_avg/POPESTIMATE2019*100000) %>%
  
  # link to MD
  left_join(m %>% dplyr::select(date, state, Statewide), 
            c("state" = "state", "ymd" = "date")) %>%
  
  # estimate CDC metrics
  mutate(
    
  # fix MD as needed
  cases_avg_per_100k = ifelse(is.na(Statewide), cases_avg_per_100k, Statewide),

  # remove NAs from bed percentages
  perc_covid = ifelse(is.na(perc_covid), 0, perc_covid),
  
  # define CDC "high"
  cdc_flag_1 = (cases_avg_per_100k > 200/7 & (admits_confirmed_100K > 10/7 | perc_covid > .1)),  # over 200/100K 7-d
  cdc_flag_2 = (cases_avg_per_100k < 200/7 & (admits_confirmed_100K > 20/7 | perc_covid > .15)), # under 200/100K 7-d
  cdc_flag = cdc_flag_1 | cdc_flag_2,
 
  # future deaths per 100k
  deaths_21_lag_100k = lead(deaths_avg_per_100k, 21), 

  # future ICU admissions
  icu_21_lag_100K = lead(icu_100K, 21), 
  
  # past cases 
  cases_lag_21_100K = lag(cases_avg_per_100k, 21), 

  # hosps per 100K
  admits_7_lag = lead(admits_confirmed_avg, 7),
  admits_7d_ago = lag(admits_confirmed_avg, 7),
  admits_21d_ago = lag(admits_confirmed_avg, 21),
  admits_28d_ago = lag(admits_confirmed_avg, 28),
  
  # day of the week
  dotw = weekdays(ymd),
  
  # check completeness/duplicates
  chk = paste(ymd, state)) %>% filter(ymd>=filter_date)

## CHECKS ##
# check for duplicates
k = table(df$chk)
k[k > 1]

# check for completeness by state
table(df$state)

# check on lags
df %>% group_by(state) %>%
  summarize(a = deaths_21_lag_100k[date=="2022-02-01"], b = deaths_avg_per_100k[date=="2022-02-22"]) %>%
  ungroup() %>% summarize(mean(a==b))

#### EXPORT CLEANED DATA WITH KEY METRICS ####
d_out_pre_state = df %>% 
  
  # start in March
  #filter(ymd >= "2021-03-01") %>%
  
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday")  %>%
  
  # group by state + date to calculate relevant benchmarks
  group_by(state) %>% arrange(ymd) %>%
  
  # make a 14d average as alternative outcome
  mutate(deaths_21_lag_100k_14d = rollmean(deaths_21_lag_100k, k = 2, align = "right", na.pad = TRUE, na.rm = T)*14) %>%
  ungroup() %>% 
  
  # put metrics on weekly scale
  mutate(deaths_weekly = deaths_21_lag_100k*7,
        admits_weekly = admits_confirmed_100K*7,
        icu_weekly = icu_100K,
        cases_weekly = round(cases_avg_per_100k*7),
        cfr = deaths_avg_per_100k/cases_lag_21_100K*100,
        perc_covid_100 = perc_covid*100,
        cfr_in_21d = deaths_weekly/cases_weekly*100) %>%
  arrange(ymd) %>% group_by(state) %>%
  mutate(

    # current outcomes
    zeke_time_0 = deaths_avg_per_100k*7 > 1,
    two_zeke_time_0 = deaths_avg_per_100k*7 > 2,
    
    # 7-day outcomes
    half_zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > .5,
    chk2 = lead(deaths_avg_per_100k, 3),
    zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > 1,
    two_zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > 2,
    icu_2_time_3 = icu_21_lag_100K > 2,
    
    # 14-day outcomes
    zeke_time_3_14d = deaths_21_lag_100k_14d > 2,
    two_zeke_time_3_14d = deaths_21_lag_100k_14d > 4) %>%
  group_by(ymd) %>%
  mutate(weight = POPESTIMATE2019/sum(POPESTIMATE2019),
         weight_alt = 1/length(POPESTIMATE2019))

## CHECKS ##
# check on lags
mean(d_out_pre_state$chk2 == d_out_pre_state$deaths_21_lag_100k, na.rm = T)

# check on missing data
d_out_pre_state %>% gather(var, value, deaths_weekly, admits_weekly, cases_weekly, perc_covid_100) %>%
  filter(date>="2022-09-01" & date<="2022-10-01") %>%
  ungroup() %>% summarize(sum(is.na(value)))


#### SAVE CLEANED DATA ####
save(d_out_pre_state, file = here("0_Data", "Cleaned", "state_time_data.RData"))
