#### SETUP ####
here::i_am("1_Scripts/0_data_cleaning_hsa.R")
source("global_options.R")

#### POPULATION DATA ####
data(county_census)
data(state_census)

# pull state abbreviation from data
s = state_census %>% dplyr::select(NAME, ABBR)

# pull HSAs
c = read.csv(here("0_Data", "Raw", "United_States_COVID-19_Community_Levels_by_County.csv")) %>%
  mutate(ymd = as.Date(date_updated, "%Y-%m-%d")+1, fips = as.numeric(county_fips)) %>%
  filter(ymd == "2022-05-06") %>%
  dplyr::select(county, county_fips, state, fips,
                county_population, health_service_area_number,
                health_service_area, health_service_area_population)

#### HOSPITALIZATIONS ####
h = read.csv(here("0_Data", "Raw", "hosps_county.csv")) %>% 
  mutate(fips = as.numeric(fips_code)) %>%
  
  # join to health services areas
  left_join(c, c("fips" = "fips")) %>%
  
  # harmonize date with case reporting dates
  mutate(date = as.Date(collection_week, format = "%Y/%m/%d")+5,
         
         # impute suppressed (1 to 5) per Salomon & Bilinski, Annals, 2022
         # admissions
         previous_day_admission_adult_covid_confirmed_7_day_sum = ifelse(previous_day_admission_adult_covid_confirmed_7_day_sum < 0, 2, previous_day_admission_adult_covid_confirmed_7_day_sum),
         previous_day_admission_pediatric_covid_confirmed_7_day_sum = ifelse(previous_day_admission_pediatric_covid_confirmed_7_day_sum < 0, .5, previous_day_admission_pediatric_covid_confirmed_7_day_sum),
         
         # bed usage
         inpatient_beds_used_covid_7_day_sum = ifelse(total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum < 0, 0, total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum) + 
           ifelse(total_adult_patients_hospitalized_confirmed_covid_7_day_sum < 0, 0, total_adult_patients_hospitalized_confirmed_covid_7_day_sum),
         inpatient_beds_7_day_sum = ifelse(inpatient_beds_7_day_avg < 0, 0, inpatient_beds_7_day_avg),
         
         # staffed ICU adult patients
         staffed_icu_adult_patients_confirmed_covid_7_day_sum = ifelse(staffed_icu_adult_patients_confirmed_covid_7_day_sum<0, .5, staffed_icu_adult_patients_confirmed_covid_7_day_sum)) %>%

  # group by HSA
  group_by(health_service_area_number, health_service_area_population, date) %>%
  summarize(
         n = n(),
         
         # admissions
         admits_confirmed = sum(previous_day_admission_adult_covid_confirmed_7_day_sum, na.rm = T) + sum(previous_day_admission_pediatric_covid_confirmed_7_day_sum, na.rm = T),

         # inpatient beds confirmed COVID
         inpt_beds_covid = sum(inpatient_beds_used_covid_7_day_sum, na.rm = T),
         
         # inpatient beds
         inpt_beds = sum(inpatient_beds_7_day_sum, na.rm = T),
         
         # ICU patients
         staffed_icu_adult_patients_confirmed_covid = sum(as.numeric(staffed_icu_adult_patients_confirmed_covid_7_day_sum), na.rm = T),
         
         # check missing percentages
         missing_kid = sum(is.na(previous_day_admission_pediatric_covid_confirmed_7_day_sum)), missing_kid_perc = mean(is.na(previous_day_admission_pediatric_covid_confirmed_7_day_sum)),
         missing_adult = sum(is.na(previous_day_admission_adult_covid_confirmed_7_day_sum)), missing_adult_perc = mean(is.na(previous_day_admission_adult_covid_confirmed_7_day_sum))) %>%
  
  
  group_by(health_service_area_number) %>% arrange(date) %>%
  mutate(# admits
         admits_confirmed_avg = admits_confirmed/7,
         admits_confirmed_avg = ifelse(is.na(admits_confirmed_avg), 0, admits_confirmed_avg),
         
         # ICU
         icu_confirmed_avg = staffed_icu_adult_patients_confirmed_covid/7,
         
         # percent bed usage
         perc_covid = inpt_beds_covid/7/inpt_beds,
         perc_covid = ifelse(perc_covid=="Inf", 0, perc_covid),
         
         # by population percentages
         admits_confirmed_100K = admits_confirmed_avg/health_service_area_population*100000,
         icu_100K = icu_confirmed_avg/health_service_area_population*100000/7) %>%
  
  # filter date
  filter(date>="2021-01-01")
  

## CHECKS ##
# check for duplicates
k = table(paste(h$health_service_area_number, h$date))
k[k > 1]

# check for completeness
chk_complete = h %>% filter(date >= "2021-02-01" & date <= "2022-10-01") %>%
  group_by(health_service_area_number) %>% summarize(n = n())

#### CASE DATA ####
df0 = read.csv(here("0_Data", "Raw", "us-counties-2021.csv")) %>% 
  bind_rows(read.csv(here("0_Data", "Raw", "us-counties-2022.csv"))) %>%
  
  # filter out PR & Virgin Islands & arrange
  filter(!state%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "Virgin Islands", 
                     "United States")) %>%

  filter(!county=="Unknown") %>%
  mutate(fips = as.numeric(sub("USA-", "", geoid)),
         state_old = state) %>%
  
  # rename edited counties
  # note that all NYT-combined counties mapped to same HSA
  # so I just picked one
  mutate(fips = ifelse(fips==36998, 36005, fips)) %>%
  mutate(fips = ifelse(fips==29998, 29037, fips)) %>%
  mutate(fips = ifelse(fips==29997, 29097, fips)) %>%
  mutate(fips = ifelse(fips==02997, 02060, fips)) %>%
  mutate(fips = ifelse(fips==02998, 02105, fips)) %>%
  
  # join to hsa
  left_join(c, c("fips" = "county_fips"))

# run checks on missing
# View(df0 %>% filter(is.na(health_service_area_population)) %>% ungroup() %>% dplyr::select(health_service_area, health_service_area_number) %>% unique())
# not concerning -- just ones that NYT combined + outside of scope
# View(c %>% filter(!county_fips %in% df0$fips) %>% ungroup() %>% unique())

# run subsequent
df = df0 %>%
  
  # average over HSA
  group_by(date, health_service_area_number, health_service_area, health_service_area_population) %>%
  summarize(cases_avg = sum(cases_avg, na.rm = T), deaths_avg = sum(deaths_avg, na.rm = T)) %>%
  mutate(cases_avg_per_100k = cases_avg/health_service_area_population*100000,
         deaths_avg_per_100k = deaths_avg/health_service_area_population*100000,
         POPESTIMATE2019 = health_service_area_population) %>%
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-")) %>% 
  
  group_by(health_service_area_number) %>%
  arrange(ymd, .by_group = TRUE) %>%

  # join to hospital data
  left_join(h %>% dplyr::select(date,
                                admits_confirmed_avg, perc_covid,
                                admits_confirmed_100K, icu_confirmed_avg, icu_100K), 
            c("health_service_area_number"="health_service_area_number", "ymd"="date")) %>%
  
  # estimate CDC metrics
  mutate(
    
  # remove NAs 
  perc_covid = ifelse(is.na(perc_covid), 0, perc_covid),
  admits_confirmed_100K = ifelse(is.na(admits_confirmed_100K), 0, admits_confirmed_100K),
  
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
  chk = paste(ymd, health_service_area_number)) %>% filter(ymd>="2021-01-01") %>% ungroup() %>%
  mutate(county_rank = rank(-1*POPESTIMATE2019))

## CHECKS ##

# duplicates/missing
k = table(df$chk)
k[k > 1]

df %>% filter(is.na(health_service_area_number))

# check for completeness by county
min(table(df$health_service_area_number))
max(table(df$health_service_area_number))

# check on lags
df %>% group_by(health_service_area_number) %>%
  summarize(a = deaths_21_lag_100k[date=="2022-02-01"], b = deaths_avg_per_100k[date=="2022-02-22"]) %>%
  ungroup() %>% summarize(mean(a==b))

#### EXPORT CLEANED DATA WITH KEY METRICS ####
d_out_pre_hsa = df %>% 
  
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday") %>%
  
  # start in March
  filter(date >= "2021-03-01") %>%
  
  # group by county + date to calculate relevant benchmarks
  group_by(health_service_area_number) %>% arrange(ymd) %>%
  mutate(deaths_21_lag_100k_14d = rollmean(deaths_21_lag_100k, k = 2, align = "right", na.pad = TRUE, na.rm = T)*14,
  
  # put metrics on weekly scale
         deaths_weekly = deaths_21_lag_100k*7,
         admits_weekly = admits_confirmed_100K*7,
         cases_weekly = round(cases_avg_per_100k*7),
         icu_weekly = icu_100K*7,
         perc_covid_100 = perc_covid*100,
         cfr = deaths_avg_per_100k/cases_lag_21_100K*100,
  
         # 7-day outcomes
         half_zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > 0.5,
         chk2 = lead(deaths_avg_per_100k, 3),
         zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > 1,
         two_zeke_time_3 = lead(deaths_avg_per_100k*7, 3) > 2,
         icu_2_time_3 = icu_21_lag_100K*7 > 2,
         perc_covid_10_time_3 = lead(perc_covid_100, 3) > 10,
         change_admits = admits_weekly - lag(admits_weekly, 1),
         change_perc = perc_covid_100 - lag(perc_covid_100, 1),
         change_cases = cases_weekly - lag(cases_weekly, 1),
  
         # 14-day outcomes
         zeke_time_3_14d = deaths_21_lag_100k_14d > 2,
         two_zeke_time_3_14d = deaths_21_lag_100k_14d > 4,
         
         # rename for coding consistency
         state = health_service_area_number) %>%
  group_by(ymd) %>%
  mutate(weight = POPESTIMATE2019/sum(POPESTIMATE2019),
         weight_alt = 1/length(POPESTIMATE2019))

## CHECKS ##
# check on lags
mean(d_out_pre_hsa$chk2 == d_out_pre_hsa$deaths_21_lag_100k, na.rm = T)

# check on missing data
d_out_pre_hsa %>% gather(var, value, deaths_weekly, admits_weekly, cases_weekly, perc_covid_100) %>%
  filter(date<="2022-10-01") %>%
  group_by(var) %>% summarize(sum(is.na(value)))

# check on missing data
d_out_pre_hsa %>% filter(is.na(health_service_area_population)) %>% ungroup() %>% dplyr::select(ymd, state, health_service_area, health_service_area_number) %>% unique()
#View(d_out_pre_hsa %>% group_by(ymd) %>% summarize(sum(health_service_area_population)))

#### SAVE CLEANED DATA ####
save(d_out_pre_hsa, file = here("0_Data", "Cleaned", "hsa_time_data.RData"))
write.csv(d_out_pre_hsa, file = here("0_Data", "Cleaned", "hsa_time_data.csv"))



