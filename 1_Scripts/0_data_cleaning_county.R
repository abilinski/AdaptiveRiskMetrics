#### SETUP ####
here::i_am("1_Scripts/0_data_cleaning_county.R")
source("global_options.R")

#### POPULATION DATA ####
data(county_census)
data(state_census)
filter_date = "2021-01-01"

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
  
  # join to counties
  left_join(c, c("health_service_area_number" = "health_service_area_number")) %>% ungroup() %>%
  
  # rename NYC counties to just 1 NYC entry
  filter(!fips%in%c(36005, 36047, 36061, 36081)) %>%
  mutate(fips = ifelse(fips == c(36085), 36998, fips)) %>%
  
  # Kansas City counties
  filter(!fips%in%c(29037, 29165, 29047)) %>%
  mutate(fips = ifelse(fips == c(29095), 29998, fips)) %>%
  
  # Joplin, Mo. counties
  filter(!fips%in%c(29145)) %>%
  mutate(fips = ifelse(fips == c(29097), 29997, fips)) %>%
  
  # Alaska 1
  filter(!fips%in%c(2164)) %>%
  mutate(fips = ifelse(fips == c(2060), 02997, fips)) %>%
  
  # Alaska 2
  filter(!fips%in%c(2105)) %>%
  mutate(fips = ifelse(fips == c(2282), 02998, fips)) %>%
  
  # filter date
  filter(date>filter_date)
  

## CHECKS ##
# missingness
g = h %>% group_by(health_service_area, health_service_area_population.x) %>%
  summarize(missing_kid = sum(missing_kid),
            missing_kid_perc = mean(missing_kid_perc),
            missing_adult = sum(missing_adult),
            max_perc_missing = max(missing_adult_perc),
            missing_adult_perc = mean(missing_adult_perc))
table(g$missing_adult_perc)
table(g$max_perc_missing)

sum(g$missing_adult_perc>0.01)
sum(g$max_perc_missing>.4)


# check for duplicates
k = table(paste(h$fips, h$date))
k[k > 1]

# check for completeness
chk_complete = h %>% filter(date >= "2021-02-01" & date <= "2022-10-01") %>%
  group_by(fips, state) %>% summarize(n = n())


# explore data (just to eyeball)
# by fips
chk = h %>% filter(fips==25021)
ggplot(chk %>% gather(var, value, admits_confirmed_avg,
                      admits_confirmed_100K, icu_confirmed_avg, icu_100K, perc_covid), 
       aes(x = date, y = value)) + geom_line() + facet_wrap(.~var, scale = "free")

# by state
chk = h %>% dplyr::select(state, date, admits_confirmed_avg,
                          admits_confirmed_100K, icu_confirmed_avg, icu_100K, perc_covid,
                          health_service_area_number)%>% unique() %>%
  filter(state=="Massachusetts")
ggplot(chk %>% gather(var, value, admits_confirmed_avg,
                      admits_confirmed_100K, icu_confirmed_avg, icu_100K, perc_covid) %>%
         group_by(var, date) %>% summarize(value = sum(value)), 
       aes(x = date, y = value)) + geom_line() + facet_wrap(.~var, scale = "free")


#### COUNTY CENSUS ####
# adjust for NYC
county_census = county_census %>% 
  mutate(fips = as.numeric(FIPS),
         # rename for NYC
         fips = ifelse(fips %in% c(36005, 36047, 36061, 36081, 36085),
                                                            36998, fips),
         fips = ifelse(fips %in% c(29037, 29165, 29047,29095), 29998, fips),
         fips = ifelse(fips %in% c(29145, 29097), 29997, fips),         
         fips = ifelse(fips %in% c(2164, 2060), 02997, fips), 
         fips = ifelse(fips %in% c(2105, 2282), 02998, fips)) %>%
  group_by(fips, STNAME) %>% summarize(POPESTIMATE2019 = sum(POPESTIMATE2019),
                                        CTYNAME = CTYNAME[1]) %>%
  mutate(CTYNAME = ifelse(fips==36998, "New York City", CTYNAME))

# check missing/duplicates
k = table(county_census$fips)
k[k > 1]

#### CASE DATA ####
df0 = read.csv(here("0_Data", "Raw", "us-counties-2021.csv")) %>% 
  bind_rows(read.csv(here("0_Data", "Raw", "us-counties-2020.csv"))) %>%
  bind_rows(read.csv(here("0_Data", "Raw", "us-counties-2022.csv"))) %>%
  mutate(fips = as.numeric(sub("USA-", "", geoid)),
         fips = ifelse(fips %in% c(29037, 29165, 29047,29095), 29998, fips),
         fips = ifelse(fips %in% c(29145, 29097), 29997, fips),
         ) %>%
  
  # filter out PR & Virgin Islands & arrange
  filter(!state%in%c("American Samoa", "Guam",
                     "Northern Mariana Islands", 
                     "Puerto Rico Commonwealth",
                     "Virgin Islands", 
                     "United States")) %>%
  
  group_by(fips, date, county, state) %>%
  summarize(cases_avg = sum(cases_avg, na.rm = T), deaths_avg = sum(deaths_avg, na.rm = T),
            chk1 = cases_avg_per_100k[1], chk2 = deaths_avg_per_100k[1]) %>%
  # join to county data
  left_join(county_census, c("fips"="fips"))

# df0 --> county_census
# Only ones that didn't match were unknown
# View(df0 %>% filter(is.na(POPESTIMATE2019)) %>% ungroup() %>% dplyr::select(state, fips, county) %>% unique())

# county_census --> df0
# Non-matches were full states or outside study scope
# View(county_census %>% filter(!fips%in%df0$fips))

# h --> df
# Only NA (no match to HSA) or outside of study scope
# View(h %>% filter(!fips %in% df0$fips) %>% ungroup() %>% unique())

# finish joins
df = df0 %>%
  filter(!is.na(POPESTIMATE2019)) %>%
  
  # join to state data
  left_join(s, c("state"="NAME")) %>%
  # group by state
  group_by(fips) %>% 
  
  # format dates
  mutate(ymd = as.Date(date, format = "%Y-%m-%d"),
         year = year(ymd),
         week = epiweek(ymd),
         year_wk = paste(year, week, sep = "-"),
         cases_avg_per_100k = cases_avg/POPESTIMATE2019*100000,
         deaths_avg_per_100k = deaths_avg/POPESTIMATE2019*100000) %>% 
  
  arrange(fips, ymd) %>%
  
  # join to hospital data
  left_join(h %>% dplyr::select(date, fips, health_service_area_population.x,
                                admits_confirmed_avg, perc_covid,
                                admits_confirmed_100K, icu_confirmed_avg, icu_100K), 
            c("fips"="fips", "ymd"="date")) %>%
  
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
  chk = paste(ymd, fips)) %>% filter(ymd>=filter_date) %>% ungroup() %>%
  mutate(county_rank = rank(-1*POPESTIMATE2019))

## CHECKS ##

# duplicates/missing
k = table(df$chk)
k[k > 1]

# check for completeness by county
min(table(df$fips))
max(table(df$fips))

# check on lags
df %>% group_by(fips) %>%
  summarize(a = deaths_21_lag_100k[date=="2022-02-01"], b = deaths_avg_per_100k[date=="2022-02-22"]) %>%
  ungroup() %>% summarize(mean(a==b))

#### EXPORT CLEANED DATA WITH KEY METRICS ####
d_out_pre_cty = df %>% 
  
  # keep Wednesdays (when CDC updates criteria)
  filter(dotw == "Wednesday") %>%
  
  # start in March
  filter(date >= "2021-03-01") %>%
  
  # group by county + date to calculate relevant benchmarks
  group_by(fips) %>% arrange(ymd) %>%
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
         state = fips) %>%
  group_by(ymd) %>%
  mutate(weight = POPESTIMATE2019/sum(POPESTIMATE2019),
         weight_alt = 1/length(POPESTIMATE2019)) %>%
  
  # filter out 3 fips codes with no HSA
  filter(!fips %in% c(48067, 48203, 48315))

## CHECKS ##
# check on lags
mean(d_out_pre_cty$chk2 == d_out_pre_cty$deaths_21_lag_100k, na.rm = T)

# check on missing data
d_out_pre_cty %>% gather(var, value, deaths_weekly, admits_weekly, cases_weekly, perc_covid_100) %>%
  filter(date<="2022-10-01") %>%
  group_by(var) %>% summarize(sum(is.na(value)))

# check on joins
# 4 counties missing some hosp data -- after 9/1 (not an issue as only death data is used for analysis henceforth)
# View(d_out_pre_cty %>% filter(is.na(health_service_area_population.x)) %>% ungroup() %>% dplyr::select(ymd, state, fips, county) %>% unique())
View(d_out_pre_cty %>% group_by(ymd) %>% summarize(sum(POPESTIMATE2019)))

#### SAVE CLEANED DATA ####
save(d_out_pre_cty, file = here("0_Data", "Cleaned", "county_time_data.RData"))
write.csv(d_out_pre_cty, file = here("0_Data", "Cleaned", "county_time_data.csv"))



