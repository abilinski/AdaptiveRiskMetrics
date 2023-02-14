#******************************** ESTIMATE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/1_tests.R")
source("global_options.R")
source(here("1_Scripts", "1_risk_metrics.R"))

# admission levels
admit_levels = c(5, 10)
case_levels = c(50, 100)
perc_levels = c(5, 10)

# load data
load(here("0_Data", "Cleaned", "state_time_data.RData"))

#### TEST OF define_metrics_DT ####
define_metrics = function(d_out_pre, admit_levels, case_levels, perc_levels){
  d_test_ind = d_out_pre %>% 
    expand_grid(admit_levels, case_levels, perc_levels) %>%
    mutate(type = paste(case_levels, admit_levels, perc_levels),
           indicator = cases_weekly >= case_levels & admits_weekly >= admit_levels &
             perc_covid_100 >= perc_levels
    ) %>%
    gather(var, value, indicator, cdc_flag) %>%
    mutate(var = ifelse(var == "indicator", type, var)) %>%
    gather(outcome_label, outcome_value, half_zeke_time_3, zeke_time_3, two_zeke_time_3) %>%
    mutate(outcome_label = factor(outcome_label, levels = c("half_zeke_time_3", "zeke_time_3", "two_zeke_time_3")),
           current_zeke = ifelse(outcome_label == "half_zeke_time_3", deaths_avg_per_100k*7>(.5),
                                 deaths_avg_per_100k*7>1),
           current_zeke = ifelse(outcome_label == "two_zeke_time_3", deaths_avg_per_100k*7>2,
                                 current_zeke),
           current_zeke = as.numeric(current_zeke))
  
  z = unique(d_test_ind$type)
  d_test_ind = d_test_ind[(d_test_ind$var=="cdc_flag" & d_test_ind$type==z[1]) | (d_test_ind$var!="cdc_flag"),]
  return(d_test_ind)
}

# test version
v1 = define_metrics(d_out_pre_state %>% filter(state%in%c("Texas", "Massachusetts")), admit_levels, case_levels, perc_levels) %>%
  mutate(var = as.character(var)) %>%
  arrange(state, date, admit_levels, case_levels, perc_levels, type, var, outcome_label) %>%
  mutate(var = as.character(var))

# operational version
v2 = define_metrics_DT(d_out_pre_state %>% filter(state%in%c("Texas", "Massachusetts")), admit_levels, case_levels, perc_levels)%>%
  mutate(var = as.character(var)) %>%
  arrange(state, date, admit_levels, case_levels, perc_levels, type, var, outcome_label) 
v3 = data.frame(v2)[,names(v1)]

compare(data.frame(v1), v3)


#### TEST OF calc_metrics_DT ####

# missing categories
test_df1 = expand_grid(obs = 1:100, outcome_label = c("test1", "test2"),
                       outcome_value = c(1,0), value = c(1,0)) %>%
  mutate(g1 = outcome_value == value, g2 = outcome_value == 1) %>%
  mutate(weight = 1/100)

g = calc_metrics_DT(data.table(test_df1), group_vars = c("outcome_label", "g1", "g2"))

# all categories
test_df2 = expand_grid(obs = 1:100, outcome_label = c("test1", "test2"),
                       outcome_value = c(1,0), value = c(1,0)) %>%
  mutate(weight = 1/100)

g = calc_metrics_DT(data.table(test_df2), group_vars = "outcome_label")

# also reviewed by JG

#### TEST OF best_measure_DT_reg ####
end_date = "2022-10-01"
d_test_ind = define_metrics_DT(d_out_pre_state %>% group_by(ymd) %>%
                                 mutate(weight = POPESTIMATE2019/sum(POPESTIMATE2019),
                                 ), 
                               admit_levels = admit_levels, case_levels, perc_levels,
                               outcomes = c("zeke_time_3", "two_zeke_time_3"))[ymd<=end_date,]
end_dates = sort(unique(d_out_pre_state$ymd[d_out_pre_state$ymd >= start_date & d_out_pre_state$ymd <= end_date]))

# weekly update
out = best_measure_DT_reg(d_test_ind = d_test_ind[var=="cdc_flag",], end_dates = end_dates)

# check training data times and length of test data
out %>% summarize(min(test_date - max_train_date), max(test_date - max_train_date), sum(weeks!=1),
                  (mean(max_train_date-min_train_date)+7)/7, mean(w))

# check number of outcomes
nrow(out)/length(end_dates)/length(unique(d_out_pre_state$state))/length(unique(out$outcome_label))

# quarterly update
out2 = best_measure_DT_reg(d_test_ind = d_test_ind[var=="cdc_flag",], end_dates = end_dates, quarter = T)

# check quarterly data
out2 %>% mutate(qy = paste(year(ymd), quarter(ymd))) %>%
  group_by(qy) %>% summarize(max(max_train_date), min(test_date-max_train_date), max(test_date-max_train_date),
                             max(weeks))


