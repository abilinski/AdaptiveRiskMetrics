#******************************** ESTIMATE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/1_tests.R")
source("global_options.R")
source(here("1_Scripts", "1_risk_metrics.R"))

# admission levels
admit_levels = c(5, 10)
case_levels = c(50, 100)
perc_levels = c(5, 10)
mult = 1

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
    gather(outcome_label, outcome_value, zeke_time_3, two_zeke_time_3) %>%
    
    mutate(
           current_zeke = ifelse(outcome_label == "half_zeke_time_3", deaths_avg_per_100k*7>(.5),
                                 deaths_avg_per_100k*7>1),
           current_zeke = ifelse(outcome_label == "two_zeke_time_3", deaths_avg_per_100k*7>2,
                                 current_zeke),
           current_zeke = as.numeric(current_zeke)) %>%
    gather(var, value, indicator, cdc_flag, current_zeke) %>%

    mutate(var = ifelse(var == "indicator", type, var),
           outcome_label = factor(outcome_label, levels = c("zeke_time_3", "two_zeke_time_3")),
           outcome_value = as.numeric(outcome_value),
           current_zeke = ifelse(outcome_label == "half_zeke_time_3", deaths_avg_per_100k*7>(.5),
                                 deaths_avg_per_100k*7>1),
           current_zeke = ifelse(outcome_label == "two_zeke_time_3", deaths_avg_per_100k*7>2,
                                 current_zeke),
           current_zeke = as.numeric(current_zeke))
  
  z = unique(d_test_ind$type)
  d_test_ind = d_test_ind[(d_test_ind$var=="cdc_flag" & d_test_ind$type==z[1]) | (d_test_ind$var=="current_zeke" & d_test_ind$type==z) | (!d_test_ind$var%in%c("cdc_flag", "current_zeke")),]
  return(d_test_ind)
}

# test version
v1 = define_metrics(d_out_pre_state, admit_levels, case_levels, perc_levels) %>%
  mutate(var = as.character(var)) %>%
  arrange(state, date, admit_levels, case_levels, perc_levels, type, var, outcome_label) %>%
  mutate(var = as.character(var)) 

# operational version
v2 = define_metrics_DT(d_out_pre_state, admit_levels, case_levels, perc_levels, outcomes = c("zeke_time_3", "two_zeke_time_3"))%>%
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

#### Compare data frames ####

# load data

  # state
  load(here("0_Data", "Cleaned", "state_time_data.RData"))
  
  
  # HSA
  load(here("0_Data", "Cleaned", "hsa_time_data_chk.RData"))
  
  # summarize by location
  # state
  g_state = d_out_pre_state %>% group_by(state, ymd) %>%
    summarize(cases_avg = sum(cases_avg),
              admits_confirmed_avg = sum(admits_confirmed_avg, na.rm = T),
              icu_confirmed_avg = sum(icu_confirmed_avg, na.rm = T))

  # hsa
  g_hsa = d_out_pre_hsa %>% group_by(state_old, ymd) %>%
    summarize(cases_avg = sum(cases_avg),
              admits_confirmed_avg = sum(admits_confirmed_avg, na.rm = T),
              icu_confirmed_avg = sum(icu_confirmed_avg, na.rm = T))
  
  # quite a few cross over states
  # so there will be some duplicates
  # meaning admits, ICU slightly greater than expected
  # note that multipliers are not sensitive to slight changes in imputation parameters
  d_out_pre_hsa %>% group_by(state) %>% summarize(num = length(unique(state_old))) %>%
    group_by(num) %>% summarize(n())
  View(d_out_pre_hsa %>% group_by(state) %>% summarize(unique(state_old)))
  
  # merge
  temp = g_state %>% left_join(g_hsa, c("state" = "state_old", "ymd" = "ymd")) 
  
  # check percs
  View(temp)
  View(temp %>% group_by(state) %>%
         summarize(
           sum(cases_avg.y, na.rm = T)/sum(cases_avg.x, na.rm = T),
           sum(admits_confirmed_avg.y, na.rm = T)/sum(admits_confirmed_avg.x, na.rm = T),
           sum(icu_confirmed_avg.y, na.rm = T)/sum(icu_confirmed_avg.x, na.rm = T)))
  View(temp %>% group_by(ymd) %>%
         summarize(
           sum(cases_avg.y, na.rm = T)/sum(cases_avg.x, na.rm = T),
           sum(admits_confirmed_avg.y, na.rm = T)/sum(admits_confirmed_avg.x, na.rm = T),
           sum(icu_confirmed_avg.y, na.rm = T)/sum(icu_confirmed_avg.x, na.rm = T)))
  
#### ONE LAST GLOBAL TEST ####
  
  # pull for a specific date
  use_date = end_dates[20]
  chk_data = v3 %>% filter(outcome_label=="zeke_time_3" & ymd >= "2021-10-06" & ymd <= "2021-10-27") %>% filter(var=="cdc_flag")
  chk_data_out = v3 %>% filter(outcome_label=="zeke_time_3" & ymd == "2021-11-17") %>% filter(var=="cdc_flag")

  # run regression
  lm = glm(as.numeric(outcome_value)~admits_weekly+current_zeke, data = chk_data, weight = weight, family = "binomial")
  
  # generate predictions
  chk_data_out$preds = predict(lm, newdata = chk_data_out, type = "response")  

  # load comparison
  load(here("2_Figures", "Data", "Raw", "state_data_outcome_value ~ admits_weekly + current_zeke.RData"))
  preds2 = z %>% filter(outcome_label=="zeke_time_3" & ymd == "2021-11-17")
  
  # compare
  cbind(chk_data_out$preds, preds2$pred)
  
  # now repeat for a quarter
  end_dates_chk = end_dates[end_dates>="2022-04-01" & end_dates<"2022-07-01"]
  outcome_use = "zeke_time_3"
  temp_df = v3 %>% filter(outcome_label==outcome_use & ymd%in%end_dates_chk) %>% filter(var=="cdc_flag") %>%
    mutate(preds = NA)
  
  for(i in 1:length(end_dates_chk)){
    use_date = end_dates_chk[i]
    chk_data = v3 %>% filter(outcome_label==outcome_use & ymd >= use_date - 7*6 & ymd <= use_date - 7*3) %>% filter(var=="cdc_flag")
    chk_data_out = v3 %>% filter(outcome_label==outcome_use & ymd == use_date) %>% filter(var=="cdc_flag")
    
    # run regression
    lm = glm(as.numeric(outcome_value)~admits_weekly+current_zeke, data = chk_data, weight = weight, family = "binomial")
    
    # generate predictions
    temp_df$preds[temp_df$ymd==use_date] = predict(lm, newdata = chk_data_out, type = "response")  
  }
  
  # then compare to aggregate in Figure S6
  temp_df$base = temp_df$preds > .5
  weighted.mean(temp_df$base==temp_df$outcome_value, w = temp_df$weight)
  sum(temp_df$weight*(temp_df$base==temp_df$outcome_value))/sum(temp_df$weight)
  
  comp = z %>% filter(outcome_label==outcome_use & ymd%in%end_dates_chk)
  weighted.mean((comp$pred>.5)==comp$outcome_value, w = comp$weight)
  
  # lower (MULT = 1) BETTER SAFE THAN SORRY
  temp_df$lower = temp_df$preds > 1/3
  1 - ((1-1/3)*weighted.mean(temp_df$lower==1 & temp_df$outcome_value!=1, w = temp_df$weight) + 
         (1+1/3)*weighted.mean(temp_df$lower!=1 & temp_df$outcome_value==1, w = temp_df$weight) )/ 
    (weighted.mean(temp_df$outcome_value!=1, w = temp_df$weight) + 
       weighted.mean(temp_df$outcome_value==1, w = temp_df$weight))

  # upper (MULT = .5)
  temp_df$upper = temp_df$preds > 2/3
  1 - ((1+1/3)*weighted.mean(temp_df$upper==1 & temp_df$outcome_value!=1, w = temp_df$weight) + 
         (1-1/3)*weighted.mean(temp_df$upper!=1 & temp_df$outcome_value==1, w = temp_df$weight) )/ 
    (weighted.mean(temp_df$outcome_value!=1, w = temp_df$weight) + 
       weighted.mean(temp_df$outcome_value==1, w = temp_df$weight))
  
  