#******************************** ESTIMATE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/2_main_analyses.R")
source("global_options.R")
source(here("1_Scripts", "1_risk_metrics.R"))

# load data

  # state
  load(here("0_Data", "Cleaned", "state_time_data.RData"))
  
  # county
  load(here("0_Data", "Cleaned", "county_time_data.RData"))

  # HSA
  load(here("0_Data", "Cleaned", "hsa_time_data.RData"))

# set up dates
for(type in c("base", "omicron")){
  
end_date = "2022-10-01"

if(type=="base"){
  start_date = "2021-04-01"
  end_pre = "2021-12-31"
  location = "Data/Base"
}else if(type=="omicron"){
  start_date = "2021-12-15"
  end_pre = "2022-02-15"
  location = "Data/Omicron"
}

# admission levels
admit_levels = c(5, 10, 15, 20, 25)
case_levels = c(50, 100, 150, 200, 250, 300)
perc_levels = c(5, 10, 15, 20)

run_analyses = function(type_val = "state", run = 1:13, save = F, specs = FALSE){
  if(type_val=="state"){
    d_out_pre = d_out_pre_state
  }else if(type_val=="county"){
    d_out_pre = d_out_pre_cty
  }else{d_out_pre = d_out_pre_hsa}
  
  # make names
  df_names = c("w8", "w12", "C", "CH", "CHO1", "CHO2", "CHO3", "HO", "0", "uw", "roc", "CHO4")
  dfs = c(type_val, paste(type_val, df_names, sep = "_"))
  
  # whether to rerun indicators
  # run states with H + adaptive (w/different week lags)
  if(1 %in% run) assign(dfs[1], run_base_ests(d_out_pre, admit_levels = admit_levels, run_all = T, run_all_specs = specs, save = save, type = type_val)); gc()
  if(2 %in% run) assign(dfs[2], run_base_ests(d_out_pre, admit_levels = admit_levels, w = 8)); gc()
  if(3 %in% run) assign(dfs[3], run_base_ests(d_out_pre, admit_levels = admit_levels, w = 12)); gc()
  
  # run states with other indicator combos 
  if(4 %in% run) assign(dfs[4], run_base_ests(d_out_pre, case_levels = case_levels, run_all = F)); gc()
  if(5 %in% run) assign(dfs[5], run_base_ests(d_out_pre, admit_levels = admit_levels, case_levels = case_levels, run_all = F)); gc()
  if(6 %in% run) assign(dfs[6], run_base_ests(d_out_pre, admit_levels = admit_levels, case_levels = case_levels, perc_levels = perc_levels, run_all = F, outcome = "zeke_time_3")); gc()
  if(7 %in% run) assign(dfs[7], run_base_ests(d_out_pre, admit_levels = admit_levels, case_levels = case_levels, perc_levels = perc_levels, run_all = F, outcome = "two_zeke_time_3")); gc()
  if(8 %in% run) assign(dfs[8], run_base_ests(d_out_pre, admit_levels = admit_levels, case_levels = case_levels, perc_levels = perc_levels, run_all = F, outcome = c("icu_2_time_3"))); gc()
  if(13 %in% run) assign(dfs[13], run_base_ests(d_out_pre, admit_levels = admit_levels, case_levels = case_levels, perc_levels = perc_levels, run_all = F, outcome = c("perc_covid_10_time_3"))); gc()
  if(9 %in% run)assign(dfs[9], run_base_ests(d_out_pre, admit_levels = admit_levels, perc_levels = perc_levels, run_all = F)); gc()

  # prevalence
  if(10 %in% run) assign(dfs[10], run_base_ests(d_out_pre, admit_levels = 0, case_levels = 0, perc_levels = 0, run_all = F, prev = T)); gc()
  
  # unweighted
  if(11 %in% run) assign(dfs[11], run_base_ests(d_out_pre %>% mutate(weight = weight_alt), admit_levels = admit_levels, run_all = T)); gc()
  
  # ROC
  cuts = c(1/100,1/20,1/15,1/10,1/7,1/5,1/3,1/2,1,2,3,5,7,10,15,20,100)
  if(12 %in% run) assign(dfs[12], run_roc(d_out_pre, admit_levels = admit_levels, run_all = T, cuts = cuts)); gc()
  
  save(list = dfs[run], file = here("2_Figures", location, paste(type_val, "_data_", min(run), "_", max(run), ".RData", sep = "")))
}

# set up saving
save_var = type=="base"

# run states
run_analyses(type_val = "state", save = save_var, specs = type=="base")

# run hsa
run_analyses(type_val = "hsa", save = save_var, specs = type=="base")

# run counties
run_analyses(type_val = "county", 1:4, save = save_var)
run_analyses(type_val = "county", 5)
run_analyses(type_val = "county", 6)
run_analyses(type_val = "county", 7)
run_analyses(type_val = "county", 8)
run_analyses(type_val = "county", 13)
run_analyses(type_val = "county", 9)
run_analyses(type_val = "county", 10:12)
}

