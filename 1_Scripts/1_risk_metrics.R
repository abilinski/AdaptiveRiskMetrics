#******************************** ESTIMATE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/1_risk_metrics.R")
source("global_options.R")

#### DEFINE METRICS ####
define_metrics_DT = function(d_out_pre, admit_levels, case_levels, perc_levels,
                             outcomes = c("half_zeke_time_3", "zeke_time_3", "two_zeke_time_3")){
  
  # convert into data.table
  dt = as.data.table(d_out_pre)
  
  # add row numbers
  dt = dt[, nth := row.names(.SD)]
  
  # expand grid
  grid = CJ(nth = unlist(dt[,"nth"]), admit_levels, case_levels, perc_levels, sorted = F)
  
  # join to grid
  dt = dt[grid, on = .(nth)]
  dt = dt[,nth:=NULL]
  
  # add a label for each metric
  dt = dt[, type:=do.call(paste,.SD), .SDcols = c( "case_levels", "admit_levels", "perc_levels")]
  
  # estimate each indicator
  dt = dt[, indicator :=  cases_weekly >= case_levels & admits_weekly >= admit_levels &
            perc_covid_100 >= perc_levels]
  
  # melt over outcome variables
  dt = melt(dt, measure.vars = outcomes,
            variable.name = "outcome_label", value.name = "outcome_value")
  
  # label outcomes as factor
  dt = dt[, outcome_label := factor(outcome_label, outcomes)]

  # estimate current outcome level
  dt = dt[,current_zeke:=0][icu_weekly>2 & outcome_label=="icu_2_time_3", current_zeke:=1]
  dt = dt[deaths_avg_per_100k*7>1 & outcome_label=="zeke_time_3", current_zeke:=1]
  dt = dt[deaths_avg_per_100k*7>2 & outcome_label=="two_zeke_time_3", current_zeke:=1]

  # current values (sims)
  dt = dt[outcome_label=="cutoff1", current_zeke:=lag_cutoff1]
  dt = dt[outcome_label=="cutoff2", current_zeke:=lag_cutoff2]
  dt = dt[outcome_label=="cutoff3", current_zeke:=lag_cutoff3]
  dt = dt[outcome_label=="cutoff4", current_zeke:=lag_cutoff4]
  dt = dt[outcome_label=="cutoff5", current_zeke:=lag_cutoff5]
  dt = dt[outcome_label=="cutoff6", current_zeke:=lag_cutoff6]
  
  # reshape data frame
  dt = melt(dt, measure.vars = c("indicator", "cdc_flag", "current_zeke"),
            variable.name = "var", value.name = "value")
  
  # rename label to separate out CDC
  dt = dt[var == "indicator", var := type]
  
  # remove CDC & Zeke duplicates
  z = unlist(dt[,"type"][1])
  dt = dt[(var=="cdc_flag" & type==z) | (var=="current_zeke" & type==z) | (!var%in%c("cdc_flag", "current zeke"))]
  
  # estimate current outcome level
  dt = dt[,current_zeke:=0][icu_weekly>2 & outcome_label=="icu_2_time_3", current_zeke:=1]
  dt = dt[deaths_avg_per_100k*7>1 & outcome_label=="zeke_time_3", current_zeke:=1]
  dt = dt[deaths_avg_per_100k*7>2 & outcome_label=="two_zeke_time_3", current_zeke:=1]
  
  # current values (sims)
  dt = dt[outcome_label=="cutoff1", current_zeke:=lag_cutoff1]
  dt = dt[outcome_label=="cutoff2", current_zeke:=lag_cutoff2]
  dt = dt[outcome_label=="cutoff3", current_zeke:=lag_cutoff3]
  dt = dt[outcome_label=="cutoff4", current_zeke:=lag_cutoff4]
  dt = dt[outcome_label=="cutoff5", current_zeke:=lag_cutoff5]
  dt = dt[outcome_label=="cutoff6", current_zeke:=lag_cutoff6]

  # make outcome values numeric
  dt = dt[, outcome_value:=as.numeric(outcome_value)]
  
  return(dt)
}

#### CALCULATE PERFORMANCE ####
# calculate sensitivity and specificity, PPV & NPV
calc_metrics_DT = function(dt, 
                           group_vars = c("var", "case_levels", "admit_levels", "perc_levels", "outcome_label")){
  
  # calculate multiplier
  dt = dt[,a:=(mult-1)/(1+mult)]
  
  # calculate metrics
  dt = dt[, .(sens = sum(weight*(outcome_value & value), na.rm = T)/sum(weight*(outcome_value & !is.na(value)), na.rm = T),
              spec = sum(weight*(!outcome_value & !value), na.rm = T)/sum(weight*(!outcome_value & !is.na(value)), na.rm = T),
              ppv = sum(weight*(outcome_value & value), na.rm = T)/sum(weight*(value & !is.na(outcome_value)), na.rm = T),
              npv = sum(weight*(!outcome_value & !value), na.rm = T)/sum(weight*(!value & !is.na(outcome_value)), na.rm = T),
              percright = sum(weight*(value==outcome_value), na.rm = T)/sum(weight*(!is.na(value) & !is.na(outcome_value)), na.rm = T),
              n = length(weight),
              n2 = sum(weight),
              fn = sum(weight*(1+a)*(outcome_value & !value), na.rm = T),
              fp = sum(weight*(1-a)*(!outcome_value & value), na.rm = T),
              tn = sum(weight*(!outcome_value & !value), na.rm = T),
              tp = sum(weight*(outcome_value & value), na.rm = T),
              pos = sum(weight*(value), na.rm = T),
              neg = sum(weight*(!value), na.rm = T)), by = group_vars]
  
  # combine into weighted accuracy
  dt = dt[, w_acc:=1-(fn + fp)/(neg+pos)]

}

#### BEST MEASURE WITH REGRESSIONS
best_measure_DT_reg = function(d_test_ind, end_dates, method = "glm", return_mods = F, w = 6, 
                               form = outcome_value ~ admits_weekly, quarter = F, mtry = 1){
  
  # loop over outcome dates
  out = foreach(i=1:length(end_dates), .combine = rbind) %dopar% {
    
    # run over recent past period
    d = 21
    
    # if updating weekly
    if(!quarter){
      
      # pull training data
      d_train = d_test_ind[ymd > (end_dates[i]-d-w*7) & ymd <= (end_dates[i]-d)]
      
      # pull test data
      d_test = d_test_ind[ymd == end_dates[i]]
  }else{ # if updating quarterly
    
    # make quarter variable
    d_test_ind[,"qy":=paste(quarter(ymd), year(ymd))]
    
    # pull quarter for training
    quarter_train = paste(quarter(end_dates[i] %m-% months(3)), year(end_dates[i]%m-% months(3)))
    d_train = d_test_ind[qy==quarter_train & ymd <= (end_dates[i]-d),]
    
    # pull test data
    d_test = d_test_ind[ymd == end_dates[i]]
  }
    
    # make a list of outcomes
    outcomes = unlist(unique(d_test_ind[,"outcome_label"]))
    
    # length of outcomes
    len = length(outcomes)
    
    # loop over outcomes
    for(j in 1:len){
    
    # check that there are test data  
    if(nrow(d_test[outcome_label==outcomes[j]])>0){

      # glm 
      if(method=="glm"){
        
      # fit model
      lm = glm(form, 
               data = d_train[outcome_label==outcomes[j]], 
               family = binomial(), 
               weights = weight)
      
      # make predictions
      pred_temp = predict(lm, newdata = d_test[outcome_label==outcomes[j]], type = "response")
      }
      
      # random forest
      # not used in paper
      if(method=="rf"){
        chk = mean(as.numeric(unlist(d_train[outcome_label==outcomes[j],"outcome_value"])))
        if(chk%in%c(0,1)){
          pred_temp = rep(chk, nrow(d_test[outcome_label==outcomes[j],]))
        }else{
        d_train = d_train[,outcome_value:=factor(outcome_value)]
        control <- trainControl(method='repeatedcv', 
                                number=5, 
                                repeats=1)
        set.seed(123)
        tunegrid <- expand.grid(.mtry=c(1:mtry))
        
        rf <- train(form, 
                            data=d_train[outcome_label==outcomes[j]], 
                            method='rf', weights = weight, maxdepth = 3,
                            tuneGrid=tunegrid, 
                            trControl=control)
        
        
        pred_temp = predict(rf, newdata = d_test[outcome_label==outcomes[j]], type = "prob")[,2]
        }
      }
      
      # store prediction
      d_test[outcome_label==outcomes[j], "pred" := .(pred_temp)]
      
      # store other outputs
      # value is the predicted binary based on risk preferences
      d_test[, c("value", "var", "w", "min_train_date", "max_train_date", "test_date", "weeks") := 
               .(pred >= (1/(1+mult)), "Smoothed", w, 
                 as.Date(min(unlist(d_train[,"ymd"]))),  as.Date(max(unlist(d_train[,"ymd"]))), 
                 as.Date(max(unlist(d_test[,"ymd"]))), 
             nrow(unique(d_test[,"ymd"])))]
      
    }
    
  }
    return(d_test)
    
  }
}

#### SUMMARY METRICS FOR BEST INDICATORS ####
summ_metrics = function(z1, start_date_val = start_date, end_pre_cut = end_pre, by_start = F){
  
  # filter on start date
  z1 = z1[ymd>=start_date_val,]
  
  # generate grouping variables
  gvars = c("var", "outcome_label", "quarter")
  if(by_start) gvars =  c("var", "outcome_label", "quarter", "diff")
  
  # calculate overall metrics
  metrics = calc_metrics_DT(z1[,quarter:="Overall"], group_vars = gvars)
  
  # calculate metrics by period
  metrics_date = calc_metrics_DT(z1[,quarter:=c("Training", "Test")[(ymd>end_pre_cut)+1]], 
                                 group_vars = gvars)

  return(rbindlist(list(metrics, metrics_date)))
  
}

#### COLLAPSE OVER SPECIFIED TIME PERIOD ####
summ_metrics2 = function(d_test_ind, gvars = c("quarter", "var", "outcome_label")){
  
  # pull parts of the year
  d_test_ind = d_test_ind[, c("month", "year", "day"):=
                            .(month(ymd), year(ymd), 1)][ymd>=start_date & ymd<=end_date]
  
  # pull quarter
  d_test_ind = d_test_ind[, c("quarter"):=
                            .(quarter(ymd))]
  
  # set up to run by month (with sart date as the first)
  d_test_ind = d_test_ind[, type:=do.call(paste,.SD), .SDcols = c("year", "month", "day")]
  d_test_ind = d_test_ind[, month:=as.Date(type, format = "%Y %m %d")]
  
  # set up to run by quarter
  d_test_ind = d_test_ind[, quarter:=do.call(paste,.SD), .SDcols = c("year", "quarter")]
  
  out = calc_metrics_DT(d_test_ind, group_vars = gvars)
  
}

##### RUN SPECIFICATIONS ####
run_specs = function(d_test_ind, w_ind, end_dates, form, var_lab, quarter = F,
                     by_start_val, gvars = c("quarter", "var", "outcome_label"),
                     end_pre_cut, method = "glm", mtry = 1){
  
  # run regressions
  # filter data to only have on indicator
  z = best_measure_DT_reg(d_test_ind[var=="cdc_flag",], w = w_ind, end_dates = end_dates,
                          quarter = quarter, form = form, method = method)[,var:=var_lab]
  
  # summarize overall
  out = summ_metrics(z, by_start = by_start_val, end_pre_cut = end_pre_cut)
  
  # summarize by month
  out.1 = summ_metrics2(z, gvars)

  return(rbindlist(list(out, out.1), use.names = T, fill = T))
  
}

#### SUMMARY STATISTICS ####
run_summary_stats = function(d_test_ind, by_start_val = F, end_pre_cut = end_pre, 
                             end_dates, gvars = c("quarter", "var", "outcome_label"),
                             run_all = F, w_ind = 4, method = "glm"){
  
  # cdc metrics
  out_cdc = summ_metrics(d_test_ind[ymd <= end_date], 
                         by_start = by_start_val,
                         end_pre_cut = end_pre_cut)
  out_cdc.1 = summ_metrics2(d_test_ind, gvars)
  
  # put in a table
  tbl = rbindlist(list(out_cdc.1, out_cdc), fill = T)
  
  ## adaptive predictions
  # one-week
  if(run_all){
    
    # labels
    var_lab = c("Adaptive", "Adaptive: H",
                "Adaptive: C", "Adaptive: CZ", "Adaptive: CH", "Adaptive: CHZ",
                "Adaptive: HO", "Adaptive: HOZ", 
                "Adaptive: CHO", "Adaptive: CHOZ", "Simplified adaptive")
    
    # make data frames
    specs = data.frame(var_lab,
      quarter = c(rep(F, length(var_lab)-1), T),
      form = 1:length(var_lab),
      mtry = c(2,1,1,2,2,3,2,3,3,4,2)
    )
    
    # functional forms
    forms = c(outcome_value ~ admits_weekly+current_zeke,
              outcome_value ~ admits_weekly,
              outcome_value ~ cases_weekly,
              outcome_value ~ cases_weekly + current_zeke,
              outcome_value ~ cases_weekly + admits_weekly,
              outcome_value ~ cases_weekly + admits_weekly + current_zeke,
              outcome_value ~ perc_covid_100 + admits_weekly,
              outcome_value ~ perc_covid_100 + admits_weekly + current_zeke,
              outcome_value ~ cases_weekly + admits_weekly + perc_covid_100,
              outcome_value ~ cases_weekly + admits_weekly + perc_covid_100 + current_zeke,
              outcome_value ~ admits_weekly + current_zeke)
    
    # set up data table for storage
    outs = data.table()
    
    # run each specification
    for(i in 1:nrow(specs)){
    temp = run_specs(d_test_ind = d_test_ind, w_ind = w_ind, end_dates = end_dates, gvars = gvars,
                   form = forms[[specs$form[i]]], var_lab = specs$var_lab[i], method = method,
                   quarter = specs$quarter[i], by_start_val = by_start_val, end_pre_cut = end_pre_cut, mtry = specs$mtry[1])
    outs = rbindlist(list(outs, temp), fill = TRUE)
    
    }

    tbl = rbindlist(list(tbl, outs), fill = TRUE)
  }
  
  # put second bit in a table
  tbl2 = tbl %>% 
    gather(metric, metric_value, sens, spec, ppv, npv, percright, w_acc) %>%
    mutate(
      
      # rename metrics
      metric = case_when(metric=="percright"~"Accuracy",
                         metric=="sens"~"Sensitivity",
                         metric=="spec"~"Specificity",
                         metric=="ppv"~"PPV",
                         metric=="npv"~"NPV",
                         metric=="w_acc"~"Weighted accuracy"),
      metric = factor(metric, levels = c("Accuracy", "Weighted accuracy", "PPV", "NPV", "Sensitivity", "Specificity"))) 
  
  # make plot
  return(list(tbl, tbl2))
}

#### RUN ESTIMATES ####
run_base_ests = function(d_out_pre, 
                         admit_levels = 0, case_levels = 0,
                         perc_levels = 0, w = 4,
                         title = "States", run_all = T,
                         outcomes = c("zeke_time_3", "two_zeke_time_3", "icu_2_time_3"),
                         prev = F, method = "glm", gvars = c("quarter", "var", "outcome_label")){

  # set up different indicators
  d_test_ind = define_metrics_DT(d_out_pre,
                                 admit_levels = admit_levels, case_levels, perc_levels, outcomes = outcomes)[ymd<=end_date,]
  end_dates = sort(unique(d_out_pre$ymd[d_out_pre$ymd >= start_date & d_out_pre$ymd <= end_date]))

  # set preference sets differently for prevalence
  if(!prev){
    mult_high = 2
    mult_low = .5
  } else{
    mult_high = 1
    mult_low = 1
  }
  
  # run over different preference sets
  d_test_ind = d_test_ind[,mult:=1]
  tbl1_summ1 = run_summary_stats(d_test_ind, w_ind = w, gvars = gvars,
                                 end_dates = end_dates, run_all = run_all, method = method)
  
  d_test_ind = d_test_ind[,mult:=mult_high]
  tbl1_summ2= run_summary_stats(d_test_ind, end_dates = end_dates, run_all = run_all, method = method, gvars = gvars)

  d_test_ind = d_test_ind[,mult:=mult_low]
  tbl1_summ.5 = run_summary_stats(d_test_ind, end_dates = end_dates, run_all = run_all, method = method, gvars = gvars)

  d_test_ind = d_test_ind[,mult:=1]
  d_test_ind = d_test_ind[current_zeke==FALSE,mult:=5]
  tbl1_summ_sens = run_summary_stats(d_test_ind, end_dates = end_dates, run_all = run_all, method = method, gvars = gvars)
  
  # combine different values
  out = rbindlist(list(data.table(tbl1_summ1[[2]])[,lab := "Neutral"],
                       data.table(tbl1_summ2[[2]])[,lab:="Better safe than sorry (0.5x FP)"], 
                       data.table(tbl1_summ.5[[2]])[,lab:="Don't cry wolf (0.5x FN)"],
                       data.table(tbl1_summ_sens[[2]])[,lab:="Sensitivity analysis"]
                       ))
  
  # set labels
  out[,lab:=factor(lab, levels = c("Neutral", "Don't cry wolf (0.5x FN)", "Better safe than sorry (0.5x FP)", "Sensitivity analysis"))]
  out[var=="cdc_flag", var:="Community level"]
  out[var=="current_zeke", var:="Z"]
  out[outcome_label=="zeke_time_3", lab2:=">1 death/100K/wk"]
  out[outcome_label=="two_zeke_time_3", lab2:=">2 deaths/100K/wk"]
  out[outcome_label=="half_zeke_time_3", lab2:=">0.5 deaths/100K/wk"]
  out[outcome_label=="icu_2_time_3", lab2:=">2 ICU patients/100K/wk"]
  out[outcome_label=="pred_rev", lab2:="Synthetic data"]
  
  return(out)
}

### SIMULATIONS ####
run_base_ests_sims = function(d_out_pre, 
                         admit_levels = 0, case_levels = 0,
                         perc_levels = 0, w = 4, 
                         run_all = T, gvars = c("month", "var", "outcome_label"),
                         outcomes = c("zeke_time_3", "two_zeke_time_3", "icu_2_time_3"),
                         method = "glm"){
  
  # set up different indicators
  d_test_ind = define_metrics_DT(d_out_pre, 
                                 admit_levels = admit_levels, case_levels, perc_levels, outcomes = outcomes)[ymd<=end_date,]
  end_dates = sort(unique(d_out_pre$ymd[d_out_pre$ymd >= start_date & d_out_pre$ymd <= end_date]))

  # run over different preference sets
  d_test_ind = d_test_ind[,mult:=1]
  out = data.table(run_summary_stats(d_test_ind, w_ind = w, gvars = gvars,
                                 end_dates = end_dates, run_all = run_all, method = method)[[2]])

  # set labels
  out = out[var=="cdc_flag", var:="Community level"]
  out = out[, lab2:=outcome_label]
  
  # choose metric of interest
  met = "Weighted accuracy"
  
  return(out)
}

#### RUN ROC CURVE ####
run_roc = function(d_out_pre, 
                         admit_levels = 0, case_levels = 0,
                         perc_levels = 0, w = 4,
                         title = "States", run_all = T,
                         outcomes = c("zeke_time_3", "two_zeke_time_3", "icu_2_time_3"),
                         method = "glm", cuts){
  
  # set up different indicators
  d_test_ind = define_metrics_DT(d_out_pre, 
                                 admit_levels = admit_levels, case_levels, perc_levels, outcomes = outcomes)[ymd<=end_date,]
  end_dates = sort(unique(d_out_pre$ymd[d_out_pre$ymd >= start_date & d_out_pre$ymd <= end_date]))
  
  # store output
  out = data.table()
  for(i in cuts){
   # run over different preference sets
  d_test_ind[,mult:=i]
  #d_test_ind[current_zeke==FALSE,mult:=i]

  tbl1_summ = run_summary_stats(d_test_ind, w_ind = w,
                                 end_dates = end_dates, run_all = run_all, method = method)
  
  # combine data
  out = rbindlist(list(out, data.table(tbl1_summ[[2]])[,lab := i]))
  }

  
  # set labels
  out[var=="cdc_flag", var:="Community level"]
  out[outcome_label=="zeke_time_3", lab2:=">1 death/100K/wk"]
  out[outcome_label=="two_zeke_time_3", lab2:=">2 deaths/100K/wk"]
  out[outcome_label=="half_zeke_time_3", lab2:=">0.5 deaths/100K/wk"]
  out[outcome_label=="icu_2_time_3", lab2:=">2 ICU patients/100K/wk"]
  out[outcome_label=="pred_rev", lab2:="Synthetic data"]
  
  return(list(out))
}




