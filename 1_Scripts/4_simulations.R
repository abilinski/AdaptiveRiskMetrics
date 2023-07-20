#******************************** ESTIMATE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/4_simulations.R")
source("global_options.R")
source(here("1_Scripts", "1_risk_metrics.R"))

#### LOAD DATA ####
load(here("0_Data", "Cleaned", "state_time_data.RData"))
admit_levels = c(0, 5, 10, 15, 20, 25, 30)

# dates
start_date = "2021-07-01"
end_pre = "2021-10-01"
end_date = "2022-10-01"

# actual prevalence
d1 = d_out_pre_state %>% filter(ymd>="2021-04-01" & ymd <=end_date) %>%
  mutate(weight = weight_alt, quarter = paste(year, quarter(ymd)))

# constant prevalence
d2 = d_out_pre_state %>% filter(ymd>="2021-04-01" & ymd <=end_date) %>%
  ungroup() %>%
  mutate(weight = weight_alt,
         n = n(),
         admits_weekly = runif(n = n(), min = 2, max = 20))

# normal based
d3 = d_out_pre_state %>% filter(ymd>="2021-04-01" & ymd <=end_date) %>%
  mutate(weight = weight_alt, quarter = paste(year, quarter(ymd))) %>%
  ungroup() %>%
  mutate(mu = ifelse(quarter%in%c("2021 2", "2021 4", "2022 2"), 5, 15),
         admits_weekly = rnorm(n = n(), mean = mu, sd = 1))
  

#### CUTOFFS ####
param_dates = data.frame(ymd = sort(unique(d1$ymd))) %>%
  mutate(n = n(),
         r = row_number(),
         cutoff1 = 5,
         cutoff2 = 10,
         cutoff3 = seq(5, 15, length.out = n()),
         cutoff4 = 5 + 10/(1+exp(-.3*(r-40))),
         cutoff5 = (20-.01*(r-25)^2 + 10)/2,
         cutoff6 = 5 + 10/(1+exp(-.3*(r-25))) -  5/(1+exp(-.4*(r-45)))) 

# make plot
p1 = ggplot(param_dates %>%
              gather(cutoff, cutoff_val, cutoff1, cutoff2, cutoff3, cutoff4, cutoff5, cutoff6) %>%
              mutate(cutoff_lab = case_when(cutoff=="cutoff2"~"Constant",
                                            cutoff=="cutoff3"~"Linear",
                                            cutoff=="cutoff4"~"Logistic",
                                            cutoff=="cutoff6"~"Non-monotonic")) %>%
              filter(!is.na(cutoff_lab)), 
            aes(x = ymd, y = cutoff_val, group = cutoff)) + geom_line() + 
  facet_wrap(.~cutoff_lab, ncol = 6) + ylim(0, 15) +  theme_bw() +
  theme(strip.placement = "outside") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank()) + labs(x = "", y = "Optimal cutoff") + 
  scale_x_date(date_labels = "%b %y")

# save plot
ggsave(p1, filename = here("3_Figures", "sims_setup.png"), width = 8, height = 5)

#### SIMULATION FUNCTION ####
run_sims = function(d1, param_dates, scale_val = 0, gvars = c("month", "var", "outcome_label")){
  temp_df = d1 %>% left_join(param_dates, c("ymd" = "ymd")) %>%
    gather(cutoff, cutoff_val, cutoff1, cutoff2, cutoff3, cutoff4, cutoff5, cutoff6) %>%
    group_by(state) %>% arrange(ymd) %>%
    mutate(n = n(), 
           fake_data = 1/(1+exp(-1*(3*admits_weekly-3*cutoff_val))),
           fake_outcome = rbinom(n, size = 1, prob = fake_data)) 
  # Check that this is working right
  #temp_df %>% mutate(prob_cat = cut(fake_data, breaks = 10, include_lowest = T)) %>%
  #  group_by(prob_cat) %>% summarize(mean(fake_outcome))
  
  #ggplot(temp_df %>% filter(admits_weekly <= 20), aes(x = admits_weekly, y = fake_outcome)) + geom_smooth(se = F) + facet_grid(.~cutoff)
  #mean(temp_df$admits_weekly>100)
  
  #temp_df %>% group_by(cutoff) %>% summarize(min(fake_outcome), max(fake_outcome), mean(is.na(fake_outcome)))
  
  temp_df2 = temp_df %>%
    dplyr::select(-cutoff_val, -fake_data) %>%
    spread(cutoff, fake_outcome) %>%
    group_by(state) %>% arrange(ymd) %>%
    mutate(lag_cutoff1 = lag(cutoff1, 3),
           lag_cutoff2 = lag(cutoff2, 3),
           lag_cutoff3 = lag(cutoff3, 3),
           lag_cutoff4 = lag(cutoff4, 3),
           lag_cutoff5 = lag(cutoff5, 3),
           lag_cutoff6 = lag(cutoff6, 3))
  
  # run models
  state = run_base_ests_sims(temp_df2, admit_levels = admit_levels, run_all = T, gvars = gvars,
                        outcomes = c("cutoff1", "cutoff2", "cutoff3", "cutoff4", "cutoff5", "cutoff6")); gc()
  
  # rename output
  state = state[, lab2:=outcome_label]
  state = state[, lab:="Neutral"]
  state = state[!outcome_label%in%c("cutoff1", "cutoff5") & !var%in%c("Community level", "Adaptive: CHOZ", "Adaptive: CHO")]
  state = state[var=="0 0 0", var:="Prevalence"]
  
  return(state)
}

#### TABLE FUNCTION ####
make_table = function(lists = list(county[[1]][,type:="H"]), filter_best = F, value_weight = "Neutral", met = "Weighted accuracy",
                      label = "Counties", vars = c("current_zeke", "Community level", "Adaptive: CHOZ", "Adaptive: CHO","Adaptive", "Simplified adaptive", "Prevalence", "Z"),
                      order = c("current_zeke", "Adaptive: CHOZ", "Adaptive: CHO", "Adaptive: H",  "Adaptive: HOZ", "Adaptive: HZ", "Simplified adaptive: HZ", "Community level")){
  
  # calculate states
  states_plot = rbindlist(lists, fill = T) %>% filter(metric==met) %>% 
    mutate(var = ifelse(var=="current_zeke", "Z", as.character(var))) %>%
    filter(!var%in%c("Community level", "Z") | type=="H") %>%
    dplyr::select(lab, lab2, var,type, quarter, metric_value, sim) %>% 
    group_by(lab, type, lab2, var, sim) %>% 
    mutate(ind = grepl("0", var), filter_best) %>%
    spread(quarter, metric_value) %>%
    group_by(lab, lab2, type, ind, sim) %>% 
    mutate(max = max(Training), best = Training == max(Training) & ind,
           var = sub(" 0 0 0", "", var),
           var = ifelse(ind, type, var),
           var = factor(var)) %>% 
    ungroup() %>%
    filter(filter_best | (best | var%in%vars)) %>%
    mutate(var = ifelse(var%in%c("Adaptive", "Simplified adaptive"), paste(var, "HZ", sep = ": "), as.character(var))) %>%
    dplyr::select(-ind, -max, - best, -type, -filter_best) %>%
    mutate_if(is.numeric, function(a) round(a*100)) %>%
    arrange(lab, lab2, -Test) %>% gather(var2, value2, -1:-4) %>%
    group_by(lab, lab2, sim) %>% mutate(value2 = ifelse(value2 < 0, 0, value2),
                                        fill = scale(value2),
                                        cutoff_lab = case_when(lab2=="cutoff2"~"Constant",
                                                               lab2=="cutoff3"~"Linear",
                                                               lab2=="cutoff4"~"Logistic",
                                                               lab2=="cutoff6"~"Non-monotonic"),
                                        sim = factor(sim, levels = c("Empirical", "Constant", "Sharp waves")))
  
  # set factor levels
  states_plot$var = factor(states_plot$var, levels = rev(c(order, "CHO", "HO", "CH", "O", "H", "C", "Z", "Prevalence")))  
  states_plot$var2 = sub("20", "", states_plot$var2)
  states_plot$var2 = sub(" ", "-", states_plot$var2)
  states_plot$var2 = factor(states_plot$var2, levels = sort(unique(states_plot$var2))[c(1:6,8,9,7)])
  
  # run plot
  plot = ggplot(states_plot %>% filter(lab %in% value_weight), 
                aes(x = var2, y = var,
                    fill = ifelse(var2%in%c("21-2", "21-3", "Overall", "Training") | var == "Prevalence", NA, value2))) + 
    geom_tile() + 
    geom_text(aes(label = value2,
                  col = ifelse(var2%in%c("21-2", "21-3", "Overall", "Training") | var == "Prevalence", "black", "white"))) + 
    facet_grid(sim~cutoff_lab, scales = "free_y") + 
    scale_x_discrete(position = "top") + 
    scale_fill_gradient(name = "", na.value = "lightgrey", guide = "none", high = "#01579b", low = "#BA0001") + 
    scale_color_manual(guide = "none", values = c("black", "white")) + 
    labs(x = "", y = "") + 
    theme_bw() +
    theme(strip.placement = "outside") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank())  
  
  return(list(plot, states_plot))
}

#### RUN SIMULATIONS ####
tic()

# store data
s0 = data.table()
s1 = data.table()
s2 = data.table()

# set seed
set.seed(02138)

# run 50 sims
for(i in 1:50){
  
# empirical
temp = run_sims(d1 = d1, param_dates = param_dates, scale_val = 1,
              gvars = c("quarter", "var", "outcome_label")) %>% 
  mutate(sim = "Empirical", val = i, rn = row_number())

s0 = rbindlist(list(s0, temp))

# constant
d2 = d_out_pre_state %>% filter(ymd>="2021-04-01" & ymd <=end_date) %>%
  ungroup() %>%
  mutate(weight = weight_alt,
         n = n(),
         admits_weekly = runif(n = n(), min = 2, max = 20))

temp2 = run_sims(d1 = d2, param_dates = param_dates, scale_val = 1,
              gvars = c("quarter", "var", "outcome_label")) %>% 
  mutate(sim = "Constant",  val = i, rn = row_number())

s1 = rbindlist(list(s1, temp2))

# sharp saves
d3 = d_out_pre_state %>% filter(ymd>="2021-04-01" & ymd <=end_date) %>%
  mutate(weight = weight_alt, quarter = paste(year, quarter(ymd))) %>%
  ungroup() %>%
  mutate(mu = ifelse(quarter%in%c("2021 2", "2021 4", "2022 2"), 5, 15),
         admits_weekly = rnorm(n = n(), mean = mu, sd = 1))

temp3 = run_sims(d1 = d3, param_dates = param_dates, scale_val = 1,
              gvars = c("quarter", "var", "outcome_label")) %>% 
  mutate(sim = "Sharp waves",  val = i, rn = row_number()) %>% filter(var!="0 5 0")

s2 = rbindlist(list(s2, temp3))


}
toc()

# average over sims
s0 = s0 %>% group_by(quarter, var, outcome_label, metric, lab, lab2, sim, rn) %>% summarize(metric_value = mean(metric_value)) 
s1 = s1 %>% group_by(quarter, var, outcome_label, metric, lab, lab2, sim, rn) %>% summarize(metric_value = mean(metric_value)) 
s2 = s2 %>% group_by(quarter, var, outcome_label, metric, lab, lab2, sim, rn) %>% summarize(metric_value = mean(metric_value)) 

# make plot
sim_out = make_table(lists = list(data.table(s0)[,type:="H"],
                                  data.table(s1)[,type:="H"],
                                  data.table(s2)[,type:="H"]))

# save plot
ggsave(sim_out[[1]], width = 15, height = 8, filename = here("3_Figures", "sims_results.png"))                            

