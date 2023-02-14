#******************************** SUMMARIZE BASIC METRICS **********************************#
#### SETUP ####
here::i_am("1_Scripts/3_figures.R")
source("global_options.R")

# main location
location = "Data/Base"

# load files
g = list.files(here("2_Figures",  location))
for(i in 1:length(g)) load(here("2_Figures",  location, g[i]))

# recode
state_0[var=="0 0 0", var:="Prevalence"]
county_0[var=="0 0 0", var:="Prevalence"]
hsa_0[var=="0 0 0", var:="Prevalence"]

#### TIME TREND FIGURES ####
make_line_plots = function(out, met = "Weighted accuracy", title = "States"){
  
  # filter data for plot
  plot_data = out %>% 
    mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
    filter(lab!="Sensitivity analysis") %>%
    filter(lab=="Neutral") %>%
    #filter(lab2!=">2 ICU patients/100K/wk") %>%
    filter(!quarter%in%c("Training", "Test", "Overall")) %>%
    filter(!var%in%c("0 15 0", "0 25 0")) %>%
    filter(metric == met)
  
  p_data1 = plot_data %>%
    separate(var, into = c("C", "H", "O", "junk1", "junk2", "junk3"), sep = "\ ") %>%
    mutate(H = factor(H, levels = seq(25, 5, by = -5)),
           metric_value = ifelse(metric_value < 0, 0, metric_value)) %>%
    # remove other metrics
    filter(!is.na(H))
  
  # make plot
  plot = ggplot(p_data1,
                aes(x = quarter, y = metric_value, group = H)) + 
    facet_grid(.~lab2) + scale_linetype(name = "") +
    scale_color_manual(name = "", values = c("darkblue", pal[5])) +
    theme_bw() + 
    geom_line(col = "darkgrey", lwd = .3) + 
    geom_point(col = "darkgrey", size = .5) + 
    geom_text(data = p_data1 %>% filter(quarter == "2022 3"), col = "darkgrey", aes(x = 6.5, label = paste("H:", H))) + 
    ylim(0,1) + 
    geom_line(data = plot_data %>%
                filter(metric == met & var %in% c("Adaptive", "Community Levels")), 
              aes(x = quarter, y =metric_value, group = var, col = var)) +
    geom_point(data = plot_data %>%
                filter(metric == met & var %in% c("Adaptive", "Community Levels")), 
              aes(x = quarter, y =metric_value, group = var, col = var), size = .8) +
    labs(x = "", y = "", title = title) + theme(legend.position = "bottom", panel.grid = element_blank()) + 
    expand_limits(x = c(1,7))
  
}

# make state plot
p1 = make_line_plots(state)

# make county plot
p2 = make_line_plots(county, title = "Counties")

# make HSA plot
p3 = make_line_plots(hsa, title = "HSAs")

# arrange plots
a = ggarrange(p1, p2, ncol = 1, common.legend = T,
              legend = "bottom")

a.2 = ggarrange(p1, p3, p2, ncol = 1, common.legend = T,
              legend = "bottom")

# save plots
ggsave(a, filename = here("2_Figures", "figures_time.png"), width = 9, height = 7)
ggsave(a.2, filename = here("2_Figures", "figures_time_hsa.png"), width = 9, height = 10.5)

#### MAIN TABLE ####
make_table = function(lists = list(county[[1]][,type:="H"]), filter_best = F, value_weight = "Neutral", met = "Weighted accuracy",
                      label = "Counties", vars = c("current_zeke", "Community Levels", "Adaptive: CHOZ", "Adaptive: CHO","Adaptive", "Simplified adaptive", "Prevalence", "Z"),
                      order = c("current_zeke", "Adaptive: CHOZ", "Adaptive: CHO", "Adaptive: H",  "Adaptive: HOZ", "Adaptive: HZ", "Simplified adaptive: HZ", "Community Levels", "Z")){
  
  # define training period
  train = c("2021 2", "2021 3", "2021 4")
  test = c("2022 1", "2022 2", "2022 3")
  
  # calculate states
  states_plot = rbindlist(lists) %>% filter(metric==met) %>% 
    mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
    filter(outcome_label!="icu_2_time_3") %>%
    filter(!var%in%c("Community Levels", "Z") | type=="H") %>%
    dplyr::select(lab, lab2, var,type, quarter, metric_value) %>% 
    group_by(lab, type, lab2, var) %>% 
    mutate(ind = grepl("0", var), filter_best) %>%
    spread(quarter, metric_value) %>%
    group_by(lab, lab2, type, ind) %>% 
    mutate(max = max(Training), best = Training == max(Training) & ind)
  
  print(states_plot %>% filter(lab=="Neutral") %>% filter(best) %>% dplyr::select(var))
  
  states_plot = states_plot %>%
    mutate(
           var = sub(" 0 0 0", "", var),
           var = ifelse(ind, type, var),
           var = factor(var)) %>% 
    ungroup() %>%
    filter(filter_best | (best | var%in%vars)) %>%
    mutate(var = ifelse(var%in%c("Adaptive", "Simplified adaptive"), paste(var, "HZ", sep = ": "), as.character(var))) %>%
    dplyr::select(-ind, -max, - best, -type, -filter_best) %>%
    mutate_if(is.numeric, function(a) round(a*100)) %>%
    arrange(lab, lab2, -Test) %>% gather(var2, value2, -1:-3) %>%
    group_by(lab, lab2) %>% mutate(value2 = ifelse(value2 < 0, 0, value2),
                                   fill = scale(value2),
                                   var3 = ifelse(var2%in%train, "Training\n MR", var2),
                                   var3 = ifelse(var2%in%test, "Test\n MR", var3)) %>%
    group_by(var2, lab, lab2) %>% mutate(max = max(value2), regret = max-value2) %>%
    group_by(lab, lab2, var, var3) %>% mutate(max_regret = max(regret)) %>% filter(!grepl("MR", var3) | regret == max_regret) %>%
    mutate(value2 = ifelse(grepl("MR", var3), max_regret, value2)) %>%
    group_by(lab2, var3) %>% 
    mutate(
      fill_val = ifelse(grepl("MR", var3), scale(100-value2), scale(value2)),
      fill = ifelse(grepl("Test", var3), fill_val, NA),
      value2 = ifelse(grepl("prev", var) & grepl("MR", var3), NA, value2),
      fill = ifelse(grepl("prev", var), NA, fill))
  
  # set factor levels
  states_plot$var = factor(states_plot$var, levels = rev(c(order, "CHO", "HO", "CH", "O", "H", "C", "Prevalence")))  
  states_plot$var3 = factor(states_plot$var3, levels = sort(unique(states_plot$var3))[c(4,5,2,3)])
  
  # run plot
  plot = ggplot(states_plot %>% filter(lab %in% value_weight) %>%
                  filter(var3!="Overall"),
                aes(x = var3, y = var)) + 
    geom_tile(aes(fill = fill)) + 
    geom_text(aes(label = value2, col = ifelse(grepl("Test", var3) & !grepl("prev", var), "white", "black"))) + 
    scale_color_manual(guide = "none", values = c("black", "white")) + 
    facet_grid(lab2~lab, scales = "free_y") + 
    scale_x_discrete(position = "top") +
    scale_fill_gradient(name = "", na.value = "lightgrey", guide = "none", high = "#01579b", low = "#BA0001") + 
    labs(x = "", y = "", title = label) + 
    theme_bw() +
    theme(strip.placement = "outside") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank())  +
    annotate("rect", xmin=c(0.5), xmax=c(4.5), ymin=c(8.5), ymax=c(8.5), colour="black", fill="transparent", size=1)
  
  return(list(plot, states_plot))
}

# state table
states = make_table(lists = list(state[,type:="H"],
                                 state_C[,type:="C"],
                                 state_CH[,type:="CH"],
                                 state_CHO1[,type:="CHO"], 
                                 state_CHO2[,type:="CHO"],
                                 state_CHO3[,type:="CHO"],
                                 state_HO[,type:="HO"],
                                 state_0[,type:="Prev"]),
                    label = "States",
                    value_weight = unique(state$lab)[-4])

# hsa table
hsas = make_table(lists = list(hsa[,type:="H"],
                                 hsa_C[,type:="C"],
                                 hsa_CH[,type:="CH"],
                                 hsa_CHO1[,type:="CHO"], 
                                 hsa_CHO2[,type:="CHO"],
                                 hsa_CHO3[,type:="CHO"],
                                 hsa_HO[,type:="HO"],
                                 hsa_0[,type:="Prev"]),
                    label = "HSAs",
                    value_weight = unique(hsa$lab)[-4])

# county table
counties = make_table(lists = list(county[,type:="H"],
                                 county_C[,type:="C"],
                                 county_CH[,type:="CH"],
                                 county_CHO1[,type:="CHO"], 
                                 county_CHO2[,type:="CHO"],
                                 county_CHO3[,type:="CHO"],
                                 county_HO[,type:="HO"],
                                 county_0[,type:="Prev"]),
                    label = "Counties",
                    value_weight = unique(county$lab)[-4])

# combine into table
table = ggarrange(states[[1]], counties[[1]], 
                  ncol = 1, common.legend = T,
                  legend = "bottom")

ggsave(table, filename = here("2_Figures",  paste("table.png")), width = 10, height = 10)

# combine into table
table.2 = ggarrange(states[[1]], hsas[[1]], counties[[1]], 
                  ncol = 1, common.legend = T,
                  legend = "bottom")

ggsave(table.2, filename = here("2_Figures",  paste("table_with_HSA.png")), width = 10, height = 15)


#### TABLES ####
make_table2 = function(lists = list(county[[1]][,type:="H"]), filter_best = F, value_weight = "Neutral", met = "Weighted accuracy",
                      label = "Counties", vars = c("current_zeke", "Community Levels", "Adaptive: CHOZ", "Adaptive: CHO","Adaptive", "Simplified adaptive", "Prevalence", "Z"),
                      order = c("current_zeke", "Adaptive: CHOZ", "Adaptive: CHO", "Adaptive: H",  "Adaptive: HOZ", "Adaptive: HZ", "Simplified adaptive: HZ", "Community Levels", "Z")){
  
  # calculate states
  states_plot = rbindlist(lists) %>% filter(metric==met) %>% 
    #filter(outcome_label!="icu_2_time_3") %>%
    mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
    filter(!var%in%c("Community Levels", "Z") | type=="H") %>%
    dplyr::select(lab, lab2, var,type, quarter, metric_value) %>% 
    group_by(lab, type, lab2, var) %>% 
    mutate(ind = grepl("0", var), filter_best) %>%
    spread(quarter, metric_value) %>%
    group_by(lab, lab2, type, ind) %>% 
    mutate(max = max(Training), best = Training == max(Training) & ind,
           var = sub(" 0 0 0", "", var),
           var = ifelse(ind, type, var),
           var = factor(var)) %>% 
    ungroup() %>%
    filter(filter_best | (best | var%in%vars)) %>%
    mutate(var = ifelse(var%in%c("Adaptive", "Simplified adaptive"), paste(var, "HZ", sep = ": "), as.character(var))) %>%
    dplyr::select(-ind, -max, - best, -type, -filter_best) %>%
    mutate_if(is.numeric, function(a) round(a*100)) %>%
    arrange(lab, lab2, -Test) %>% gather(var2, value2, -1:-3) %>%
    group_by(lab, lab2) %>% mutate(value2 = ifelse(value2 < 0, 0, value2),
                                   fill = scale(value2))
  
  # set factor levels
  states_plot$var = factor(states_plot$var, levels = rev(c(order, "CHO", "HO", "CH", "O", "H", "C", "Prevalence")))  
  states_plot$var2 = sub("20", "", states_plot$var2)
  states_plot$var2 = sub(" ", "-", states_plot$var2)
  states_plot$var2 = factor(states_plot$var2, levels = sort(unique(states_plot$var2))[c(1:6,8,9,7)])
  
  # run plot
    plot = ggplot(states_plot %>% filter(lab %in% value_weight), 
                aes(x = var2, y = var,
                    fill = ifelse(var2%in%c("21-2", "21-3", "21-4", "Overall", "Training") | var == "Prevalence", NA, value2))) + 
    geom_tile() + 
    geom_text(aes(label = value2,
                  col = ifelse(var2%in%c("21-2", "21-3", "21-4", "Overall", "Training") | var == "Prevalence", "black", "white"))) + 
    facet_grid(lab2~lab, scales = "free_y") + 
    scale_x_discrete(position = "top") + 
    scale_fill_gradient(name = "", na.value = "lightgrey", guide = "none", high = "#01579b", low = "#BA0001") + 
    scale_color_manual(guide = "none", values = c("black", "white")) + 
    labs(x = "", y = "", title = label) + 
    theme_bw() +
    theme(strip.placement = "outside") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_blank(),
          axis.ticks = element_blank()) +
      annotate("rect", xmin=c(0.5), xmax=c(9.5), ymin=c(8.5), ymax=c(8.5), colour="black", fill="transparent", size=1)
    
    
    return(list(plot, states_plot))
}

#### FIGURE 3 EXPANDED ####
# state table
states = make_table2(lists = list(state[,type:="H"],
                                 state_C[,type:="C"],
                                 state_CH[,type:="CH"],
                                 state_CHO1[,type:="CHO"], 
                                 state_CHO2[,type:="CHO"],
                                 state_CHO3[,type:="CHO"],
                                 state_HO[,type:="HO"],
                                 state_0[,type:="Prev"]),
                    label = "States",
                    value_weight = unique(state$lab)[-4])

# hsa table
hsas = make_table2(lists = list(hsa[,type:="H"],
                               hsa_C[,type:="C"],
                               hsa_CH[,type:="CH"],
                               hsa_CHO1[,type:="CHO"], 
                               hsa_CHO2[,type:="CHO"],
                               hsa_CHO3[,type:="CHO"],
                               hsa_HO[,type:="HO"],
                               hsa_0[,type:="Prev"]),
                  label = "HSAs",
                  value_weight = unique(hsa$lab)[-4])

# county table
counties = make_table2(lists = list(county[,type:="H"],
                                   county_C[,type:="C"],
                                   county_CH[,type:="CH"],
                                   county_CHO1[,type:="CHO"], 
                                   county_CHO2[,type:="CHO"],
                                   county_CHO3[,type:="CHO"],
                                   county_HO[,type:="HO"],
                                   county_0[,type:="Prev"]),
                      label = "Counties",
                      value_weight = unique(county$lab)[-4])



# arrange
ggsave(states[[1]], filename = here("2_Figures",  paste("states_full.png")), width = 15, height = 8)
ggsave(hsas[[1]], filename = here("2_Figures",  paste("hsas_full.png")), width = 15, height = 8)
ggsave(counties[[1]], filename = here("2_Figures",  paste("counties_full.png")), width = 15, height = 8)

#### STATIC METRICS -- TEXT ####
states2 = make_table(lists = list(state[,type:="H"]),
                     label = "States", filter_best = T)
states2

counties2 = make_table(lists = list(county[,type:="H"]),
                       label = "Counties", filter_best = T)
counties2


#### ROC CURVES ####
library(MASS)
make_roc = function(state_HO, state_CHO, state_roc, qs = c("Test"),
                    title = "States",
                    block1 = c(15, 1/7, 1/5, 1/10, 1/2, 1/3, 1/20),
                    block2 = c(1/2, 1/3, 15,1/15, 1/5,3,7,10, 1/10, 1/7),
                    v1 = "0 5 5", v2 = "0 15 5",
                    v3 = "50 5 5", v4 = "200 5 5"){

# pull from HO
a1 = state_HO %>% mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
filter(metric %in% c("Sensitivity", "Specificity") & 
                                !var%in%c("Community Levels", "current_zeke", "Z") &
                                quarter%in%qs & outcome_label!="icu_2_time_3" & lab=="Neutral") %>%
  dplyr::select(quarter, var, lab2, outcome_label, metric,metric_value) %>% 
  spread(metric, metric_value) %>%
  group_by(lab2, outcome_label) %>%
  mutate(n = row_number(), mult = 1,
         chk = sapply(n, function(a) sum(Specificity>=Specificity[a] & Sensitivity>=Sensitivity[a]))) %>%
  #filter(chk==1) %>% 
  filter(mult==1) %>% 
  filter((var==v1 & outcome_label=="zeke_time_3") | (var==v2 & outcome_label=="two_zeke_time_3")) %>%
  mutate(col = "HO")

# pull from CHO
a2 = state_CHO %>%  mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
  filter(metric %in% c("Sensitivity", "Specificity") & 
                                 !var%in%c("Community Levels", "current_zeke", "Z") &
                            quarter%in%qs & outcome_label!="icu_2_time_3" & lab=="Neutral") %>%
  dplyr::select(quarter, var, lab2, outcome_label, metric,metric_value) %>% 
  spread(metric, metric_value) %>%
  group_by(lab2, outcome_label) %>%
  mutate(n = row_number(), mult = 1,
         chk = sapply(n, function(a) sum(Specificity>=Specificity[a] & Sensitivity>=Sensitivity[a]))) %>%
  #filter(chk==1) %>% 
  filter(mult==1) %>% 
  filter((var==v3 & outcome_label=="zeke_time_3") | (var==v4 & outcome_label=="two_zeke_time_3")) %>%
  mutate(col = "CHO")

# pull adaptive + CL
a = state_roc[[1]] %>% mutate(var = ifelse(var=="Community level", "Community Levels", as.character(var))) %>%
  filter(metric %in% c("Sensitivity", "Specificity") & (var %in% c("Adaptive", "Community Levels", "current_zeke", "Z")) & 
                                outcome_label!="icu_2_time_3" & quarter%in%qs) %>%
  dplyr::select(quarter, var, lab2, outcome_label, lab, metric,metric_value) %>% spread(metric, metric_value) %>%
  mutate(col = ifelse(var=="current_zeke", "Z", as.character(var))) %>%
  bind_rows(a1) %>%
  bind_rows(a2) %>%
  filter(!(outcome_label=="zeke_time_3" & lab%in%block1) & 
           !(outcome_label=="two_zeke_time_3" & lab%in%block2)) %>%
  mutate(col = factor(col, levels = c("Community Levels", "CHO", "HO", "Z")))

# make plot
roc = ggplot(a, aes(x = 1-Specificity, y = Sensitivity)) +
  geom_line(data = a %>% filter(var=="Adaptive")) + 
  geom_point(data = a %>% filter(var=="Adaptive"), col = "white", size = 8) + 
  geom_text(data = a %>% filter(var=="Adaptive"), size = 3, aes(label = fractions(1/lab))) +
  geom_point(data = a %>% filter(var!="Adaptive"), size = 2, alpha = .7, aes(pch = col, col = col)) + 
  scale_shape_manual(name = "", values = c(16,5,8,1)) + 
  scale_color_discrete(name = "") + 
  facet_grid(.~lab2) + ylim(0,1) + 
  theme_bw() +
  theme(strip.placement = "outside") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_blank(),
        axis.ticks = element_blank()) + labs(title = title)

# return plot
return(roc)
}

# state
s = make_roc(state_HO = state_HO, state_CHO = rbindlist(list(state_CHO1, state_CHO2, state_CHO3)), state_roc = state_roc)

# county
c = make_roc(county_HO, rbindlist(list(county_CHO1, county_CHO2, county_CHO3)), 
             county_roc, title = "Counties",
             block1 = c(1/2, 1/10, 1/5,7, 1/7, 10,15,20, 1/20),
             block2 = c(1/3, 1/2, 20, 1/7, 1/15, 1/20, 2, 3, 7), v4 = "150 10 5")

# combined
roc = ggarrange(s, c, common.legend = T, legend = "bottom", ncol = 1)
ggsave(roc, filename = here("2_Figures",  paste("roc_curves.png")), width = 8, height = 7)

#### OMICRON SUB ANALYSES ####

# main location
location = "Data/Omicron"

# load files
g = list.files(here("2_Figures",  location))
for(i in 1:length(g)) load(here("2_Figures",  location, g[i]))

# recode
state_0[var=="0 0 0", var:="Prevalence"]
county_0[var=="0 0 0", var:="Prevalence"]
hsa_0[var=="0 0 0", var:="Prevalence"]


# state table
states = make_table(lists = list(state[,type:="H"],
                                 state_C[,type:="C"],
                                 state_CH[,type:="CH"],
                                 state_CHO1[,type:="CHO"], 
                                 state_CHO2[,type:="CHO"],
                                 state_CHO3[,type:="CHO"],
                                 state_HO[,type:="HO"],
                                 state_0[,type:="Prev"]),
                    label = "States",
                    value_weight = unique(state$lab)[-4])

# hsa table
hsas = make_table(lists = list(hsa[,type:="H"],
                               hsa_C[,type:="C"],
                               hsa_CH[,type:="CH"],
                               hsa_CHO1[,type:="CHO"], 
                               hsa_CHO2[,type:="CHO"],
                               hsa_CHO3[,type:="CHO"],
                               hsa_HO[,type:="HO"],
                               hsa_0[,type:="Prev"]),
                  label = "HSAs",
                  value_weight = unique(hsa$lab)[-4])

# county table
counties = make_table(lists = list(county[,type:="H"],
                                   county_C[,type:="C"],
                                   county_CH[,type:="CH"],
                                   county_CHO1[,type:="CHO"], 
                                   county_CHO2[,type:="CHO"],
                                   county_CHO3[,type:="CHO"],
                                   county_HO[,type:="HO"],
                                   county_0[,type:="Prev"]),
                      label = "Counties",
                      value_weight = unique(county$lab)[-4])

# combine into table
table.2 = ggarrange(states[[1]], hsas[[1]], counties[[1]], 
                    ncol = 1, common.legend = T,
                    legend = "bottom")

ggsave(table.2, filename = here("2_Figures",  paste("table_with_HSA_OMICRON.png")), width = 10, height = 15)

