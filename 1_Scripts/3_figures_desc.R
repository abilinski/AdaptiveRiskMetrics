#### FIGURE 1 AND FRIENDS ####
library(here)
source(here("global_options.R"))

# generate data for timeline
df1 = data.frame(time = 3:9, label = c("X[i][','][w-6]", "X[i][','][w-5]", "X[i][','][w-4]", "X[i][','][w-3]", "", "", "X[i][','][w]"), x = "Inputs \n(Indicators)", x_val = 1.75)
df2 = data.frame(time = 3:6, label = c("Y[i][','][w-3]", "Y[i][','][w-2]", "Y[i][','][w-1]", "Y[i][','][w]"), x = "Outputs \n(Lagged mortality)", x_val = .75)
df = bind_rows(df1, df2) %>% mutate(x = factor(x), x = factor(x, levels = c("Outputs \n(Lagged mortality)", "Inputs \n(Indicators)")))

# make figure 1
ggplot(df, aes(x = x, y = time)) + geom_point() + #geom_line() + 
  geom_text(aes(x = x_val, y = time, label = label), parse = T) + 
  coord_flip() + 
  geom_rect(xmin = .5, xmax = 2.2, ymin = 2.75, ymax = 6.25, fill = NA, color = "black", lwd = .25) +
  geom_rect(xmin = .5, xmax = 2.2, ymin = 8.75, ymax = 9.25, fill = NA, color = "black", lwd = .25) +
  labs(x = "", y = "") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank()) +
  geom_point(x = 1, y = 9, col = "#01579b") + 
  geom_segment(aes(x = 1, y = 6.25, xend = 1, yend = 8.75),
               arrow = arrow(length = unit(0.2, "cm")), lty = 1, lwd = .2, col = "#01579b") + 
  geom_text(x = 1.25, y = 7.75, label = "Model", col = "#01579b") + 
  geom_text(x = .75, y = 9, label = expression(hat(Y)[i][','][w+3]), col = "#01579b") + 
  geom_text(x = 2.5, y = 4.5, label = "Training data", col = "#01579b") + 
  geom_text(x = 2.5, y = 9, label = "Test data", col = "#01579b") + 
  geom_text(x = .25, y = 4.5, label = "Optimize:", col = "#01579b") +
  geom_text(x = .25, y = 9, label = "Apply:", col = "#01579b") +
  scale_x_discrete(expand = expansion(mult = 1.2)) + 
  scale_y_discrete(expand = expansion(mult = .1)) + 
  geom_text(x = 0, y = 4.5, label = expression(widehat(beta)~"="~"arg min"[beta]~(Y[i][','][v] - f(X[i][','][v-3], beta))^2)) +
  geom_text(x = 0, y = 9, label = expression(widehat(Y)[i][','][w+3]~"="~f(X[i][','][w], widehat(beta))))

# export Figure 1
ggsave(filename = here("3_Figures", "figure_diag.png"), width = 8, height = 3)

#### FIGURE 2 ####

# load data
# state
load(here("0_Data", "Cleaned", "state_time_data.RData"))

# county
load(here("0_Data", "Cleaned", "county_time_data.RData"))

# run figures
for(i in c("State", "County")){
  if(i == "State") d_out_pre = d_out_pre_state
  if(i == "County") d_out_pre = d_out_pre_cty
figure1_data = d_out_pre %>% 
  gather(var, value, cases_weekly, admits_weekly, perc_covid_100) %>%
  filter(date >= "2021-04-01" & date<="2022-10-01") %>%
  mutate(
         var2 = case_when(var=="cases_weekly"~"Cases/100K", 
                          var=="admits_weekly"~"Hospital admissions/100K",
                          var=="perc_covid_100"~"Percent bed occupancy",
                          var=="icu_weekly"~"ICU admissions/100K"),
         outcome_value2 = case_when(two_zeke_time_3~">2 deaths/100K/week",
                                    (zeke_time_3&!two_zeke_time_3)~"1-2 deaths/100K/week",
                                    (!zeke_time_3&!two_zeke_time_3)~"<1 death/100K/week"),
         outcome_value2 = factor(outcome_value2,
                                 levels = c("<1 death/100K/week",
                                            "1-2 deaths/100K/week",
                                            ">2 deaths/100K/week")))

figure1_data3 = figure1_data %>%
  group_by(ymd, var2, outcome_value2) %>% 
  summarize(med = median(value), 
            q25 = quantile(value, .25),
            q75 = quantile(value, .75),
            n = length(value)) %>%
  group_by(ymd, var2) %>% mutate(perc = n/sum(n)*100)

ggplot(figure1_data3, aes(x = ymd, y = med, group = paste(ymd, outcome_value2), 
                          col = outcome_value2)) + 
  geom_point(aes(size = perc), alpha = .5) +
  scale_size(name = "Percentage of units", range = c(.05, 3)) + 
  geom_errorbar(aes(ymin = q25, ymax = q75), lwd = .3) + 
  facet_wrap(var2~., scales = "free", ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_date(breaks = "1 month") + labs(x = "", y = "", title = "") + 
  scale_color_manual(name = "3-week lagged mortality", 
                     values = c("#01579b", "#FF7F7F", "#BA0001")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(figure1_data3, aes(x = ymd, y = med, group = paste(ymd, outcome_value2), 
                          col = outcome_value2)) + 
  geom_point(aes(size = perc), alpha = .5) +
  scale_size(name = "Percentage of units", range = c(.05, 3)) + 
  geom_errorbar(aes(ymin = q25, ymax = q75), lwd = .3) + 
  facet_wrap(var2~., scales = "free", ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_x_date(breaks = "1 month") + labs(x = "", y = "", title = "") + 
  scale_color_manual(name = "3-week lagged mortality", 
                     values = c("#01579b", "#FF7F7F", "#BA0001")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  scale_y_continuous(trans='log2',
                     breaks = trans_breaks("log2", function(x) round(10^x))) 
ggsave(filename = here("2_Figures", paste("fig_long", i, ".png", sep = "")), width = 8, height = 8)

figure1_data4 = figure1_data %>% filter(date<="2022-10-01") %>% #filter(deaths_avg_per_100k*7<2) %>%
  mutate(
         quarter = paste(year(ymd), quarter(ymd), sep = "-"))

ggplot(figure1_data4 %>% filter(POPESTIMATE2019 > 500000),
       aes(x = log(value), y = log(deaths_weekly))) + 
  geom_point(alpha = ifelse(i=="County", .05, .1), aes(col = outcome_value2)) +
  facet_grid(quarter~var2, scales = "free_x") +
  theme_bw() +
  scale_color_manual(name = "", 
                     values = c("#01579b", "#ec8400", "#b02912")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "bottom") +
  scale_x_continuous(labels=function(x)round(exp(x))) +
  scale_y_continuous(labels=function(x) ifelse(round(exp(x)) < 1, round(exp(x),1), round(exp(x)))) + 
  labs(x = "", y = "") +
  labs(x = "", y = "3-week lagged mortality per 100K (log scale)") + 
  geom_vline(figure1_data4 %>% filter(var2=="Cases/100K"), mapping = aes(xintercept = log(200)), col = "darkgrey", lty = 2) +
  geom_vline(figure1_data4 %>% filter(var2=="Hospital admissions/100K"), mapping = aes(xintercept = log(10)), col = "darkgrey", lty = 2) + 
  geom_vline(figure1_data4 %>% filter(var2=="Percent bed occupancy"), mapping = aes(xintercept = log(10)), col = "darkgrey", lty = 2) 

ggsave(filename = here("3_Figures", paste0("fig2", i, ".png")), width = 7, height = 10)
}

