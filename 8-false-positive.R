rm(list = ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_2025-08-28.rda")

# set critical value based on alpha of 0.05 (about 1.96)
crit_value <- qnorm(0.025, lower.tail=FALSE)

results_select_err <- results_clean %>% 
  # only 0 policy effects 
  filter(policy_intercept == 0 & policy_slope == 0) %>%
  # reject based on estimates, standard error, and critical value
  mutate(policy_intercept_rej_se = 
           abs(policy_intercept_b / policy_intercept_se) >= crit_value,
         policy_intercept_rej_nwse = 
           abs(policy_intercept_b / policy_intercept_nwse) >= crit_value,
         policy_slope_rej_se =
           abs(policy_slope_b / policy_slope_se) >= crit_value,
         policy_slope_rej_nwse = 
           abs(policy_slope_b / policy_slope_nwse) >= crit_value
  )

# supplemental table 
results_overall_sum <- results_select_err %>% 
  group_by(method) %>%
  summarize(t1e_intercept_se = mean(policy_intercept_rej_se),
            t1e_intercept_nwse = mean(policy_intercept_rej_nwse),
            t1e_slope_se = mean(policy_slope_rej_se),
            t1e_slope_nwse = mean(policy_slope_rej_nwse))
results_overall_sum
write.csv(results_overall_sum,
          file="results/summary_t1e.csv",
          quote = FALSE,
          row.names=FALSE)
results_overall_sum <- results_select_err %>% 
  filter(policy_start_date == as.yearmon("Dec 2018") | 
           policy_start_date == as.yearmon("Jun 2021")) %>%
  group_by(method, policy_start_date) %>%
  summarize(t1e_intercept_se = mean(policy_intercept_rej_se),
            t1e_intercept_nwse = mean(policy_intercept_rej_nwse),
            t1e_slope_se = mean(policy_slope_rej_se),
            t1e_slope_nwse = mean(policy_slope_rej_nwse))
write.csv(results_overall_sum,
          file="results/summary_t1e_split.csv",
          quote = FALSE,
          row.names=FALSE)

# all scenarios
results_sum <- results_select_err %>%
  group_by(covid_scenario, method, policy_start_date, policy_intercept, policy_slope) %>%
  summarise(prop_rej_intercept_se = mean(policy_intercept_rej_se),
            prop_rej_intercept_nwse = mean(policy_intercept_rej_nwse),
            prop_rej_slope_se = mean(policy_slope_rej_se),
            prop_rej_slope_nwse = mean(policy_slope_rej_nwse))

# Figure 4 of the manuscript (main figure)
results_sum_maintext <- results_sum %>% 
  filter(covid_scenario %in% c("Lin. 3mo",
                               "Lin. 1yr",
                               "Lin. 2yr",
                               "Permanent",
                               "Nonlinear") &
           method %in% c("DID", 
                         "ITS RTN 8mo",
                         "ITS Assume-May 2020",
                         "ITS Assume-Feb 2021",
                         "ITS Assume-Feb 2022",
                         "ITS Assume-Dec 2023")
  )

## plot
results_sum_maintext %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_nwse,
                linetype = "Intercept")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_nwse,
                linetype = "Slope")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Parameter") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.2) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/8-figure4-t1e.png", width=4000, height=3000, units="px")

## Supplemental Figures
### Supp Figure for Type 1 Error (intercept): assume, include Newey West and Vanilla Errors
results_sum %>%
  filter(method %in% 
           c("ITS Assume-May 2020",
             "ITS Assume-Aug 2020",
             "ITS Assume-Feb 2021",
             "ITS Assume-Aug 2021",
             "ITS Assume-Feb 2022",
             "ITS Assume-Aug 2022",
             "ITS Assume-Feb 2023",
             "ITS Assume-May 2023",
             "ITS Assume-Dec 2023")) %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_se,
                linetype = "Conventional SE")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_nwse,
                linetype = "Newey-West SE")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Standard Error Type") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/8-suppfig-t1e-intercept-ses-assume.png", width=8, height=6, units="in", dpi=500, scale=2)

### Supp Figure for Type 1 Error (intercept): DID+rtns, include Newey West and Vanilla Errors
results_sum %>%
  filter(method %in% 
           c("DID",
             "ITS RTN 1mo",
             "ITS RTN 2mo",
             "ITS RTN 3mo",
             "ITS RTN 4mo",
             "ITS RTN 5mo",
             "ITS RTN 6mo",
             "ITS RTN 7mo",
             "ITS RTN 8mo",
             "ITS RTN 9mo",
             "ITS RTN 10mo",
             "ITS RTN 11mo",
             "ITS RTN 12mo")) %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_se,
                linetype = "Conventional SE")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_nwse,
                linetype = "Newey-West SE")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Standard Error Type") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/8-suppfig-t1e-intercept-ses-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)

### Supp Figure for T1E (slope): assume, include Newey West and Vanilla Errors
results_sum %>%
  filter(method %in% 
           c("ITS Assume-May 2020",
             "ITS Assume-Aug 2020",
             "ITS Assume-Feb 2021",
             "ITS Assume-Aug 2021",
             "ITS Assume-Feb 2022",
             "ITS Assume-Aug 2022",
             "ITS Assume-Feb 2023",
             "ITS Assume-May 2023",
             "ITS Assume-Dec 2023")) %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_se,
                linetype = "Conventional SE")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_nwse,
                linetype = "Newey-West SE")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Standard Error Type") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/8-suppfig-t1e-slope-ses-assume.png", width=8, height=6, units="in", dpi=500, scale=2)

### Supp Figure for T1E (slope): DID+rtn, include Newey West and Vanilla Errors
results_sum %>%
  filter(method %in% 
           c("DID",
             "ITS RTN 1mo",
             "ITS RTN 2mo",
             "ITS RTN 3mo",
             "ITS RTN 4mo",
             "ITS RTN 5mo",
             "ITS RTN 6mo",
             "ITS RTN 7mo",
             "ITS RTN 8mo",
             "ITS RTN 9mo",
             "ITS RTN 10mo",
             "ITS RTN 11mo",
             "ITS RTN 12mo")) %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_se,
                linetype = "Conventional SE")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_nwse,
                linetype = "Newey-West SE")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Standard Error Type") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.4) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/8-suppfig-t1e-slope-ses-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)
