rm(list=ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_2025-08-28.rda")

results_select_err <- results_clean %>% 
  # take out 0 policy effects 
  filter(policy_intercept != 0 & policy_slope != 0) %>%
  # raw (untransfomed) error
  mutate(policy_intercept_err = policy_intercept_b - policy_intercept,
         policy_slope_err = policy_slope_b - policy_slope) %>%
  # relative error
  mutate(policy_intercept_relerr = 100*(policy_intercept_b - policy_intercept)/policy_intercept,
         policy_slope_relerr = 100*(policy_slope_b - policy_slope)/policy_slope) %>%
  # squared relative error
  mutate(policy_intercept_sqerr = policy_intercept_relerr^2,
         policy_slope_sqerr = policy_slope_relerr^2)

# all scenarios
results_sum <- results_select_err %>%
  group_by(covid_scenario, method, policy_start_date, policy_intercept, policy_slope) %>%
  summarise(bias_intercept = mean(policy_intercept_err),
            pbias_intercept = mean(policy_intercept_relerr),
            rmspe_intercept = sqrt(mean(policy_intercept_sqerr)),
            bias_slope = mean(policy_slope_err),
            pbias_slope = mean(policy_slope_relerr),
            rmspe_slope = sqrt(mean(policy_slope_sqerr))) %>%
  mutate(rmspe_total = rmspe_intercept + rmspe_slope)

# Figure 2 of the manuscript (main figure)
results_sum_maintext <- results_sum %>% 
  # for main results, only single set of policy intercepts and slopes
  filter(policy_intercept == 2.5 & 
           policy_slope == 0.25) %>%
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
results_sum_maintext %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(aes(x = policy_start_date,
                y = pbias_intercept,
                linetype = "Intercept")) +
  geom_line(aes(x = policy_start_date,
                y = pbias_slope,
                linetype = "Slope")) +
  labs(title="",
       y="% Bias",
       x="Policy Start Date",
       linetype="Parameter") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.1) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/6-figure2-pbias.png", width=4000, height=3000, units="px")

# supplemental table 
results_overall_sum <- results_select_err %>% 
  # positive parameters only so bias doesn't cancel out
  filter(policy_intercept > 0 & 
           policy_slope > 0) %>%
  group_by(method) %>%
  summarize(pbias_intercept = mean(policy_intercept_relerr),
            pbias_slope = mean(policy_slope_relerr))
results_overall_sum
write.csv(results_overall_sum,
          file="results/summary_pbias.csv",
          quote = FALSE,
          row.names=FALSE)

# split into policy before and after COVID
results_overall_sum <- results_select_err %>% 
  # positive parameters only so bias doesn't cancel out
  filter(policy_intercept > 0 & 
           policy_slope > 0) %>%
  filter(policy_start_date == as.yearmon("Dec 2018") | 
           policy_start_date == as.yearmon("Jun 2021")) %>%
  group_by(method, policy_start_date) %>%
  summarize(pbias_intercept = mean(policy_intercept_relerr),
            pbias_slope = mean(policy_slope_relerr))
write.csv(results_overall_sum,
          file="results/summary_pbias_split.csv",
          quote = FALSE,
          row.names=FALSE)

# supplemental figure 1 - percent bias across all scenarios, intercept - ITS assume only
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
  geom_line(
    aes(x = policy_start_date,
        y = pbias_intercept,
        group = as.factor(policy_intercept),
        color = as.factor(policy_intercept))) +
  labs(title="",
       y="Intercept Change",
       x="Policy Start Date",
       color="True Policy Intercept") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/6-suppfig-intercept-pbias-assume.png", width=8, height=6, units="in", dpi=500, scale=2)

# supplemental figure 2 - percent bias across all scenarios, RTN and DID only
results_sum_didrtn <- results_sum %>%
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
             "ITS RTN 12mo"))
results_sum_didrtn %>% 
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(
    aes(x = policy_start_date,
        y = pbias_intercept,
        group = as.factor(policy_intercept),
        color = as.factor(policy_intercept))) +
  labs(title="",
       y="Intercept Change",
       x="Policy Start Date",
       color="True Policy Intercept") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/6-suppfig-intercept-pbias-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)

# supplemental figure 3 - percent bias across all scenarios, slope - its assume only
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
  geom_line(
    aes(x = policy_start_date,
        y = pbias_slope,
        group = as.factor(policy_slope),
        color = as.factor(policy_slope))) +
  labs(title="",
       y="Slope Change",
       x="Policy Start Date",
       color="True Policy Slope") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/6-suppfig-slope-pbias-assume.png", width=8, height=6, units="in", dpi=500, scale=2)

# supplemental figure 4 - percent bias across all scenarios, RTN and DID only
results_sum_didrtn %>% 
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(
    aes(x = policy_start_date,
        y = pbias_slope,
        group = as.factor(policy_slope),
        color = as.factor(policy_slope))) +
  labs(title="",
       y="Intercept Change",
       x="Policy Start Date",
       color="True Policy Slope") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/6-suppfig-slope-pbias-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)
