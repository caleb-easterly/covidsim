rm(list = ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_2026-03-16.rda")

# set critical value based on alpha of 0.05 (about 1.96)
crit_value <- qnorm(0.025, lower.tail=FALSE)

results_select_err <- results_clean %>% 
  # take out 0 policy effects 
  filter(policy_intercept != 0 & policy_slope != 0) %>%
  # reject based on estimates, standard error, and critical value
  mutate(policy_intercept_rej_se = 
           abs(policy_intercept_b / policy_intercept_se) >= crit_value,
         policy_slope_rej_se =
           abs(policy_slope_b / policy_slope_se) >= crit_value
  )

# supplemental table 
results_overall_sum <- results_select_err %>% 
  filter(policy_intercept > 0 & 
           policy_slope > 0) %>%
  group_by(method) %>%
  summarize(power_intercept_se = mean(policy_intercept_rej_se),
            power_slope_se = mean(policy_slope_rej_se))
results_overall_sum
write.csv(results_overall_sum,
          file="results/summary_power.csv",
          quote = FALSE,
          row.names=FALSE)
# how does this look with june 2019 and june 2021
results_overall_sum <- results_select_err %>% 
  # positive parameters only so bias doesn't cancel out
  filter(policy_intercept == 2.5 & 
           policy_slope == 0.25) %>%
  filter(policy_start_date == as.yearmon("Dec 2018") | 
           policy_start_date == as.yearmon("Jun 2021")) %>%
  group_by(method, policy_start_date) %>%
  summarize(power_intercept_se = mean(policy_intercept_rej_se),
            power_slope_se = mean(policy_slope_rej_se))
write.csv(results_overall_sum,
          file="results/summary_power_split.csv",
          quote = FALSE,
          row.names=FALSE)


# all scenarios
results_sum <- results_select_err %>%
  group_by(covid_scenario, method, policy_start_date, policy_intercept, policy_slope) %>%
  summarise(prop_rej_intercept_se = mean(policy_intercept_rej_se),
            prop_rej_slope_se = mean(policy_slope_rej_se))

# Figure 3 of the manuscript (main figure)
results_sum_maintext <- results_sum %>% 
  # for main results, only single set of policy intercepts and slopes
  filter(policy_intercept == 2.5 & 
           policy_slope == 0.25) %>%
  filter(covid_scenario %in% c("Lin. 3mo",
                               "Lin. 1yr",
                               "Lin. 2yr",
                               "Permanent",
                               "Nonlinear") &
           method %in% c("CITS", 
                         "ITS-Grid Search",
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
                y = prop_rej_intercept_se,
                linetype = "Intercept")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_se,
                linetype = "Slope")) +
  labs(title="",
       y="Power",
       x="Policy Start Date",
       linetype="Parameter") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.2) +
  geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/7-figure3-power.png", width=3500, height=2500, units="px")
ggsave("figures/7-figure3-power.pdf", width=3500, height=2500, units="px")


# supplemental figure 1 - power across all scenarios, intercept
results_sum %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(
    aes(x = policy_start_date,
        y = prop_rej_intercept_se,
        group = as.factor(policy_intercept),
        color = as.factor(policy_intercept))) +
  labs(title="",
       y="Power",
       x="Policy Start Date",
       color="True Policy Intercept") +
  theme_bw() +
  geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) + 
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/7-suppfig-intercept-power-all.png", width=10, height=7, units="in", dpi=500, scale=2)

# supplemental figure 1 - percent bias across all scenarios, slope
results_sum %>%
  ggplot() + 
  facet_grid(covid_scenario ~ method) +
  geom_line(
    aes(x = policy_start_date,
        y = prop_rej_slope_se,
        group = as.factor(policy_slope),
        color = as.factor(policy_slope))) +
  labs(title="",
       y="Power",
       x="Policy Start Date",
       color="True Policy Slope") +
  theme_bw() +
  geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") + 
  guides(colour = guide_legend(nrow = 1))
ggsave("figures/7-suppfig-slope-power-all.png", width=10, height=7, units="in", dpi=500, scale=2)

# ## Supplemental Figures
# ### Supp Figure for Power (intercept): ITS assume, include Newey West and Vanilla Errors
# results_sum %>%
#   filter(method %in% 
#            c("ITS Assume-May 2020",
#              "ITS Assume-Aug 2020",
#              "ITS Assume-Feb 2021",
#              "ITS Assume-Aug 2021",
#              "ITS Assume-Feb 2022",
#              "ITS Assume-Aug 2022",
#              "ITS Assume-Feb 2023",
#              "ITS Assume-May 2023",
#              "ITS Assume-Dec 2023")) %>%
#   ggplot() + 
#   facet_grid(covid_scenario ~ method) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_intercept_se,
#                 linetype = "Conventional SE",
#                 group = as.factor(policy_intercept),
#                 color = as.factor(policy_intercept))) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_intercept_nwse,
#                 linetype = "Newey-West SE",
#                 group = as.factor(policy_intercept),
#                 color = as.factor(policy_intercept))) +
#   labs(title="",
#        y="Power",
#        x="Policy Start Date",
#        linetype="Standard Error Type",
#        color="True Policy Intercept") +
#   theme_bw() +
#   geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
#   geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom") + 
#   guides(colour = guide_legend(nrow = 1))
# ggsave("figures/7-suppfig-power-intercept-ses-assume.png", width=8, height=6, units="in", dpi=500, scale=2)
# 
# ### Supp Figure for Power (intercept): DID rtn, include Newey West and Vanilla Errors
# results_sum %>%
#   filter(method %in% 
#            c("DID",
#              "ITS RTN 1mo",
#              "ITS RTN 2mo",
#              "ITS RTN 3mo",
#              "ITS RTN 4mo",
#              "ITS RTN 5mo",
#              "ITS RTN 6mo",
#              "ITS RTN 7mo",
#              "ITS RTN 8mo",
#              "ITS RTN 9mo",
#              "ITS RTN 10mo",
#              "ITS RTN 11mo",
#              "ITS RTN 12mo")) %>%
#   ggplot() + 
#   facet_grid(covid_scenario ~ method) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_intercept_se,
#                 linetype = "Conventional SE",
#                 group = as.factor(policy_intercept),
#                 color = as.factor(policy_intercept))) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_intercept_nwse,
#                 linetype = "Newey-West SE",
#                 group = as.factor(policy_intercept),
#                 color = as.factor(policy_intercept))) +
#   labs(title="",
#        y="Power",
#        x="Policy Start Date",
#        linetype="Standard Error Type",
#        color="True Policy Intercept") +
#   theme_bw() +
#   geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
#   geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom") + 
#   guides(colour = guide_legend(nrow = 1))
# ggsave("figures/7-suppfig-power-intercept-ses-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)
# 
# ### Supp Figure for Power (slope): ITS assume, include Newey West and Vanilla Errors
# results_sum %>%
#   filter(method %in% 
#            c("ITS Assume-May 2020",
#              "ITS Assume-Aug 2020",
#              "ITS Assume-Feb 2021",
#              "ITS Assume-Aug 2021",
#              "ITS Assume-Feb 2022",
#              "ITS Assume-Aug 2022",
#              "ITS Assume-Feb 2023",
#              "ITS Assume-May 2023",
#              "ITS Assume-Dec 2023")) %>%
#   ggplot() + 
#   facet_grid(covid_scenario ~ method) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_slope_se,
#                 linetype = "Conventional SE",
#                 group = as.factor(policy_slope),
#                 color = as.factor(policy_slope))) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_slope_nwse,
#                 linetype = "Newey-West SE",
#                 group = as.factor(policy_slope),
#                 color = as.factor(policy_slope))) +
#   labs(title="",
#        y="Power",
#        x="Policy Start Date",
#        linetype="Standard Error Type",
#        color="True Policy Slope") +
#   theme_bw() +
#   geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
#   geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom") + 
#   guides(colour = guide_legend(nrow = 1))
# ggsave("figures/7-suppfig-power-slope-ses-assume.png", width=8, height=6, units="in", dpi=500, scale=2)
# 
# ### Supp Figure for Power (slope): DID rtn, include Newey West and Vanilla Errors
# results_sum %>%
#   filter(method %in% 
#            c("DID",
#              "ITS RTN 1mo",
#              "ITS RTN 2mo",
#              "ITS RTN 3mo",
#              "ITS RTN 4mo",
#              "ITS RTN 5mo",
#              "ITS RTN 6mo",
#              "ITS RTN 7mo",
#              "ITS RTN 8mo",
#              "ITS RTN 9mo",
#              "ITS RTN 10mo",
#              "ITS RTN 11mo",
#              "ITS RTN 12mo")) %>%
#   ggplot() + 
#   facet_grid(covid_scenario ~ method) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_slope_se,
#                 linetype = "Conventional SE",
#                 group = as.factor(policy_slope),
#                 color = as.factor(policy_slope))) +
#   geom_line(aes(x = policy_start_date,
#                 y = prop_rej_slope_nwse,
#                 linetype = "Newey-West SE",
#                 group = as.factor(policy_slope),
#                 color = as.factor(policy_slope))) +
#   labs(title="",
#        y="Power",
#        x="Policy Start Date",
#        linetype="Standard Error Type",
#        color="True Policy Slope") +
#   theme_bw() +
#   geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.3) +
#   geom_hline(aes(yintercept = 0.8), linetype=2, linewidth=0.3) + 
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "bottom") + 
#   guides(colour = guide_legend(nrow = 1))
# ggsave("figures/7-suppfig-power-slope-ses-didrtn.png", width=8, height=6, units="in", dpi=500, scale=2)
