rm(list=ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_diffrtn_2026-03-16.rda")

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
  group_by(method,
           policy_start_date,
           policy_intercept,
           policy_slope,
           which_first,
           covid_end_true_ctrl,
           covid_end_true_trt) %>%
  summarise(bias_intercept = mean(policy_intercept_err),
            pbias_intercept = mean(policy_intercept_relerr),
            rmspe_intercept = sqrt(mean(policy_intercept_sqerr)),
            bias_slope = mean(policy_slope_err),
            pbias_slope = mean(policy_slope_relerr),
            rmspe_slope = sqrt(mean(policy_slope_sqerr))) %>%
  mutate(rmspe_total = rmspe_intercept + rmspe_slope)

# clean up group labels
results_sum$which_first <- factor(results_sum$which_first, 
                                  levels = c("ctrl", "trt"),
                                  labels = c("Control RTN Earlier",
                                             "Treatment RTN Earlier"))

# rename method
levels(results_sum$method)[levels(results_sum$method) == "GS-ITS"] <- "ITS-Grid Search"

# Supplementary Figure
results_sum_maintext <- results_sum %>%
  # for main results, only single set of policy intercepts and slopes
  filter(policy_intercept == 2.5 &
           policy_slope == 0.25) %>%
  filter(
    method %in% c(
      "CITS",
      "ITS-Grid Search",
      "ITS Assume-May 2020",
      "ITS Assume-Feb 2021",
      "ITS Assume-Feb 2022",
      "ITS Assume-Dec 2023"
    )
  )
results_sum_maintext %>%
  ggplot() + 
  facet_grid(which_first ~ method) +
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
ggsave("figures/11-suppfig-pbias-diffrtn.png", width=3500, height=2000, units="px")
ggsave("figures/11-suppfig-pbias-diffrtn.pdf", width=3500, height=2000, units="px")

# supplemental table 
results_overall_sum <- results_select_err %>% 
  # positive parameters only so bias doesn't cancel out
  filter(policy_intercept > 0 & 
           policy_slope > 0) %>%
  group_by(method, which_first, covid_end_true_ctrl, covid_end_true_trt) %>%
  summarize(pbias_intercept = mean(policy_intercept_relerr),
            pbias_slope = mean(policy_slope_relerr))
results_overall_sum
write.csv(results_overall_sum,
          file="results/summary_pbias_diffrtn.csv",
          quote = FALSE,
          row.names=FALSE)
