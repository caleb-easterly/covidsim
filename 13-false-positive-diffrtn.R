rm(list = ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_diffrtn_2026-03-16.rda")

# set critical value based on alpha of 0.05 (about 1.96)
crit_value <- qnorm(0.025, lower.tail=FALSE)
results_select_err <- results_clean %>% 
  # only 0 policy effects 
  filter(policy_intercept == 0 & policy_slope == 0) %>%
  # reject based on estimates, standard error, and critical value
  mutate(policy_intercept_rej_se = 
           abs(policy_intercept_b / policy_intercept_se) >= crit_value,
         policy_slope_rej_se =
           abs(policy_slope_b / policy_slope_se) >= crit_value
  )


# clean up group labels
results_select_err$which_first <- factor(results_select_err$which_first, 
                                         levels = c("ctrl", "trt"),
                                         labels = c("Control RTN Earlier",
                                                    "Treatment RTN Earlier"))

levels(results_select_err$method)[levels(results_select_err$method) == "GS-ITS"] <- "ITS-Grid Search"

# supplemental table 
results_overall_sum <- results_select_err %>% 
  group_by(method, which_first) %>%
  summarize(t1e_intercept_se = mean(policy_intercept_rej_se),
            t1e_slope_se = mean(policy_slope_rej_se))
results_overall_sum
write.csv(results_overall_sum,
          file="results/diffrtn_summary_t1e.csv",
          quote = FALSE,
          row.names=FALSE)

# all scenarios
results_sum <- results_select_err %>%
  group_by(which_first, method, policy_start_date, policy_intercept, policy_slope) %>%
  summarise(prop_rej_intercept_se = mean(policy_intercept_rej_se),
            prop_rej_slope_se = mean(policy_slope_rej_se))

# Figure 4 of the manuscript (main figure)
results_sum_maintext <- results_sum %>%
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

## plot
results_sum_maintext %>%
  ggplot() + 
  facet_grid(which_first ~ method) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_intercept_se,
                linetype = "Intercept")) +
  geom_line(aes(x = policy_start_date,
                y = prop_rej_slope_se,
                linetype = "Slope")) +
  labs(title="",
       y="Type 1 Error Rate",
       x="Policy Start Date",
       linetype="Parameter") +
  theme_bw() +
  geom_vline(aes(xintercept = as.yearmon("Mar 2020")), linetype=2, linewidth=0.2) +
  geom_hline(aes(yintercept = 0.05), linetype=2, linewidth=0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("figures/13-suppfig-diffrtn-t1e.png", width=3500, height=2000, units="px")
ggsave("figures/13-suppfig-diffrtn-t1e.pdf", width=3500, height=2000, units="px")
