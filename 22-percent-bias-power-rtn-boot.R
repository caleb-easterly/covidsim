# rm(list=ls())

library(tidyverse)
library(reshape2)

load("results/results_clean_rtn_boot-09-17.rda")

results_select_err <- results_clean %>% 
  # take out 0 policy effects 
  filter(policy_intercept != 0 & policy_slope != 0) %>%
  # raw (untransfomed) error
  mutate(policy_intercept_err = intercept_est - policy_intercept,
         policy_slope_err = slope_est - policy_slope) %>%
  # relative error
  mutate(policy_intercept_relerr = 100*(intercept_est - policy_intercept)/policy_intercept,
         policy_slope_relerr = 100*(slope_est - policy_slope)/policy_slope) %>%
  # squared relative error
  mutate(policy_intercept_sqerr = policy_intercept_relerr^2,
         policy_slope_sqerr = policy_slope_relerr^2) %>%
  # CI width 
  mutate(int_ci_width = intercept_ub - intercept_lb,
         slope_ci_width = slope_ub - slope_lb) %>%
  # reject null
  mutate(int_reject_null = (intercept_lb > 0) | (intercept_ub) < 0,
         slope_reject_null = (slope_lb > 0) | (slope_ub) < 0)

# whate percentage of intervals are wider?
results_sim_compare <- results_select_err %>%
  select(rep, int_ci_width, slope_ci_width, method)

# reshape 
results_sim_compare_wide <- reshape(results_sim_compare, 
                                    idvar = "rep", 
                                    direction="wide",
                                    timevar = "method")
colnames(results_sim_compare_wide) <- c("rep",
                                        "int_ci_width_onestep",
                                        "slope_ci_width_onestep",
                                        "int_ci_width_boot",
                                        "slope_ci_width_boot")

results_sim_compare_wide <- results_sim_compare_wide %>%
  mutate(rel_ci_width_int = int_ci_width_boot/int_ci_width_onestep,
         rel_ci_width_slope = slope_ci_width_boot/slope_ci_width_onestep)

library(ggplot2)
library(patchwork)
gint <- ggplot(results_sim_compare_wide) +
  geom_histogram(aes(x=rel_ci_width_int)) +
  theme_bw() +
  ggtitle("Intercept") +
  scale_x_continuous(name = "Relative CI Width (bootstrap/one-step)") +
  scale_y_continuous(name = "Frequency")

gslope <- ggplot(results_sim_compare_wide) +
  geom_histogram(aes(x=rel_ci_width_slope)) +
  theme_bw() +
  ggtitle("Slope") +
  scale_x_continuous(name = "Relative CI Width (bootstrap/one-step)") +
  scale_y_continuous(name = "Frequency")

gint + gslope
ggsave("figures/22-ci-width.png", width = 8, height=5, units = "in", dpi=300)

# supplemental table
results_overall_sum <- results_select_err %>%
  group_by(method) %>%
  summarize(int_est = mean(intercept_est),
            int_lb = mean(intercept_lb),
            int_ub = mean(intercept_ub),
            slope_est = mean(slope_est),
            slope_lb = mean(slope_lb),
            slope_ub = mean(slope_ub),
            pbias_intercept = mean(policy_intercept_relerr),
            pbias_slope = mean(policy_slope_relerr),
            int_reject_null = mean(int_reject_null),
            slope_rej_null = mean(slope_reject_null)) %>%
  select(-method)
results_overall_sum_long <- t(results_overall_sum)
colnames(results_overall_sum_long) <- c("One Step", "Bootstrapped")
write.csv(results_overall_sum_long,
          file="results/22-summary_pbias_rtn_boot.csv",
          quote = FALSE,
          row.names=TRUE)
