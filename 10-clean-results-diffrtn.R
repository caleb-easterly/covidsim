library(tidyverse)
library(reshape2)
load("results/results_all_methods_diffrtn_2026-03-16.rda")
results_clean <- results_all_methods %>%
  # prettify names of methods
  mutate(method = 
           case_match(
             method,
             "did" ~ "CITS",
             "lm_assume" ~ "ITS Assume",
             "lm_rtn_grid" ~ "GS-ITS",
             .default = method)
  ) %>%
  mutate(method = ifelse(method == "ITS Assume",
                         paste(method, assumed_rtn_date, sep="-"),
                         method))

results_clean$method <- factor(results_clean$method,
                               levels = c("CITS",
                                          "GS-ITS",
                                          "ITS Assume-May 2020",
                                          "ITS Assume-Aug 2020",
                                          "ITS Assume-Feb 2021",
                                          "ITS Assume-Aug 2021",
                                          "ITS Assume-Feb 2022",
                                          "ITS Assume-Aug 2022",
                                          "ITS Assume-Feb 2023",
                                          "ITS Assume-May 2023",
                                          "ITS Assume-Dec 2023"))

# reorder factors
results_clean <- results_clean %>%
  mutate(covid_scenario = ifelse(covid_end_true_ctrl < covid_end_true_trt,
                                 "Control First",
                                 "Treatment First"))

save(results_clean, file="results/results_clean_diffrtn_2026-03-16.rda")

# plot scenarios diff RTN
load("results/simulated_data_diffrtn_2026-03-16.rda")

diff_rtn_example <- simulated %>%
  filter((
    policy_start == as.yearmon("Jun 2018") |
      policy_start == as.yearmon("Sep 2021")
  ) &
    policy_intercept == 2.5 &
    policy_slope == -0.25 & 
    rep == 1
  )

diff_rtn_example$which_first <- factor(diff_rtn_example$which_first, 
                                  levels = c("ctrl", "trt"),
                                  labels = c("Control RTN Earlier",
                                             "Treatment RTN Earlier"))

ggplot(diff_rtn_example) +
  facet_grid(policy_start ~ which_first) + 
  geom_line(aes(x = yearmon, y = outcome, group = treated, color = as.factor(treated))) +
  theme_bw() +
  labs(y = "Outcome", x = "", color="Group") + 
  scale_color_discrete(labels = c("Control", "Treatment")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(aes(xintercept = as.yearmon("Feb 2021")), linewidth=0.2, linetype="dashed") +
  geom_vline(aes(xintercept = as.yearmon("Feb 2022")), linewidth=0.2, linetype="dashed")
ggsave("figures/10-suppfig-scen-diffrtn.png", width=2500, height=2000, units="px")
ggsave("figures/10-suppfig-scen-diffrtn.pdf", width=2500, height=2000, units="px")

