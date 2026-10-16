library(tidyverse)
library(reshape2)
source("1-define-parameters.R")

parameters_short <- parameters %>%
  filter((intercept == 2.5 & slope == 0.25) |
           (intercept == 0 & slope == 0)) %>%
  filter(policy_start >= 6 & policy_start <= 13) %>%
  filter(covid_parms <= 4)

load("results/results_short_series_2026-09-13.rda")
results_clean <- results_short_series %>%
  # prettify names of methods
  mutate(method = 
           case_match(
             method,
             "did" ~ "CITS",
             "lm_assume" ~ "ITS Assume",
             "lm_rtn_grid" ~ "ITS-Grid Search",
             .default = method)
  ) %>%
  mutate(method = ifelse(method == "ITS Assume",
                         paste(method, assumed_rtn_date, sep="-"),
                         method))

results_clean$method <- factor(results_clean$method,
                               levels = c("CITS",
                                          "ITS-Grid Search",
                                          "ITS Assume-May 2020",
                                          "ITS Assume-Aug 2020",
                                          "ITS Assume-Feb 2021",
                                          "ITS Assume-Aug 2021"))

# covid scenarios
covid_parms <- sapply(parameters_short$covid_parms, function(i) covid_scenarios[[i]]$name)
covid_scenarios <- data.frame(covid_scenario = covid_parms) %>%
  mutate(scenario = row_number())

# join to big dataset
results_clean <- left_join(results_clean, covid_scenarios, by = "scenario")

# reorder factors
results_clean$covid_scenario <- factor(results_clean$covid_scenario,
                                       levels = c("Linear, 3 months",
                                                  "Linear, 6 months",
                                                  "Linear, 1 year",
                                                  "Linear, 18 months"),
                                       labels = c("Lin. 3mo",
                                                  "Lin. 6mo",
                                                  "Lin. 1yr",
                                                  "Lin. 18mo"))

save(results_clean, file="results/results_clean_short_series_2026-09-13.rda")
