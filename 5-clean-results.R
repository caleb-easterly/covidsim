library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_all_methods_2026-03-16.rda")
results_clean <- results_all_methods %>%
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
                                          "ITS Assume-Aug 2021",
                                          "ITS Assume-Feb 2022",
                                          "ITS Assume-Aug 2022",
                                          "ITS Assume-Feb 2023",
                                          "ITS Assume-May 2023",
                                          "ITS Assume-Dec 2023"))

# covid scenarios
covid_parms <- sapply(parameters$covid_parms, function(i) covid_scenarios[[i]]$name)
covid_scenarios <- data.frame(covid_scenario = covid_parms) %>%
  mutate(scenario = row_number())

# join to big dataset
results_clean <- left_join(results_clean, covid_scenarios, by = "scenario")

# reorder factors
results_clean$covid_scenario <- factor(results_clean$covid_scenario,
                                       levels = c("Linear, 3 months",
                                                  "Linear, 6 months",
                                                  "Linear, 1 year",
                                                  "Linear, 18 months",
                                                  "Linear, 2 years",
                                                  "Linear, 30 months",
                                                  "Linear, 36 months",
                                                  "Linear, 39 months",
                                                  "Permanent, no slope",
                                                  "Nonlinear"),
                                       labels = c("Lin. 3mo",
                                                  "Lin. 6mo",
                                                  "Lin. 1yr",
                                                  "Lin. 18mo",
                                                  "Lin. 2yr",
                                                  "Lin. 30mo",
                                                  "Lin. 36mo",
                                                  "Lin. 39mo",
                                                  "Permanent",
                                                  "Nonlinear"))

save(results_clean, file="results/results_clean_2026-03-16.rda")
