library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_all_methods_2025-08-28.rda")
# exclude SER start dates
results_clean <- results_all_methods %>%
  filter(policy_start_date != as.yearmon("Jul 2021") & 
           policy_start_date != as.yearmon("May 2019")) %>%
  # prettify names of methods
  mutate(method = 
           case_match(
             method,
             "did" ~ "DID",
             "lm_rtn1" ~ "ITS RTN 1mo",
             "lm_rtn2" ~ "ITS RTN 2mo",
             "lm_rtn3" ~ "ITS RTN 3mo",
             "lm_rtn4" ~ "ITS RTN 4mo",
             "lm_rtn5" ~ "ITS RTN 5mo",
             "lm_rtn6" ~ "ITS RTN 6mo",
             "lm_rtn7" ~ "ITS RTN 7mo",
             "lm_rtn8" ~ "ITS RTN 8mo",
             "lm_rtn9" ~ "ITS RTN 9mo",
             "lm_rtn10" ~ "ITS RTN 10mo",
             "lm_rtn11" ~ "ITS RTN 11mo",
             "lm_rtn12" ~ "ITS RTN 12mo",
             "lm_assume" ~ "ITS Assume",
             "lm_sahwash" ~ "ITS SAH-Ind",
             "lm_twoseg" ~ "ITS SAH-Drop",
             .default = method)
  ) %>%
  mutate(method = ifelse(method == "ITS Assume",
                         paste(method, assumed_rtn_date, sep="-"),
                         method))

results_clean$method <- factor(results_clean$method,
                               levels = c("DID",
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
                                          "ITS RTN 12mo",
                                          "ITS SAH-Drop",
                                          "ITS SAH-Ind",
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

save(results_clean, file="results/results_clean_2025-08-28.rda")
