library(tidyverse)
library(reshape2)
source("1-define-parameters.R")

load("results/results_rtn_boot_op_2026-09-18.rda")

results_clean <- results_rtn_boot %>%
  # prettify names of methods
  mutate(method = 
           case_match(
             method,
             "model" ~ "One Step",
             "boot" ~ "Bootstrapped",
             .default = method)
  )

results_clean$method <- factor(results_clean$method,
                               levels = c("One Step",
                                          "Bootstrapped"))

# covid scenarios
covid_parms <- sapply(parameters$covid_parms, function(i) covid_scenarios[[i]]$name)
covid_scenarios <- data.frame(covid_scenario = covid_parms) %>%
  mutate(scenario = row_number())

# join to big dataset
results_clean <- left_join(results_clean, covid_scenarios, by = "scenario")

save(results_clean, file="results/results_clean_rtn_boot-09-17.rda")
