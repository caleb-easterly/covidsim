# packages
library(tidyverse)
library(parallel)
library(sandwich)
library(zoo)
library(rlang)

# define parameters
source("1-define-parameters.R")

# functions
source("functions/utilities.R")
source("functions/estimate_all.R")
source("functions/simulate_estimate.R")
source("functions/simulate_covid.R")

# parameters - short series - only default policy intercept and slope
# also filter policy start dates to be 6/2019 to 6/2021 
policy_scenarios[[6]]
policy_scenarios[[13]]

# and filter so the last COVID end date is 8/2021
covid_end_dates[[4]]
covid_scenarios <- covid_scenarios[1:4]
covid_end_dates <- covid_end_dates[1:4]

parameters_short <- parameters %>%
  filter((intercept == 2.5 & slope == 0.25) |
          (intercept == 0 & slope == 0)) %>%
  filter(policy_start >= 6 & policy_start <= 13) %>%
  filter(covid_parms <= 4)
nscenarios <- 64
nreps <- 500

# define grid of parameter variations (these variables are defined in 1-define-parameters.R)
variations <- expand.grid(1:nscenarios, 1:nreps, stringsAsFactors = FALSE)
colnames(variations) <- c("scenario", "rep")
variations$num <- as.numeric(rownames(variations))

# estimate for one scenario
par_estimate_all <- function(i, variations) {
  s <- variations$scenario[i]
  r <- variations$rep[i]
  print(paste0("Scenario: ", s))
  print(paste0("Rep: ", r))
  simulate_estimate(scenario = s, 
                    rep = r, 
                    parameters_short, 
                    covid_scenarios,
                    policy_scenarios,
                    sim_start = c(2018, 1),
                    sim_end = c(2021, 12))
}

# non-parallel version - used for testing
# simulated_and_estimated <- lapply(X = 1:nrow(variations),
#                        FUN = function(x) par_estimate_all(x, variations))

## parallel version
# make cluster
cl <- makeCluster(8, type = "FORK")

# distribute streams to the cluster members
RNGkind("L'Ecuyer-CMRG")
clusterSetRNGStream(cl, iseed = 65156114)

# simulate and estimate across all variations
simulated_and_estimated <- parLapply(cl, X = 1:nrow(variations),
                                     fun = function(x) par_estimate_all(x, variations))
stopCluster(cl)

# unpack simulated vs. estimated
# simulated data
simulated_short_series <- bind_rows(lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$simulated)) %>%
  dplyr::relocate(scenario, rep)
save(simulated_short_series, file=paste0("results/simulated_data_short_series_", today(), ".rda"))

## estimated parameters
estimated <- lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$estimated)
results_short_series <- bind_rows(estimated)

save(results_short_series, file=paste0("results/results_short_series_", today(), ".rda"))
