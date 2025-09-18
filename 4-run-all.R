# packages
library(tidyverse)
library(parallel)
library(sandwich)
library(zoo)
library(rlang)

# functions
source("functions/utilities.R")
source("functions/estimate_all.R")
source("functions/simulate_estimate.R")
source("functions/simulate_covid.R")

# we have 2340 scenarios and 1000 reps per scenario (these variables are defined in 1-define-parameters.R)
variations <- expand.grid(1:nscenarios, 1:nreps, stringsAsFactors = FALSE)
colnames(variations) <- c("scenario", "rep")
variations$num <- as.numeric(rownames(variations))

# estimate for one scenario
par_estimate_all <- function(i, variations) {
  s <- variations$scenario[i]
  r <- variations$rep[i]
  print(paste0("Scenario: ", s))
  print(paste0("Rep: ", r))
  simulate_estimate(scenario = s, rep = r, parameters, covid_scenarios, policy_scenarios)
}

# non-parallel version - used for testing
# simulated_and_estimated <- lapply(X = 1:nrow(variations),
#                        FUN = function(x) par_estimate_all(x, variations))

## parallel version
# make cluster
# 8.18.2025 - 16 clusters was causing issues, so decrease to 8 cores
cl <- makeCluster(8, type = "FORK")

# distribute streams to the cluster members
RNGkind("L'Ecuyer-CMRG")
clusterSetRNGStream(cl, iseed = 65156114)

# simulate and estimate across all variations
simulated_and_estimated <- parLapply(cl, X = 1:nrow(variations),
                       fun = function(x) par_estimate_all(x, variations))
stopCluster(cl)

# unpack simulated vs. estimated
## simulated data
simulated <- bind_rows(lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$simulated)) %>%
  dplyr::relocate(scenario, rep)
save(simulated, file=paste0("results/simulated_data_", today(), ".rda"))

## estimated parameters
estimated <- lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$estimated)
results_all_methods <- bind_rows(estimated)

save(results_all_methods, file=paste0("results/results_all_methods_", today(), ".rda"))
