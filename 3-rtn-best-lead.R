# packages
library(tidyverse)
library(parallel)
library(sandwich)
library(zoo)
library(rlang)

# parameters
load("data/sim_parameters.rda")

# functions
source("functions/utilities.R")
source("functions/estimate_all.R")
source("functions/simulate_estimate_rtn_only.R")
source("functions/simulate_covid.R")

# only including null policy effects
nreps <- 1000
nscenarios <- 10

# parameters - only need to loop over the 10 COVID scenarios
parameters_best_lead <- parameters %>%
  filter(intercept == 0 & slope == 0 & policy_start == 1)
variations <- expand.grid(1:nscenarios, 1:nreps, stringsAsFactors = FALSE)
colnames(variations) <- c("scenario", "rep")
variations$num <- as.numeric(rownames(variations))

# estimate for one scenario
par_estimate_all <- function(i, variations) {
  s <- variations$scenario[i]
  r <- variations$rep[i]
  print(paste0("Scenario: ", s))
  print(paste0("Rep: ", r))
  fig <- r == 1
  simulate_estimate_rtn_only(scenario = s, rep = r, parameters_best_lead, covid_scenarios, policy_scenarios, fig)
}

# non-parallel version - used for testing
simulated_and_estimated <- lapply(X = 1:nrow(variations),
                       FUN = function(x) par_estimate_all(x, variations))

# # make cluster
# cl <- makeCluster(8, type = "FORK")
# 
# # distribute streams to the cluster members
# RNGkind("L'Ecuyer-CMRG")
# clusterSetRNGStream(cl, iseed = 65156114)
# 
# # simulate and estimate across all variations
# simulated_and_estimated <- parLapply(cl, X = 1:nrow(variations),
#                        fun = function(x) par_estimate_all(x, variations))
# stopCluster(cl)

# unpack simulated vs. estimated
## simulated data
simulated <- bind_rows(lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$simulated)) %>%
  dplyr::relocate(scenario, rep)
save(simulated, file=paste0("results/simulated_data_rtn_only_", today(), ".rda"))

## estimated parameters
estimated <- lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$estimated)
results_rtn_only <- bind_rows(estimated)

save(results_rtn_only, file=paste0("results/results_rtn_only_", today(), ".rda"))
