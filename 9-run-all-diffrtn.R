# clear all
rm(list = ls())

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
source("functions/simulate_estimate_diffrtn.R")

# only take first COVID scenario - these parameters are overwritten by the diffrtn function
parameters_diffrtn <- parameters %>%
  filter(covid_parms == 1)
nscenarios <- 208
nreps <- 500

# define grid of parameter variations (these variables are defined in 1-define-parameters.R)
variations <- expand.grid(1:nscenarios, 1:nreps, stringsAsFactors = FALSE)
colnames(variations) <- c("scenario", "rep")
variations$num <- as.numeric(rownames(variations))

# estimate for one scenario
par_estimate_diffrtn <- function(i,
                                 variations,
                                 which.first = c("ctrl", "trt")) {
  s <- variations$scenario[i]
  r <- variations$rep[i]
  print(paste0("Scenario: ", s))
  print(paste0("Rep: ", r))
  simulate_estimate_diffrtn(
    scenario = s,
    rep = r,
    parameters_diffrtn,
    covid_scenarios,
    policy_scenarios,
    which.first = which.first
  )
}

# non-parallel version - used for testing
# simulated_and_estimated_control <- lapply(X = 1:nrow(variations),
#                        FUN = function(x) par_estimate_diffrtn(x, variations, "ctrl"))
# simulated_and_estimated_treatment <- lapply(X = 1:nrow(variations),
#                                           FUN = function(x) par_estimate_diffrtn(x, variations, "trt"))

## parallel version
# make cluster
# 8.18.2025 - 16 clusters was causing issues, so decrease to 8 cores
# 3.15.2025 - temporarily increase to 16 cores
cl <- makeCluster(8, type = "FORK")

# distribute streams to the cluster members
RNGkind("L'Ecuyer-CMRG")
clusterSetRNGStream(cl, iseed = 65156114)

# simulate and estimate across all variations
simulated_and_estimated_control <- parLapply(
  cl,
  X = 1:nrow(variations),
  fun = function(x)
    par_estimate_diffrtn(x, variations, "ctrl")
)
simulated_and_estimated_treatment <- parLapply(
  cl,
  X = 1:nrow(variations),
  fun = function(x)
    par_estimate_diffrtn(x, variations, "trt")
)
stopCluster(cl)

# unpack simulated vs. estimated
# simulated data
simulated_control <- bind_rows(lapply(
  X = 1:nrow(variations),
  FUN = function(x)
    simulated_and_estimated_control[[x]]$simulated
)) %>%
  dplyr::relocate(scenario, rep)
simulated_treatment <- bind_rows(lapply(
  X = 1:nrow(variations),
  FUN = function(x)
    simulated_and_estimated_treatment[[x]]$simulated
)) %>%
  dplyr::relocate(scenario, rep)
simulated <- bind_rows(simulated_control, simulated_treatment)
save(simulated,
     file = paste0("results/simulated_data_diffrtn_", today(), ".rda"))

## estimated parameters
estimated_control <- lapply(
  X = 1:nrow(variations),
  FUN = function(x)
    simulated_and_estimated_control[[x]]$estimated
)
estimated_treatment <- lapply(
  X = 1:nrow(variations),
  FUN = function(x)
    simulated_and_estimated_treatment[[x]]$estimated
)
results_all_methods <- bind_rows(estimated_control, estimated_treatment)
save(
  results_all_methods,
  file = paste0("results/results_all_methods_diffrtn_", today(), ".rda")
)
