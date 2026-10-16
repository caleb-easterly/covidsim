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
source("functions/simulate_estimate_rtn_grid_boot.R")
source("functions/simulate_covid.R")

# only default policy intercept and slope and default policy start date
parameters <- parameters %>%
  filter((intercept == 2.5 & slope == 0.25)) %>%
  filter(policy_start == 13) %>% # policy start date of June 2021 %>%
  filter(covid_parms == 3) # COVID one year long
nscenarios <- 1
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
  simulate_estimate_rtn_grid_boot(scenario = s, 
                    rep = r, 
                    parameters, 
                    covid_scenarios,
                    policy_scenarios,
                    nboot = 1000)
}

# non-parallel version - used for testing
# simulated_and_estimated <- lapply(X = 1:nrow(variations),
#                        FUN = function(x) par_estimate_all(x, variations))

# ## parallel version
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
simulated_rtn_boot <- bind_rows(lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$simulated)) %>%
  dplyr::relocate(scenario, rep)
save(simulated_rtn_boot, file=paste0("results/simulated_data_rtn_boot_op_", today(), ".rda"))

## estimated parameters
estimated <- lapply(X = 1:nrow(variations), FUN = function(x) simulated_and_estimated[[x]]$estimated)
results_rtn_boot <- bind_rows(estimated)

save(results_rtn_boot, file=paste0("results/results_rtn_boot_op_", today(), ".rda"))
