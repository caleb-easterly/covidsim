#' Function to simulate and then estimate different ITS and DID models.
#' In the final lapply call, we will loop over a matrix of combinations of scenarios and reps.
#' 
#' Arguments:
#' - scenario: numeric index for parameter scenario
#' - rep: numeric index for the replicate within the scenario 
#' - parameters: list of parameters, unpacked within the function based on the scenario
#' - covid_scenarios: list of COVID scenarios. 
#' - policy_scenarios: list of policy scenarios (policy start dates)
#' 
#' Output: List of data frames. The first has the simulated data (N=198 observations) and the second has the parameter estimates from each method
#' 
#' 
source("functions/estimate_did.R")
source("functions/estimate_lm.R")
source("functions/estimate_rtn.R")
source("functions/utilities.R")
source("functions/estimate_rtn_grid_boot.R")

simulate_estimate_rtn_grid_boot <- function(scenario, 
                                            rep, 
                                            parameters, 
                                            covid_scenarios, 
                                            policy_scenarios,
                                            sim_start = c(2015, 10),
                                            sim_end = c(2023, 12),
                                            nboot = 500,
                                            L = 10) {
  
  ######### SIMULATE ######### 
  parms <- parameters[scenario, ]
  
  # covid parameters
  covid_parms <- covid_scenarios[[parms$covid_parms]]
  covid_end <- covid_parms$covid_end
  covid_end_df <- convert_yearmon(covid_end[1], covid_end[2])
  beta0 <- covid_parms$beta0
  beta1 <- covid_parms$beta1
  beta2 <- covid_parms$beta2
  beta3 <- covid_parms$beta3
  nonlin <- covid_parms$nonlin
  
  # policy parameters
  policy_start_date <- policy_scenarios[[parms$policy_start]]
  policy_intercept <- parms$intercept
  policy_slope <- parms$slope
  
  policy_begin_ym <- convert_yearmon(policy_start_date[1], policy_start_date[2])
  
  simulated_data_treated <- simulate_covid(
    beta0 = beta0, 
    beta1 = beta1, 
    beta2 = beta2, 
    beta3 = beta3, 
    gamma0 = policy_intercept,
    gamma1 = policy_slope,
    covid_end = covid_end,
    nonlin = nonlin,
    policy_start = policy_start_date,
    rho = 0, wnvar = 1,
    sim_start = sim_start,
    sim_end = sim_end)
  simulated_data_treated <- simulated_data_treated %>%
    mutate(scenario = scenario,
           rep = rep,
           treated = 1,
           beta0 = beta0,
           beta1 = beta1,
           beta2 = beta2,
           beta3 = beta3,
           policy_intercept = policy_intercept,
           policy_slope = policy_slope,
           covid_end = covid_end_df,
           nonlin = nonlin,
           policy_start = policy_begin_ym)

  # burn a few simulations (turner)
  burn <- rnorm(300)
  
  ######### ESTIMATE ######### 
  estimated <- estimate_rtn_grid_boot(
    simulated_data_treated,
    nboot, 
    L) %>%
    mutate(scenario = scenario,
           rep = rep,
           policy_start_date = policy_begin_ym,
           policy_intercept = policy_intercept,
           policy_slope = policy_slope,
           beta0 = beta0,
           beta1 = beta1,
           beta2 = beta2,
           beta3 = beta3,
           covid_end_true = covid_end_df)

  ######### RESULTS ######### 
  # return data
  return(list("simulated" = simulated_data_treated, "estimated" = estimated))
}
