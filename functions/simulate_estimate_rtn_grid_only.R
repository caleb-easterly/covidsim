#' Function to simulate and then estimate ONLY RTN. 
#' This is to establish the best threshold for calling the return to normal date.
#' 
#' Differences:
#' - don't simulate treated group
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
source("functions/estimate_all.R")

simulate_estimate_rtn_grid_only <- function(scenario,
                                            rep,
                                            parameters,
                                            covid_scenarios,
                                            policy_scenarios,
                                            fig = 0) {
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
  
  # only need treatment group
  simulated_data_treatment <- simulate_covid(
    beta0 = beta0, 
    beta1 = beta1, 
    beta2 = beta2, 
    beta3 = beta3, 
    gamma0 = policy_intercept,
    gamma1 = policy_slope,
    covid_end = covid_end,
    nonlin = nonlin,
    policy_start = policy_start_date,
    rho = 0, wnvar = 1)
  simulated_data_treatment <- simulated_data_treatment %>%
    mutate(scenario = scenario,
           rep = rep,
           treated = 0,
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
  rtn_output <- estimate_rtn_grid(simulated_data_treatment)
  covid_end_true <- convert_yearmon(covid_end[1], covid_end[2])
  covid_end_index <- which.max(simulated_data_treatment$yearmon == covid_end_true)
  rtn_output <- c(
    rtn_output,
    "scenario" = scenario,
    "rep" = rep,
    "covid_end_year" = covid_end[1],
    "covid_end_month" = covid_end[2],
    "covid_end_index" = covid_end_index
  )
  
  # add yearmon form - create column with right format and then replace
  # rtn_output$true_covid_end_yearmon <- as.yearmon("2015-10-01")
  # rtn_output$true_covid_end_yearmon <- convert_yearmon(covid_end[1], covid_end[2])

  ######### RESULTS ######### 
  # return data
  return(list("simulated" = simulated_data_treatment, "rtn_output" = rtn_output))
}