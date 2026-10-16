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
source("functions/estimate_all.R")

simulate_estimate_diffrtn <- function(scenario,
                              rep,
                              parameters,
                              covid_scenarios,
                              policy_scenarios,
                              which.first = c("ctrl", "trt")) {
  ######### ARGUMENTS #########
  which.first <- match.arg(which.first)
  
  ######### SIMULATE ######### 
  parms <- parameters[scenario, ]
  
  # covid parameters - these are hard-coded in this application
  if (which.first == "ctrl") {
    ## control COVID parameters
    covid_end_ctrl <- c(2021, 2)
    beta1_ctrl <- 10/12
    
    ## treatment COVID parameters
    covid_end_trt <- c(2022, 2)
    beta1_trt <- 10/24
  }
  if (which.first == "trt") {
    ## control COVID parameters
    covid_end_ctrl <- c(2022, 2)
    beta1_ctrl <- 10/24
    
    ## treatment COVID parameters
    covid_end_trt <- c(2021, 2)
    beta1_trt <- 10/12
  }
  
  ## shared
  beta0 <- -10
  beta2 <- 0
  beta3 <- 0
  nonlin <- FALSE
  
  ## for output
  covid_end_ctrl_df <- convert_yearmon(covid_end_ctrl[1], covid_end_ctrl[2])
  covid_end_trt_df <- convert_yearmon(covid_end_trt[1], covid_end_trt[2])

  # policy parameters
  policy_start_date <- policy_scenarios[[parms$policy_start]]
  policy_intercept <- parms$intercept
  policy_slope <- parms$slope
  
  policy_begin_ym <- convert_yearmon(policy_start_date[1], policy_start_date[2])
  
  simulated_data_treated <- simulate_covid(
    beta0 = beta0, 
    beta1 = beta1_trt, 
    beta2 = beta2, 
    beta3 = beta3, 
    gamma0 = policy_intercept,
    gamma1 = policy_slope,
    covid_end = covid_end_trt,
    nonlin = nonlin,
    policy_start = policy_start_date,
    rho = 0, wnvar = 1)
  simulated_data_treated <- simulated_data_treated %>%
    mutate(scenario = scenario,
           rep = rep,
           treated = 1,
           beta0 = beta0,
           beta1 = beta1_trt,
           beta2 = beta2,
           beta3 = beta3,
           policy_intercept = policy_intercept,
           policy_slope = policy_slope,
           covid_end = covid_end_trt_df,
           nonlin = nonlin,
           policy_start = policy_begin_ym,
           which_first = which.first)
  burn <- rnorm(300)
  
  simulated_data_control <- simulate_covid(
    beta0 = beta0, 
    beta1 = beta1_ctrl, 
    beta2 = beta2, 
    beta3 = beta3, 
    gamma0 = 0,
    gamma1 = 0,
    covid_end = covid_end_ctrl,
    nonlin = nonlin,
    policy_start = policy_start_date,
    rho = 0, wnvar = 1)
  simulated_data_control <- simulated_data_control %>%
    mutate(scenario = scenario,
           rep = rep,
           treated = 0,
           beta0 = beta0,
           beta1 = beta1_ctrl,
           beta2 = beta2,
           beta3 = beta3,
           policy_intercept = policy_intercept,
           policy_slope = policy_slope,
           covid_end = covid_end_ctrl_df,
           nonlin = nonlin,
           policy_start = policy_begin_ym,
           which_first = which.first)

  # combine treated and control groups
  simulated_data <- bind_rows(simulated_data_treated, simulated_data_control)
  
  # burn a few simulations (turner)
  burn <- rnorm(300)
  
  ######### ESTIMATE ######### 
  estimated <- estimate_all(simulated_data) %>%
    mutate(scenario = scenario,
           rep = rep,
           policy_start_date = policy_begin_ym,
           policy_intercept = policy_intercept,
           policy_slope = policy_slope,
           beta0 = beta0,
           beta1_ctrl = beta1_ctrl,
           beta1_trt = beta1_trt,
           beta2 = beta2,
           beta3 = beta3,
           covid_end_true_ctrl = covid_end_ctrl_df,
           covid_end_true_trt = covid_end_trt_df,
           which_first = which.first)

  ######### RESULTS ######### 
  # return data
  return(list("simulated" = simulated_data, "estimated" = estimated))
}