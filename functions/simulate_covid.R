library(dplyr)
library(zoo)

simulate_covid <- function(
    rho=0.1, wnvar=1,
    alpha0 = 0, alpha1 = 0,
    beta0 = -10, beta1 = 10/3,
    beta2 = 0, beta3 = 0,
    gamma0 = 0, gamma1 = 0,
    covid_end = c(2020, 5),
    nonlin = FALSE,
    policy_start = c(2021, 7),
    policy_end = c(2023, 12)
) {
  ### COVID parameters
  covid_start <- c(2020, 3)
  
  # start for the post-PHE period (only used in decay model)
  covid_delay_start <- c(2020, 6)
  
  simd <- ts(NA, 
             start = c(2015, 10),
             end = c(2023, 12),
             frequency = 12)
  sim_length <- length(simd)
  sim_times <- time(simd)
  
  sim_error <- ts(NA, 
                  start = c(2015, 10),
                  end = c(2023, 12),
                  frequency = 12)
  
  # COVID-19 period
  covid_ind <- 
    sim_times %in% 
    time(window(simd, start = covid_start, end = covid_end))
  
  # post-PHE COVID-19 period for decay model
  if (all(covid_end == c(2023, 5)) & nonlin == TRUE) {
    covid_ind_postphe <- 
      sim_times %in%
      time(window(simd, start = covid_delay_start, end = covid_end))
  } else {
    covid_ind_postphe <- covid_ind
  }
  
  ## vector index at which covid starts
  covid_start_ind <- min(which(covid_ind))
  covid_start_ind_postphe <- min(which(covid_ind_postphe))
  
  # policy in july 2021
  policy_ind <- 
    sim_times %in% 
    time(window(simd, start = policy_start, end = policy_end))
  
  ## vector index at which policy starts
  policy_start_ind <- min(which(policy_ind))
  
  ## simulate
  # first observation - just white noise
  # per turner, multiply by factor to make 
  # variance of error term constant across all time
  sim_error[1] <- rnorm(1, mean=0, sd = sqrt(wnvar)) * sqrt(1 / (1 - rho^2))
  simd[1] <- sim_error[1]
  
  # simulate
  for (t in 2:sim_length) {
    # nonlinear multiplier for beta2
    ## between march 2020 and june 2020 
    if (covid_ind[t] == 1 & covid_ind_postphe[t] == 0) {
      decay_mult <- 1
    }
    ## after june 2020 
    if (covid_ind[t] == 1 & covid_ind_postphe[t] == 1) {
      decay_mult <- exp(-beta3*(t-covid_start_ind_postphe))
    }
    ## other cases - COVID over
    if (covid_ind[t] == 0 & covid_ind_postphe[t] == 0) {
      decay_mult <- 0
    }
    sim_error[t] <- rho*sim_error[t-1] + rnorm(1, mean=0, sd = sqrt(wnvar))
    simd[t] <- alpha0 + alpha1*t +
      covid_ind[t]*
      (beta0 + beta1*(t - covid_start_ind) + decay_mult * beta2) +
      policy_ind[t]*(gamma0 + gamma1*(t - policy_start_ind)) + 
      sim_error[t]
  }
  
  # estimate
  df <- data.frame(time = 1:sim_length,
                   yearmon = zoo::as.yearmon(time(simd)),
                   outcome = simd,
                   covid_ind,
                   covid_start_ind,
                   policy_ind,
                   policy_start_ind) %>%
    mutate(covid_time = (time - covid_start_ind)*covid_ind,
           policy_time = (time - policy_start_ind)*policy_ind)
  
  return(df)
}
