#' Estimate a interrupted time series 
#' linear model based on an assumed (or estimated) end date for COVID
#' 
#' The input data needs to be one scenario/rep/treatment status (e.g., have 99 observations)
#' 
#' df: simulated data
#' covid_end_est: COVID period to use to estimate the model 
#'  (not necessarily the true COVID period)
#' 
#' Changelog
#'  2025.05.30 - CWE
#'    - branched off of estimate_lm_twoseg
#' 

# make sure utility functions are available
source("functions/utilities.R")

estimate_lm_sahwash <- function(df) {
  covid_start <- c(2020, 3)
  
  # get estimated/assumed COVID-19 period into the data
  simd <- ts(NA, 
             start = c(2015, 10),
             end = c(2023, 12),
             frequency = 12)
  sim_length <- length(simd)
  sim_times <- time(simd)
  
  # indicator for march - may 2020
  df$covid_sah <- sim_times %in% 
    time(window(simd, start = c(2020, 3), end = c(2020, 5)))
  
  # indicator variable for june 2020 to end of the data
  covid_ind <- sim_times %in%
    time(window(simd, start = c(2020, 6), end = c(2023, 12)))

  ## indicator variable with on/off for COVID
  df$covid_ind_est <- covid_ind

  ## constant variable holding the index at which COVID starts
  df$covid_start_ind_est <- min(which(df$covid_ind_est))

  ## COVID time based on estimated/assumed COVID period
  df <- df %>%
    mutate(covid_time_est = (time - covid_start_ind_est)*covid_ind_est)

  ## estimate
  ### note that covid_sah is at the end to maintain the ordering of the coefficients
  mod <- lm(outcome ~ time +
              covid_ind_est + covid_time_est +
              policy_ind + policy_time + covid_sah,
            data=df)
  model_summary <- summary(mod)
  
  ## newey west standard errors - commented out for now
  nw_vcov <- sqrt(diag(NeweyWest(mod, prewhite = FALSE, adjust = TRUE, lag = 1)))[3:6]

  # make vector - coefficient estimates (4) followed by SEs (4)
  estimates_vector <- c(model_summary$coefficients[
    c("covid_ind_estTRUE", "covid_time_est", "policy_indTRUE", "policy_time"),
    c("Estimate", "Std. Error")],
    nw_vcov)

  names(estimates_vector) <- c(
    "covid_intercept_b", "covid_slope_b",
    "policy_intercept_b", "policy_slope_b",
    "covid_intercept_se", "covid_slope_se",
    "policy_intercept_se", "policy_slope_se",
    "covid_intercept_nwse", "covid_slope_nwse",
    "policy_intercept_nwse", "policy_slope_nwse"
  )
  est_df <- as.data.frame(t(estimates_vector))
  est_df$method <- "lm_sahwash"

  ## add COVID date
  covid_end_est <- c(2023, 12)
  est_df$assumed_rtn_date <- convert_yearmon(covid_end_est[1], covid_end_est[2])
  est_df
}
