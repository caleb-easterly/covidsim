#' Estimate a difference-in-differences
#' linear model
#' 
#' The input data needs to be one scenario and rep and have both treatment and control groups
#' (e.g., should have 198 observations)
#' 
#' df: simulated data
#' 

# make sure utility functions are available
source("functions/utilities.R")

estimate_did <- function(df) {
  ## estimate using assumed COVID period
  mod <- lm(outcome ~ treated*(time + policy_ind + policy_time),
            data=df)
  model_summary <- summary(mod)
  
  # newey-west 
  nw_vcov <- sqrt(diag(NeweyWest(mod, prewhite = FALSE, adjust = TRUE, lag = 1)))[7:8]
  
  # make vector - coefficient estimates (4) followed by SEs (4)
  estimates_vector <- c(model_summary$coefficients[
    c("treated:policy_indTRUE", "treated:policy_time"),
    c("Estimate", "Std. Error")],
    nw_vcov)
  
  names(estimates_vector) <- c(
    "policy_intercept_b", "policy_slope_b",
    "policy_intercept_se", "policy_slope_se",
    "policy_intercept_nwse", "policy_slope_nwse")
  
  did_df <- as.data.frame(t(estimates_vector))
  did_df$method <- "did"
  did_df
}
