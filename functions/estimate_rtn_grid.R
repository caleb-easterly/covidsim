# Estimate the return to normal date for a competing event in an ITS model using a grid search
# 
estimate_rtn_grid <- function(df){
  sim_length <- nrow(df)
  # get covid start date from column (whole column is the same, so take the first)
  covid_start_date <- first(df$covid_start_ind)
  results <- matrix(NA, nrow = sim_length, ncol = 4)
  colnames(results) <- c("policy_intercept_b",
                         "policy_intercept_se",
                         "policy_slope_b",
                         "policy_slope_se")
  ssr <- rep(NA, sim_length)
  for (i in covid_start_date:sim_length) {
    # define COVID period for this iteration
    df$covid_assume <- df$time >= covid_start_date & df$time <= i
    # define COVID time trend for this iteration
    df$covid_time_est <- (df$time - df$covid_start_ind) * df$covid_assume
    # estimate model
    mod <- lm(outcome ~ time +
                covid_assume + covid_time_est +
                policy_ind + policy_time,
              data = df)
    # sum up squared residuals
    ssr[i] <- sum(mod$residuals^2)
    # extract coefficients 
    coefs <- summary(mod)$coefficients
    results[i, "policy_intercept_b"] <- coefs["policy_indTRUE", "Estimate"]
    results[i, "policy_intercept_se"] <- coefs["policy_indTRUE", "Std. Error"]
    results[i, "policy_slope_b"] <- coefs["policy_time", "Estimate"]
    results[i, "policy_slope_se"] <- coefs["policy_time", "Std. Error"]
  }
  # to find the best fitting model, find the row index (e.g. time) with minimum ssr
  # if there are ties, take later RTN date
  min_ssr <- min(ssr, na.rm = TRUE)
  date_minimize <- max(which(abs(ssr - min_ssr) < 1e-6))
  
  # extract matrix row and convert to data frame
  est_df <- as.data.frame(t(results[date_minimize, ]))
  est_df$assumed_rtn_date <- df$yearmon[date_minimize]
  est_df$method <- "lm_rtn_grid"
  est_df
}
