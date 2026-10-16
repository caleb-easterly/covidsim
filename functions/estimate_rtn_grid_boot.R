# Estimate the return to normal date with bootstrapping
# Process: 
# 1. Conduct grid search to determine the best-fitting COVID-19 period length 
# 2. Estimate model with best-fitting COVID-19 period length
# 3. Create fitted time series and calculate residuals
# 4. For k in 1 to nbootstraps: 
#   a. Resample residuals using block bootstrapping 
#   b. Add resampled residuals back to fitted series 
#   c. Conduct grid search to determine the best-fitting COVID-19 period 
#   d. Estimate model with best-fitting COVID-19 period length 
#   e. Save adjusted estimate of intercept change and slope change 
# 5. Save bootstrapped estimates as the 
# mean of bootstrapped replicates and the 
# 95% CI as the 2.5th and 97.5th percentiles of the estimates.
# 6. For type 1 error and power calculations, 
# reject the null if 0 is not included in the 95% CI 
estimate_rtn_grid_boot <- function(df, nboot, L){
  sim_length <- nrow(df)
  
  # copy original outcome values to store for later
  df$outcome_orig <- df$outcome
  
  # find best-fitting COVID date
  best_date <- grid_search_internal(df)
  
  # estimate model with best-fitting COVID date
  mod <- estimate_model_covid_date(best_date, df)
  
  # extract model-based estimates and CIs
  coefs <- summary(mod)$coef[c("policy_indTRUE", "policy_time"), c("Estimate", "Std. Error")]
  
  m_int_est <- coefs["policy_indTRUE", "Estimate"]
  m_int_se <- coefs["policy_indTRUE", "Std. Error"]
  m_int_ci <- m_int_est + c(-1.96*m_int_se, 1.96*m_int_se)
  m_slope_est <- coefs["policy_time", "Estimate"]
  m_slope_se <- coefs["policy_time", "Std. Error"]
  m_slope_ci <- m_slope_est + c(-1.96*m_slope_se, 1.96*m_slope_se)
  
  # fitted values 
  df$fitted_base_values <- predict(mod)
  
  # residuals
  df$base_resid <- resid(mod)
  
  # bootstrap 
  bootstrap_estimates <- replicate(nboot, rtn_boot_internal(df, L))
  
  # intercept - mean, 95% CI 
  intercept_est <- bootstrap_estimates[1, ]
  int_mean <- mean(intercept_est)
  int_ci <- quantile(intercept_est, probs = c(0.025, 0.975))
  names(int_ci) <- NULL
  
  # slope - mean, 95% CI
  slope_est <- bootstrap_estimates[2, ]
  slope_mean <- mean(slope_est)
  slope_ci <- quantile(slope_est, probs = c(0.025, 0.975))
  names(slope_ci) <- NULL
  
  # results table
  model_results <- as.data.frame(t(c(m_int_est, m_int_ci[1], m_int_ci[2], m_slope_est, m_slope_ci[1], m_slope_ci[2])))
  boot_results <- as.data.frame(t(c(int_mean, int_ci[1], int_ci[2], slope_mean, slope_ci[1], slope_ci[2])))
  
  results <- rbind(model_results, boot_results)
  results$method <- c("model", "boot")
  colnames(results) <- c("intercept_est", "intercept_lb", "intercept_ub", "slope_est", "slope_lb", "slope_ub", "method")
  
  return(results)
}

# function with grid search only - used in both initial estimation and bootstrapping
grid_search_internal <- function(df) {
  sim_length <- nrow(df)
  # get covid start date from column (whole column is the same, so take the first)
  covid_start_date <- df$covid_start_ind[1]
  ssr <- rep(NA, sim_length)
  for (i in covid_start_date:sim_length) {
    mod <- estimate_model_covid_date(i, df)
    ssr[i] <- sum(mod$residuals^2)
  }
  # row index of minimum ssr
  # if there are ties, take later RTN date
  min_ssr <- min(ssr, na.rm = TRUE)
  date_minimize <- max(which(abs(ssr - min_ssr) < 1e-6))
  return(date_minimize)
}

# estimate the model for a given COVID end date (represented by index i)
estimate_model_covid_date <- function(i, df) {
  covid_start_date <- first(df$covid_start_ind)
  df$covid_assume <- df$time >= covid_start_date & df$time <= i
  df$covid_time_est <- (df$time - df$covid_start_ind) * df$covid_assume
  lm(outcome ~ time +
       covid_assume + covid_time_est +
       policy_ind + policy_time,
     data = df)
}

rtn_boot_internal <- function(df, L) {
  sim_length <- nrow(df)
  
  # create blocks - overlapping blocks of fixed length L
  # so, for series of length T, 
  # possible blocks have indices (1, ..., L), (2, ..., L+1), (3, ..., L+2), ...
  #     (T-L+1, ..., T)
  # this means there are T-L+1 blocks 
  # we can sample randomly from indices 1:(T-L+1)
  
  # number of blocks: ceil(T/L) (truncate last block)
  nblocks <- ceiling(sim_length/L)
  
  # length of last block (remainder)
  last_block_length <- sim_length %% L 
  # container for blocks
  list_resampled_resid <- vector(length = nblocks, mode="list")
  for (i in 1:nblocks){
    # get block starting index 
    block_start <- sample(1:(sim_length - L + 1), size = 1)
    
    # extract block from residuals
    ith_block <- df$base_resid[block_start:(block_start+L-1)]
    
    # truncate last block 
    if (i == nblocks){ 
      ith_block <- ith_block[1:last_block_length]
    }
    
    # add to container  
    list_resampled_resid[[i]] <- ith_block
  }
  
  # concat container into new residuals 
  resampled_resid <- c(list_resampled_resid, recursive=TRUE)
  
  # add residuals to fitted values to get new series (overwrite old outcome values)
  df$outcome <- df$fitted_base_values + resampled_resid
  
  # find best-fitting COVID date
  best_date <- grid_search_internal(df)
  
  # estimate model with best-fitting COVID date
  mod <- estimate_model_covid_date(best_date, df)
  
  # return estimated values
  coefs <- summary(mod)$coefficients[c("policy_indTRUE", "policy_time"), "Estimate"]
  coefs
}
