# Estimate the return to normal date in simulated data
# The COVID-19 period is fixed to begin in March 2020
# fig and figpath are deprecated
estimate_rtn <- function(df, fig=FALSE, figpath="figures/temp.png"){
  ## estimate time series process based on pre-COVID data
  # make outcome missing after covid start
  df$outcome_pre_covid <- df$outcome
  df$post_covid_ind <- df$time >= df$covid_start_ind
  df$outcome_pre_covid[df$post_covid_ind] <- NA
  
  # estimate model
  mod_rtn <- lm(outcome_pre_covid ~ time, data=df)
  pred_rtn <- predict(mod_rtn, newdata = df, interval = "prediction")
  df_preds <- cbind(df, pred_rtn)
  df_preds <- df_preds %>% 
    mutate(in_bounds = outcome >= lwr & outcome <= upr,
           in_bounds_post = as.vector(in_bounds == TRUE & post_covid_ind == TRUE))
  
  # different versions of the back to normal rule
  # brute forced this - surely could do with a loop
  df_preds <-
    mutate(df_preds,
           lead1_in_bounds_post = lead(in_bounds_post, 1),
           lead2_in_bounds_post = lead(in_bounds_post, 2),
           lead3_in_bounds_post = lead(in_bounds_post, 3),
           lead4_in_bounds_post = lead(in_bounds_post, 4),
           lead5_in_bounds_post = lead(in_bounds_post, 5),
           lead6_in_bounds_post = lead(in_bounds_post, 6),
           lead7_in_bounds_post = lead(in_bounds_post, 7),
           lead8_in_bounds_post = lead(in_bounds_post, 8),
           lead9_in_bounds_post = lead(in_bounds_post, 9),
           lead10_in_bounds_post = lead(in_bounds_post, 10),
           lead11_in_bounds_post = lead(in_bounds_post, 11),
           btn1 = in_bounds_post == TRUE,
           btn2 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE,
           btn2_alt = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE,
           btn3 = in_bounds_post == TRUE & 
             lead1_in_bounds_post == TRUE & 
             lead2_in_bounds_post == TRUE,
           btn4 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE,
           btn5 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE,
           btn6 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE,
           btn7 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE,
           btn8 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE &
             lead7_in_bounds_post == TRUE,
           btn9 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE &
             lead7_in_bounds_post == TRUE &
             lead8_in_bounds_post == TRUE,
           btn10 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE &
             lead7_in_bounds_post == TRUE &
             lead8_in_bounds_post == TRUE &
             lead9_in_bounds_post == TRUE,
           btn11 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE &
             lead7_in_bounds_post == TRUE &
             lead8_in_bounds_post == TRUE &
             lead9_in_bounds_post == TRUE &
             lead10_in_bounds_post == TRUE,
           btn12 = in_bounds_post == TRUE &
             lead1_in_bounds_post == TRUE &
             lead2_in_bounds_post == TRUE &
             lead3_in_bounds_post == TRUE &
             lead4_in_bounds_post == TRUE &
             lead5_in_bounds_post == TRUE &
             lead6_in_bounds_post == TRUE &
             lead7_in_bounds_post == TRUE &
             lead8_in_bounds_post == TRUE &
             lead9_in_bounds_post == TRUE &
             lead10_in_bounds_post == TRUE &
             lead11_in_bounds_post == TRUE)
           
  covid_start_ind <- min(which(df$covid_ind))
  for (i in 1:12) {
    # calculate btn date
    ind_col <- paste0("btn", i)
    
    # create variable name
    arg <- sym(paste0("btn", i, "_date"))
    
    # define btn_date variable as date index for first occurence of btn_i == TRUE
    inject(!!arg <- which.max(df_preds[, ind_col]))
    
    # clean up - this is mostly necessary for the permanent/no slope scenario 
    # or any other case where there is no RTN estimated
    # then which.max equals 1 (since FALSE is the max and that first happens at time 1) 
    # replace with NA if RTN date is before COVID start date 
    if (eval(arg) < covid_start_ind) {
      inject(!!arg <- NA)
    }
  }
  # get return to normal for lead 3 in date format for plotting
  ## if there was no return to normal, treat COVID as permanent
  if (is.na(btn3_date)) {
    rtn <- c(2023, 12)
  } else {
    data_start <- as.yearmon("2015-10-01")
    rtn <- add_months(data_start, btn3_date - 1)
  }
  rtn_date <- convert_yearmon(rtn[1], rtn[2])
  # make figure - deprecated
  if (fig == TRUE) {
    cat("fig argument is deprecated")
    # ggplot(df_preds) + 
    #   geom_point(aes(x = yearmon, y = outcome)) + 
    #   geom_line(aes(x = yearmon, y = fit)) +
    #   geom_ribbon(aes(x = yearmon, ymin = lwr, ymax = upr), fill="grey70", alpha = 0.5) +
    #   geom_vline(aes(xintercept = as.yearmon(as.Date("2020-03-01"))), linetype = "dashed") +
    #   geom_vline(aes(xintercept = covid_end), linetype = "dashed") + 
    #   geom_vline(aes(xintercept = rtn_date), color = "darkred", linetype="dashed") +
    #   theme_bw() +
    #   labs(x = "", y = "Outcome")
    # ggsave(figpath, width=7, height=4, units="in", dpi=1000)
  }

  # export results
  covid_stop_ind <- max(which(df$covid_ind))
  df <- data.frame(covid_stop_ind,
                   btn1_date,
                   btn2_date,
                   btn3_date,
                   btn4_date,
                   btn5_date,
                   btn6_date,
                   btn7_date,
                   btn8_date,
                   btn9_date,
                   btn10_date,
                   btn11_date,
                   btn12_date)
  colnames(df) <- c("true_covid_stop_time", colnames(df)[2:13])
  df
}
