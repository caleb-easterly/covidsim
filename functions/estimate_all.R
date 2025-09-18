#' Function to estimate policy treatment parameters with all methods
#' 
#' Methods:
#' - assumed COVID-19 period (loop over assumptions and estimate_lm.R)
#' - estimated COVID-19 period (use estimate_rtn.R followed by estimate_lm.R)
#' - difference-in-differences (use estimate_did.R)
#' 
#' Arguments:
#' - df: dataframe for one scenario and one rep, with both treatment and control groups

# packages
library(zoo)
library(lubridate)

# functions
source("functions/estimate_did.R")
source("functions/estimate_lm.R")
source("functions/estimate_rtn.R")
source("functions/estimate_lm_twoseg.R")
source("functions/estimate_lm_sahwash.R")
source("functions/utilities.R")

estimate_all <- function(df) {
  ## split up control data and treatment data
  df_control <- filter(df, treated == 0)
  df_treatment <- filter(df, treated == 1)

  ## Method 1: assume different COVID-19 periods (loop over)
  results_list <- lapply(covid_end_dates, 
                         function(x) estimate_lm(df_treatment, covid_end_est = x))
  
  ## Method 2: estimate RTN and then use that in linear model
  ## as of 8/6/2025 do for 1-6 month leads
  ## UPDATE - 8/23/2025 - 1-12 month leads
  dates <- c("btn1_date", "btn2_date", "btn3_date",
             "btn4_date", "btn5_date", "btn6_date",
             "btn7_date", "btn8_date", "btn9_date",
             "btn10_date", "btn11_date", "btn12_date")
  rtn_all_dates <- unlist(estimate_rtn(df_control)[,dates])
  
  ## loop over the 12 options
  for (i in 1:12) {
    rtn_date <- rtn_all_dates[i]
    ## if there was no return to normal, treat COVID as permanent
    if (is.na(rtn_date)) {
      rtn <- c(2023, 12)
    } else {
      data_start <- as.yearmon("2015-10-01")
      rtn <- add_months(data_start, rtn_date - 1)
    }
    
    ### estimate model
    lm_rtn_results <- estimate_lm(df_treatment, covid_end_est = rtn)
    
    ### add estimated date to lm_results
    lm_rtn_results$estimated_rtn_date_num <- rtn_date
    lm_rtn_results$estimated_rtn_yearmon <- convert_yearmon(rtn[1], rtn[2])
    lm_rtn_results$method <- paste0("lm_rtn", i)
    results_list <- append(results_list, list(lm_rtn_results))
  }

  ## Method 3: difference-in-differences
  did_results <- estimate_did(df)
  results_list <- append(results_list, list(did_results))
  
  # 8/18/2025 - dropping the two segment assumption because it performs
  # badly and interferes with policy starting in June 2020
  # ## Method 4: two-segment assumption, dropping march-may 2020
  # twoseg_results <- estimate_lm_twoseg(df_treatment)
  # results_list <- append(results_list, list(twoseg_results))
  # 
  # ## Method 5: stay-at-home washout, adding indicator for march-may 2020
  # sahwash_results <- estimate_lm_sahwash(df_treatment)
  # results_list <- append(results_list, list(sahwash_results))
  # 
  ### Bind results together
  bind_rows(results_list)
}