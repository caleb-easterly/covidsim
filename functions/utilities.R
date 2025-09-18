# function to convert numeric year-month vectors to character year-month (zoo)
convert_yearmon <- function(year, month) {
  as.yearmon(as.Date(paste0(as.character(year), "-", as.character(month), "-01")))
}

# add months to a date (yearmon)
# adapted from https://stackoverflow.com/questions/14169620/add-a-month-to-a-date
add_months <- function(yearmon,n) {
  # ### logic for year:
  # ### the first normal month comes out of estimate_rtn as number of months since Oct 2015
  # ### so we add the number of months to october 2015, divide by 12, subtract 1, and take the floor
  # ### example: we estimate that the 57th month after October 2015 is the return to normal date
  # ### this is 67 months after January 2015 (add 10)
  # ### that is five years and 7 months after january 2015 
  # ### to get COVID indicator, we subtract 1 month (since COVID indicator is inclusive, [])
  # ### add the 5 months to 2015
  # rtn_year <- 2015 + floor((rtn_date + 10 - 1)/12)
  # 
  # ### logic for month: 
  # ### - get number of months since Oct 2015 after accounting for years (inside part)
  # ### - subtract one, since we count from 1
  # ### - add to the 10th month
  # ### - if this crosses over another year, we want to take the remainder again
  # rtn_month <- (10 + ((rtn_date %% 12) - 1)) %% 12
  # 
  # ## if after all of this, month is 0, decrement year by 1 and replace 0 by 12
  # if (rtn_month == 0) {
  #   rtn_year <- rtn_year - 1
  #   rtn_month <- 12
  # }
  # convert yearmon to date
  dt <- as.Date(yearmon)
  
  # increment date by number of months
  inc <- seq(dt, by = paste(n, "months"), length = 2)[2]
  
  # report vector of year and month
  c(year(inc), month(inc))
}
