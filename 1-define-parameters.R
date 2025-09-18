# clear 
rm(list=ls())

# packages
library(zoo)
library(dplyr)

# parameters
nreps <- 1000
# 3*4*18*10 + 18*10 = 2340
nscenarios <- 2340

# smaller number of reps and scenarios for testing
# nreps <- 10
# nscenarios <- 100

# Policy scenarios
# 8/28/2025 - drop policy effects above 10 
# now only 3 policy effects instead of 5
policy_effects <- c(2.5, 5, 10)

# keep policy signs so we can compare 
# even though they don't seem to matter at this point 
policy_signs <- matrix(
  c(1, 1, -1, 1, 1, -1, -1, -1),
  nrow=4,
  ncol=2,
  byrow = TRUE
)

# matrix with full factorial combinations of parameters
parameters <- expand.grid(policy_effects, 1:4, 1:18, 1:10)
colnames(parameters) <- c("effects", "signs", "policy_start", "covid_parms")

# matrix with full factorial combinations of parameters for null policy effects
# adds 180 scenarios, combine from combinations of 18 policy start dates
# and 10 covid parameter sets
# only one option for signs and policy start
# only 16 parameter start dates are used in the paper (2 were for SERs)
parameters_null <- expand.grid(0, 1, 1:18, 1:10)
colnames(parameters_null) <- c("effects", "signs", "policy_start", "covid_parms")

# combine 
parameters <- rbind(parameters, parameters_null)

parameters <- parameters %>%
  mutate(intercept = effects * policy_signs[signs, 1],
         slope = effects/10 * policy_signs[signs, 2]) %>%
  select(covid_parms, policy_start, intercept, slope, -signs, -effects)

# list of covid scenarios
covid_scenarios <- list(
  # 3 months (PHE)
  list(name = "Linear, 3 months",
       covid_end = c(2020, 5),
       beta0 = -10,
       beta1 = 10/3,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 6 months
  list(name = "Linear, 6 months",
       covid_end = c(2020, 8),
       beta0 = -10,
       beta1 = 10/6,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 1 year 
  list(name = "Linear, 1 year",
       covid_end = c(2021, 2),
       beta0 = -10,
       beta1 = 10/12,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 18 months 
  list(name = "Linear, 18 months",
       covid_end = c(2021, 8),
       beta0 = -10,
       beta1 = 10/18,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 2 years 
  list(name = "Linear, 2 years",
       covid_end = c(2022, 2),
       beta0 = -10,
       beta1 = 10/24,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 30 months 
  list(name = "Linear, 30 months",
       covid_end = c(2022, 8),
       beta0 = -10,
       beta1 = 10/30,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 36 months 
  list(name = "Linear, 36 months",
       covid_end = c(2023, 2),
       beta0 = -10,
       beta1 = 10/36,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # 39 months (PHE)
  list(name = "Linear, 39 months",
       covid_end = c(2023, 5),
       beta0 = -10,
       beta1 = 10/39,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # permanent COVID with no slope
  list(name = "Permanent, no slope",
       covid_end = c(2023, 12),
       beta0 = -10,
       beta1 = 0,
       beta2 = 0,
       beta3 = 0,
       nonlin = FALSE),
  # nonlinear COVID
  list(name = "Nonlinear",
       covid_end = c(2023, 5),
       beta0 = -2,
       beta1 = 2/39,
       beta2 = -8,
       beta3 = 0.4,
       nonlin = TRUE)
)

# list of policy start dates
policy_scenarios <- list(
  c(2018, 3), # two years before
  c(2018, 6),
  c(2018, 9), # 18 months before
  c(2018, 12),
  c(2019, 3), # one year before
  c(2019, 5), # SUD start date - not used in paper
  c(2019, 6), # nine months before
  c(2019, 9), # six months before
  c(2019, 12), # three months before
  c(2020, 6), # three months after
  c(2020, 9), # six months after
  c(2020, 12),
  c(2021, 3), # one year after
  c(2021, 6),
  c(2021, 7), # managed care's start date - not used in paper
  c(2021, 9),
  c(2021, 12),
  c(2022, 3) # two years after
)

# list of COVID end dates
covid_end_dates <- list(
  c(2020, 5), # 3 months
  c(2020, 8), # 6 months 
  c(2021, 2), # 12 months
  c(2021, 8), # 18 months
  c(2022, 2), # 24 months
  c(2022, 8), # 30 months
  c(2023, 2), # 36 months
  c(2023, 5), # 39 months (PHE)
  c(2023, 12) # permanent
)

save.image(file="data/sim_parameters.rda")
