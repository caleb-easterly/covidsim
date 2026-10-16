rm(list=ls())

library(tidyverse)
library(reshape2)
source("1-define-parameters.R")
load("results/results_clean_2026-03-16.rda")

results_grid <- results_clean %>%
  filter(method == "GS-ITS")

# create data for plot
sum_perc <- results_grid %>%
  group_by(covid_scenario, assumed_rtn_date) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count))

ggplot(sum_perc) +
  facet_wrap(~covid_scenario, nrow=5, ncol=2) + 
  geom_bar(aes(x=assumed_rtn_date, y = perc), stat="identity") +
  geom_vline(data=results_grid, aes(xintercept=covid_end_true)) +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() +
  labs(x = "Estimated end of COVID-19 period",
       y = "Relative frequency")
ggsave("figures/14-suppfig-grid-search.png", width=3500, height=2000, units="px")
ggsave("figures/14-suppfig-grid-search.pdf", width=3500, height=2000, units="px")

# averages for supplement
grid_avg <- results_grid %>%
  group_by(covid_scenario, covid_end_true) %>%
  mutate(err = (assumed_rtn_date - covid_end_true)*12) %>%
  summarize(meanerr = mean(err), sd = sd(err))
grid_avg
write.csv(grid_avg,
          file="results/14-grid_avg_err.csv",
          quote = FALSE,
          row.names=FALSE)
