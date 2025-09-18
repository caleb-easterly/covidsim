load("data/sim_parameters.rda")
source("functions/simulate_covid.R")
source("functions/utilities.R")

library(ggpubr)

set.seed(1984)

plot_covid_scenario <- function(clist) {
  covid_end <- clist$covid_end
  covid_end_df <- convert_yearmon(covid_end[1], covid_end[2])
  beta0 <- clist$beta0
  beta1 <- clist$beta1
  beta2 <- clist$beta2
  beta3 <- clist$beta3
  nonlin <- clist$nonlin
  simulated_data_nopolicy <- simulate_covid(
    beta0 = beta0, 
    beta1 = beta1, 
    beta2 = beta2, 
    beta3 = beta3, 
    gamma0 = 0,
    gamma1 = 0,
    covid_end = covid_end,
    nonlin = nonlin,
    rho = 0.1, wnvar = 1)
  simulated_data_nopolicy
}

plots <- vector(mode = "list", length = 10)

for (i in 1:10) {
  scen <- covid_scenarios[[i]]
  df <- plot_covid_scenario(scen)
  covid_end <- convert_yearmon(scen$covid_end[1], scen$covid_end[2])
  g <- ggplot(df) + 
    geom_point(aes(x = yearmon, y = outcome)) +
    theme_bw() +
    annotate("rect",
             xmin = as.yearmon("Mar 2020"),
             xmax = covid_end,
             ymin=-Inf,
             ymax=Inf,
             fill = "grey", 
             alpha = 0.5) +
    labs(x = "", y = "Outcome", title = scen$name) +
    scale_y_continuous(limits = c(-12.5, 3)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          title = element_text(size = 12))
  plots[[i]] <- g
}
ggarrange(ncol = 3, nrow = 4, plotlist = plots)
ggsave("figures/figure1-covid-scenarios.png", scale = 2, width = 6.5, height = 7, units = "in", dpi = 300)             
