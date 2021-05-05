
# Load packages -----------------------------------------------------------

library(EpiDynamics)
library(deSolve)
library(tidyverse)


# Custom, basic SIR model -------------------------------------------------

sir_v1 <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I
    dI <- beta * S * I - gamma * I
    dR <- gamma * I

    return(list(c(dS, dI, dR)))
  })
}

init <- c(S = (1 - 1e-6), I = 1e-6, 0.0)

parameters <- c(beta = 1.4247, gamma = 0.14286)

times <- 0:70

results_v1 <- as.data.frame(ode(
  y     = init,
  times = times,
  func  = sir_v1,
  parms = parameters
))

# results_v1$time <- NULL

results_v1 %>%
  rename("R" = V4) %>%
  pivot_longer(., cols = S:R, names_to = "group", values_to = "num") %>%
  ggplot(., aes(time, num, group = group, colour = group)) +
  geom_line(size = 1.2) +
  theme_classic(base_size = 16)


# Using EpiDynamics -------------------------------------------------------

seir_and_plot <- function(initials, paramaters, time) {
  results_ls <- EpiDynamics::SEIR(
    pars = paramaters,
    init = initials,
    time = time
  )
  results_ls$results %>%
    pivot_longer(., S:R, names_to = "pop", values_to = "num") %>%
      ggplot(., aes(time, num, group = pop, colour = pop)) +
      geom_line(size = 1.2) +
      theme_classic(base_size = 16)
}

seir_and_plot(
  initials = c(
    S = 0.1,
    E = 1e-4,
    I = 1e-4,
    R = 1 - 0.1 - 1e-4 - 1e-4
  ),
  paramaters = c(
    mu    = 1 / (70 * 365), # per capita death rate and pop level birth rate (we assume they're the same)
    beta  = 520 / 365, # transmission rate
    sigma = 1 / 14, # movement form exposed to infectious
    gamma = 1 / 7 # recovery rate
  ),
  time = 0:365
)
