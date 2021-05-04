

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

ed_parameters <- c(
  mu = 1 / 70, # per capita death rate and pop level birth rate (we assume they're the same)
  beta = 520 / 200, # transmission rate
  sigma = 1 / 14, # movement form exposed to infectious
  gamma = 1 / 9 # recovery rate
)

ed_initials <-
  c(
    S = 1 - 1e-6, # Susceptible
    E = 1e-04, # Exposed
    I = 1e-6, # Infected
    R = 1 - 0.1 - 1e-4 - 1e-4 # Recovered
  )

ed_seir <- SEIR(
  pars = ed_parameters,
  init = ed_initials,
  time = 0:70
)

ed_seir$results %>%
  pivot_longer(., S:R, names_to = "pop", values_to = "num") %>%
  ggplot(., aes(time, num, group = pop, colour = pop)) +
    geom_line(size = 1.2) +
    theme_classic(base_size = 16)
