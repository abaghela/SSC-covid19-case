
# Load packages
library(tidyverse)

data1 <- read_csv("../bc_data/BCCDC_COVID19_Dashboard_Case_Details.csv") %>%
  janitor::clean_names()

glimpse(data1)


# Plot new cases each day
data1 %>%
  group_by(reported_date) %>%
  summarise(
    daily_cases = n()
  ) %>%
  ggplot(., aes(reported_date, daily_cases)) +
    geom_col() +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0))


# Cumulative cases
data1 %>%
  group_by(reported_date) %>%
  summarise(
    daily_cases = n()
  ) %>%
  mutate(cumulative_cases = cumsum(daily_cases)) %>%
  ggplot(., aes(reported_date, cumulative_cases)) +
    geom_line() +
    theme_classic()


# Colour by region
data1 %>%
  group_by(reported_date, ha) %>%
  summarise(
    daily_cases = n()
  ) %>%
  ggplot(., aes(reported_date, daily_cases, fill = ha)) +
  geom_col() +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0))


# Add moving average
ma <- function(x, n = 7) {
  stats::filter(x, rep(1 / n, n), sides = 2)
}

data1 %>%
  group_by(reported_date) %>%
  summarise(
    daily_cases = n()
  ) %>%
  mutate(week_avg = ma(daily_cases, n = 30)) %>%
  ggplot(., aes(reported_date, daily_cases)) +
    geom_col() +
    geom_line(aes(reported_date, week_avg), size = 1.1, colour = "dodgerblue") +
    theme_classic() +
    scale_y_continuous(expand = c(0, 0))
