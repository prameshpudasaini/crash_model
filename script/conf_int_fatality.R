library(tidyverse)

death_data <- read_csv("data/year_fatality.csv")

l.model <- lm(Fatality ~ 1, data = death_data)

confint(l.model, level = 0.95)
mean(death_data$Fatality)
