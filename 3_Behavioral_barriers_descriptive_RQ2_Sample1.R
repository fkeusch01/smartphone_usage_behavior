# -----------------------------------------------------------------
# Analysis code for:
# Keusch, F., Wenz, A., & Conrad, F. (in press). Do you have your smartphone with you?
# Behavioral barriers for measuring everyday activities with smartphone sensors.
# Computers in Human Behavior. https://doi.org/10.1016/j.chb.2021.107054
# RQ2
# -----------------------------------------------------------------
# Set working directory
setwd("U:\\Projekte\\GIP\\W48")

# -----------------------------------------------------------------
# Load packages
library(tidyverse)
library(dplyr)
library(naniar)
library(gmodels)
library(pscl)
library(margins)
library(MASS)
library(DescTools)
library(vcd)

# -----------------------------------------------------------------
# Load data set
load("Behavioral barriers_Sample1.RData")

# -----------------------------------------------------------------
# Table A.6. Logistic regression models of smartphone usage behaviors on sociodemographic and smartphone-related characteristics Sample 1
# Smartphone shared with another person
m121 <- glm(sharing ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m121))
PseudoR2(m121, which = "Nagelkerke")
nobs(m121)
m121 <- summary(margins(m121)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 1)

# Smartphone not always on
m122 <- glm(turnedoff ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m122))
PseudoR2(m122, which = "Nagelkerke")
nobs(m122)
m122 <- summary(margins(m122)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 2)

# Smartphone left at home
m123 <- glm(outhomeuse ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m123))
PseudoR2(m123, which = "Nagelkerke") 
nobs(m123)
m123 <- summary(margins(m123)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 3)

# Smartphone carried in purse/backpack/bag
m124 <- glm(outhomelocation ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m124))
PseudoR2(m124, which = "Nagelkerke")
nobs(m124)
m124 <- summary(margins(m124)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 4)

# Smartphone left stationary when at home
m125 <- glm(athomelocation ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m125))
PseudoR2(m125, which = "Nagelkerke")
nobs(m125)
m125 <- summary(margins(m125)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 5)

# Smartphone turned off or in other room at night
m126 <- glm(nightoff_location ~ agegroup1 + gender + edu + os + frequse + skills + phonedependent, data = gipw48_sp, family = "binomial")
summary(margins(m126))
PseudoR2(m126, which = "Nagelkerke")
nobs(m126)
m126 <- summary(margins(m126)) %>%
  mutate(sample = 1) %>%
  mutate(dependent = 6)

# Save data 
sample1 <- bind_rows(m121, m122, m123, m124, m125, m126) %>%
  add_row(sample = 1, factor = "1", dependent = 1:6) %>%
  add_row(sample = 1, factor = "2", dependent = 1:6) %>%
  add_row(sample = 1, factor = "3", dependent = 1:6) %>%
  add_row(sample = 1, factor = "4", dependent = 1:6) %>%
  add_row(sample = 1, factor = "5", dependent = 1:6) %>%
  add_row(sample = 1, factor = "6", dependent = 1:6) %>%
  add_row(sample = 1, factor = "7", dependent = 1:6) %>%
  dplyr::select(sample, dependent, factor, AME, lower, upper) %>%
  rename(independent = factor)

save(sample1, file="Behavioral barriers_AMEs_Sample1.RData")