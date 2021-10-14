# -----------------------------------------------------------------
# Analysis code for:
# Keusch, F., Wenz, A., & Conrad, F. (in press). Do you have your smartphone with you?
# Behavioral barriers for measuring everyday activities with smartphone sensors.
# Computers in Human Behavior. https://doi.org/10.1016/j.chb.2021.107054
# Plots
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
# Load data sets for RQ2 plots
load("Behavioral barriers_AMEs_Sample1.RData")
sample2 <- read.csv("output_barriers_sample2.csv")

# Merge two samples
ames <- bind_rows(sample1, sample2) %>%
  mutate(sample = factor(sample, labels = c("Sample 1",
                                            "Sample 2"))) %>%
  mutate(dependent = factor(dependent, labels = c("Smartphone shared with another person",
                                                  "Smartphone not always on",
                                                  "Smartphone left at home",
                                                  "Smartphone carried in purse/backpack/bag",
                                                  "Smartphone left stationary when at home",
                                                  "Smartphone turned off or in other room at night"))) %>%
  mutate(independent = factor(independent, levels = c("1",
                                                      "genderFemale",
                                                      "2",
                                                      "agegroup125-30",
                                                      "agegroup131-40",
                                                      "agegroup141-50",
                                                      "agegroup151-60",
                                                      "agegroup161+",
                                                      "3",
                                                      "eduHigh school degree",
                                                      "eduCollege degree",
                                                      "4",
                                                      "osAndroid",
                                                      "osOther OS",
                                                      "5",
                                                      "frequseEvery day",
                                                      "frequseSeveral times a day",
                                                      "6",
                                                      "skillsIntermediate",
                                                      "skillsAdvanced",
                                                      "7",
                                                      "phonedependentYes"),
                              labels = c("Gender (Ref = Male)", "Female",
                                         "Age (Ref = 18-24)", "25-30", "31-40", "41-50", "51-60", "61+",
                                         "Edu. attainment (Ref = No high school degree)", "High school degree", "College degree",
                                         "OS (Ref = iOS)", "Android", "Other OS",
                                         "Freq. of smartphone use (Ref = Several times a week or less)",  "Every day", "Several times a day",
                                         "Smartphone skills (Ref = Beginner)",  "Intermediate","Advanced",
                                         "Smartphone dependent (Ref = No)", "Yes")))

# Function for plots
plot_ames <- function(data) {
  ggplot(data, aes(x = reorder(independent, desc(independent)), y = AME, shape = reorder(sample, desc(sample)))) +
    geom_point(size = 2, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.75), width = 0.25) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    coord_flip() +
    theme_minimal() +
    facet_wrap(.~ dependent, labeller = label_wrap_gen(width=30)) +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1), color = "black"),
          axis.text.x = element_text(size = rel(1), colour = "black"),
          axis.text.y = element_text(size = rel(1), colour = "black"),
          legend.text = element_text(size = rel(1)),
          strip.text.x = element_text(size = rel(1), color = "black", face = "bold")) +
    guides(shape = guide_legend(reverse=TRUE))
}

# Fig.1. Average marginal effects (points) and 95%-confidence intervals (lines)
# from logistic regression models of smartphone usage behaviors (smartphone shared
# with another person, smartphone not always on, smartphone left at home) on
# sociodemographic and smartphone-related characteristics. Predictors for which
# the horizontal lines do not cross the dashed 0-line are statistically significant
# at the 5%-level.
ames_plot1 <- ames %>%
  filter(dependent == "Smartphone shared with another person" |
           dependent == "Smartphone not always on" |
           dependent == "Smartphone left at home")

plot_ames(ames_plot1)
ggsave("AMEs_1.png", width = 9, height = 6.5, units = "in")

# Fig.2. Average marginal effects (points) and 95%-confidence intervals (lines)
# from logistic regression models of smartphone usage behaviors (smartphone carried
# in purse/backpack/bag, smartphone left stationary when at home, smartphone turned
# off or in other room at night) on sociodemographic and smartphone-related
# characteristics. Predictors for which # the horizontal lines do not cross the
# dashed 0-line are statistically significant
# at the 5%-level.
ames_plot2 <- ames %>%
  filter(dependent == "Smartphone carried in purse/backpack/bag" |
           dependent == "Smartphone left stationary when at home" |
           dependent == "Smartphone turned off or in other room at night")

plot_ames(ames_plot2)
ggsave("AMEs_2.png", width = 9, height = 6.5, units = "in")

# -----------------------------------------------------------------
# Load data sets for RQ3 plots
targets_log <- read.csv("output_targets_log.csv")
targets <- read.csv("output_targets.csv")

# Function for plots
plot_estimates <- function(data){
  ggplot(data, aes(x = reorder(independent, desc(independent)), y = estimate, shape = reorder(dependent, desc(dependent)))) +
    geom_point(size = 1, position = position_dodge(width = 0.75)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.75), width = 0.25) +
    geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = 8, color = "black"),
          axis.text.x = element_text(size = 8, colour = "black"),
          axis.text.y = element_text(size = 8, colour = "black"),
          legend.text = element_text(size = 8)) +
    guides(shape = guide_legend(reverse=TRUE, nrow = 2))
}

targets_log <- targets_log %>%
  filter(independent != "(Intercept)") %>%
  mutate(dependent = factor(dependent, labels = c("Mobility - weekday (log km)",
                                                  "Mobility - weekend (log km)"))) %>%
  mutate(independent = factor(independent, levels = c("sharingSmartphone shared with another person",
                                                      "turnedoffSmartphone not always on",
                                                      "outhomeuseSmartphone left at home"),
                              labels = c("Smartphone shared with another person",
                                         "Smartphone not always on",
                                         "Smartphone left at home")))

targets <- targets %>%
  filter(independent != "(Intercept)") %>%
  mutate(dependent = factor(dependent, labels = c("Mobility - weekday (km)",
                                                  "Mobility - weekend (km)",
                                                  "Physical activity - vigorous",
                                                  "Physical activity - moderate",
                                                  "Sedentary behavior - weekday (hrs)",
                                                  "Sedentary behavior - weekend (hrs)",
                                                  "Sleep - weekday (hrs)",
                                                  "Sleep - weekend (hrs)"))) %>%
  mutate(independent = factor(independent, levels = c("sharingSmartphone shared with another person",
                                                      "turnedoffSmartphone not always on",
                                                      "outhomeuseSmartphone left at home",
                                                      "athomelocationSmartphone left stationary when at home",
                                                      "outhomelocationIn purse/backpack/bag",
                                                      "nightstatusandlocationSmartphone turned off or in other room at night"),
                              labels = c("Smartphone shared with another person",
                                         "Smartphone not always on",
                                         "Smartphone left at home",
                                         "Smartphone left stationary when at home",
                                         "Smartphone carried in purse/backpack/bag",
                                         "Smartphone turned off or in other room at night")))

# Fig.3. Estimates (points) and 95%-confidence intervals (lines) from OLS regression
# models of mobility (log km) on smartphone usage behaviors controlling for sociodemographic 
# (gender, age, educational attainment - estimates not shown) and smartphone-related 
# characteristics (OS, frequency of smartphone use, smartphone skills, smartphone 
# dependency - estimates not shown). Predictors for which the horizontal lines do 
# not cross the dashed 0-line are statistically significant at the 5%-level.
mobility_log <- targets_log %>%
  filter(model == 2) %>%
  filter(dependent == "Mobility - weekday (log km)" |
           dependent == "Mobility - weekend (log km)")

plot_estimates(mobility_log)
ggsave("mobility_log.png", width = 4, height = 4, units = "in")

# Fig.4. AMEs (points) and 95%-confidence intervals (lines) from ordered logistic 
# regression models of physical activity on smartphone usage behaviors controlling 
# for sociodemographic (gender, age, educational attainment - estimates not shown) 
# and smartphone-related characteristics (OS, frequency of smartphone use, smartphone 
# skills, smartphone dependency - estimates not shown). Predictors for which the 
# horizontal lines do not cross the dashed 0-line are statistically significant 
# at the 5%-level.
activity <- targets %>%
  filter(model == 2) %>%
  filter(dependent == "Physical activity - vigorous" |
           dependent == "Physical activity - moderate")

plot_estimates(activity)
ggsave("activity_4CAT.png", width = 4, height = 4, units = "in")

# Fig.5. Estimates (points) and 95%-confidence intervals (lines) from OLS 
# regression models of sedentary behavior (hrs) on smartphone usage behaviors controlling 
# for sociodemographic (gender, age, educational attainment - estimates not shown) 
# and smartphone-related characteristics (OS, frequency of smartphone use, smartphone 
# skills, smartphone dependency - estimates not shown). Predictors for which the 
# horizontal lines do not cross the dashed 0-line are statistically significant 
# at the 5%-level.
sedentary <- targets %>%
  filter(model == 2) %>%
  filter(dependent == "Sedentary behavior - weekday (hrs)" |
           dependent == "Sedentary behavior - weekend (hrs)")

plot_estimates(sedentary)
ggsave("sedentary.png", width = 6, height = 4, units = "in")

# Fig.6. Estimates (points) and 95%-confidence intervals (lines) from OLS 
# regression models of sleep (hrs) on smartphone usage behaviors controlling 
# for sociodemographic (gender, age, educational attainment - estimates not shown) 
# and smartphone-related characteristics (OS, frequency of smartphone use, smartphone 
# skills, smartphone dependency - estimates not shown). Predictors for which the 
# horizontal lines do not cross the dashed 0-line are statistically significant 
# at the 5%-level.
sleep <- targets %>%
  filter(model == 2) %>%
  filter(dependent == "Sleep - weekday (hrs)" |
           dependent == "Sleep - weekend (hrs)")

plot_estimates(sleep)
ggsave("sleep.png", width = 6, height = 4, units = "in")