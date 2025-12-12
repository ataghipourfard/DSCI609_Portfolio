#############################################
# Analysis 3: Hierarchical Regression
# Dataset: World Happiness Report (happy/2019.csv)
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#############################################

library(tidyverse)

happy <- read.csv("happy/2019.csv")

# --- Select and Rename Variables ---
happy_data <- happy %>%
  select(
    happiness = Score,
    gdp       = GDP.per.capita,
    life      = Healthy.life.expectancy,
    support   = Social.support,
    freedom   = Freedom.to.make.life.choices
  )

# --- Model 1: Economic Predictors ---
mod1 <- lm(happiness ~ gdp + life, data = happy_data)
cat("\n=== Model 1: Economic Predictors ===\n")
print(summary(mod1))

# --- Model 2: Economic + Social Predictors ---
mod2 <- lm(happiness ~ gdp + life + support + freedom, data = happy_data)
cat("\n=== Model 2: Economic + Social Predictors ===\n")
print(summary(mod2))

# --- ΔR² Test ---
cat("\n=== Model Comparison (ANOVA) ===\n")
print(anova(mod1, mod2))

