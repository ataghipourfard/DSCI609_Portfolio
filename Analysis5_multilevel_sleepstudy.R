#############################################
# Analysis 5: Multilevel Modeling
# Dataset: sleepstudy (built-in)
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#############################################

library(tidyverse)
library(lme4)
library(lmerTest)


data("sleepstudy")
mlm_data <- sleepstudy

# Null model (random intercepts)
null_model <- lmer(Reaction ~ 1 + (1 | Subject), data = mlm_data)
cat("\n=== Null Model (Random Intercepts) ===\n")
print(summary(null_model))

# Calculate ICC
var_intercept <- as.numeric(VarCorr(null_model)$Subject[1])
var_residual  <- attr(VarCorr(null_model), "sc")^2
icc <- var_intercept / (var_intercept + var_residual)
cat("\nICC =", icc, "\n")

# Model with predictor and random slopes
model1 <- lmer(Reaction ~ Days + (Days | Subject), data = mlm_data)
cat("\n=== Random Intercept and Slope Model ===\n")
print(summary(model1))
