#############################################
# Analysis 2: Two-Way ANOVA
# Dataset: insurance.csv
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#
# Research Question:
# Do sex (male/female) and smoking status (yes/no)
# jointly predict annual medical charges?
#############################################

# --- Load Packages ---
library(tidyverse)
library(effectsize)

ins <- read.csv("insurance.csv")

# --- Prepare Variables ---
# Ensure sex and smoker are factors:
ins$sex    <- factor(ins$sex)
ins$smoker <- factor(ins$smoker, levels = c("no", "yes"))

# Quick structure check
str(ins)

# --- Descriptive Statistics by Sex x Smoker ---
cell_descriptives <- ins %>%
  group_by(sex, smoker) %>%
  summarise(
    n            = n(),
    mean_charges = mean(charges),
    sd_charges   = sd(charges),
    .groups      = "drop"
  )

cat("\n=== Descriptive statistics by sex x smoker ===\n")
print(cell_descriptives)

# --- Two-Way ANOVA ---
anova_mod <- aov(charges ~ sex * smoker, data = ins)

cat("\n=== Two-Way ANOVA results (sex, smoker, sex:smoker) ===\n")
print(summary(anova_mod))

# --- Effect Sizes (Partial Eta Squared) ---
cat("\n=== Effect sizes (partial eta-squared) ===\n")
eta_res <- eta_squared(anova_mod, partial = TRUE)
print(eta_res)

# --- Optional: Interaction Plot ---
# This helps visualize the sex x smoker interaction on charges.

cat("\nCreating interaction plot...\n")

ggplot(ins, aes(x = smoker, y = charges, color = sex, group = sex)) +
  stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.1)) +
  stat_summary(fun = mean, geom = "line", position = position_dodge(width = 0.1)) +
  theme_minimal() +
  labs(
    title = "Interaction of Sex and Smoking Status on Medical Charges",
    x     = "Smoking Status",
    y     = "Mean Annual Medical Charges",
    color = "Sex"
  )
