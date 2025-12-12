#############################################
# Analysis 1: Independent Samples t-test
# Dataset: insurance.csv
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#############################################

# --- Load Packages ---
library(tidyverse)
library(effectsize)



ins <- read.csv("insurance.csv")

# --- Prepare Variables ---
ins$smoker <- factor(ins$smoker, levels = c("no", "yes"))
ins$sex <- factor(ins$sex)

# --- Descriptive Statistics ---
descriptives <- ins %>%
  group_by(smoker) %>%
  summarise(
    n = n(),
    mean_charges = mean(charges),
    sd_charges = sd(charges)
  )
print("Descriptive statistics:")
print(descriptives)

# --- Independent Samples t-test ---
t_res <- t.test(charges ~ smoker, data = ins)
print("T-test results:")
print(t_res)

# --- Effect Size (Cohen's d) ---
d_res <- effectsize::cohens_d(charges ~ smoker, data = ins)
print("Effect size (Cohen's d):")
print(d_res)


ggplot(ins, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Medical Charges by Smoking Status",
       x = "Smoking Status",
       y = "Annual Medical Charges")

