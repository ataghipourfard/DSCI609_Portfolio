#############################################
# Analysis 4: Moderation (Interaction Regression)
# Dataset: World Happiness Report (happy/2019.csv)
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#############################################

library(tidyverse)

setwd("/Users/alitaghipourfard/Downloads/Portfolio")

happy <- read.csv("happy/2019.csv")

# Build moderation dataset
mod_data <- happy %>%
  select(
    happiness = Score,
    support = Social.support,
    freedom = Freedom.to.make.life.choices
  )

# Unstandardized moderation model
mod_model <- lm(happiness ~ support * freedom, data = mod_data)

cat("\n=== Moderation Model (Unstandardized) ===\n")
print(summary(mod_model))

# Standardized moderation model
mod_data_std <- mod_data %>%
  mutate(
    support_z = scale(support),
    freedom_z = scale(freedom),
    happiness_z = scale(happiness)
  )

mod_model_z <- lm(happiness_z ~ support_z * freedom_z, data = mod_data_std)

cat("\n=== Moderation Model (Standardized) ===\n")
print(summary(mod_model_z))

# Optional interaction plot
library(ggplot2)

ggplot(mod_data, aes(x = support, y = happiness, color = freedom)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  labs(title = "Interaction of Social Support and Freedom on Happiness")

