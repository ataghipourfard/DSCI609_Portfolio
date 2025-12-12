#############################################
# Analysis 6: Exploratory Factor Analysis
# Dataset: World Happiness 2019
# Author: Ali Taghipourfard
# Course: DSCI 609 Portfolio Project
#############################################

library(tidyverse)
library(psych)

happy <- read.csv("~/Downloads/Portfolio/happy/2019.csv")


# Select variables for EFA
fa_data <- happy %>%
  select(
    gdp        = GDP.per.capita,
    support    = Social.support,
    life       = Healthy.life.expectancy,
    freedom    = Freedom.to.make.life.choices,
    generosity = Generosity,
    corruption = Perceptions.of.corruption
  )

# Sampling adequacy tests
print(KMO(fa_data))
print(cortest.bartlett(cor(fa_data), n = nrow(fa_data)))

# EFA (2-factor solution)
fa_result <- fa(fa_data, nfactors = 2, rotate = "oblimin")
print(fa_result)

# Optional: Save loadings table to CSV
write.csv(fa_result$loadings[], "efa_loadings.csv", row.names = TRUE)
