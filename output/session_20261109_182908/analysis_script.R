# Load necessary libraries
library(lme4)
library(lattice)
library(ggplot2)

# Load the dataset
data <- read.csv('./clinical_trial_data.csv')

# Define the formula for the linear mixed model
formula <- bp_reduction ~ age + treatment + (1|clinic_site)

# Fit the linear mixed model
model <- lmer(formula, data = data)

# Extract the random effects
re <- ranef(model)

# Print the caterpillar plot of the random effects
print(dotplot(re))

# Print the summary of the linear mixed model
print(summary(model))