# Load necessary libraries
library(lme4)
library(lattice)
library(ggplot2)

# Define the function to fit the linear mixed model
fit_linear_mixed_model <- function(data, y, x1, x2, random_factor) {
  # Create the formula for the linear mixed model
  formula <- y ~ x1 + x2 + (1|random_factor)
  
  # Fit the linear mixed model
  model <- lmer(formula, data = data)
  
  # Extract the random effects
  re <- ranef(model)
  
  # Print the caterpillar plot of the random effects
  print(dotplot(re, main = "Caterpillar Plot of Random Effects"))
  
  # Print the summary of the linear mixed model
  print(summary(model))
  
  # Return the summary of the linear mixed model
  return(summary(model))
}

# Load the clinical trial data
data <- read.csv('./clinical_trial_data.csv')

# Fit the linear mixed model
fit_linear_mixed_model(data, y = "bp_reduction", x1 = "age", x2 = "treatment", random_factor = "clinic_site")