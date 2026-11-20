# Load necessary libraries
library(lme4)
library(lattice)
library(ggplot2)

# Define the function to fit the linear mixed model
enhanced_mixed_model <- function(data, formula) {
  # Fit the linear mixed model
  model <- lmer(formula, data = data)
  
  # Extract the random effects
  re <- ranef(model)
  
  # Print the caterpillar plot of the random effects
  print(dotplot(re))
  
  # Return the summary of the model
  return(summary(model))
}

# Load the clinical trial data
data <- read.csv('./clinical_trial_data.csv')

# Define the formula for the linear mixed model
formula <- y ~ age + treatment + (1|clinic_site)

# Fit the linear mixed model
model <- enhanced_mixed_model(data, formula)

# Print the summary of the model
print(model)