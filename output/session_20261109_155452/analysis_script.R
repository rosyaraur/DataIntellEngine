# Load necessary libraries
library(lme4)
library(lattice)
library(ggplot2)

# Define the function to fit the linear mixed model
enhanced_mixed_model <- function(data, formula) {
  model <- lmer(formula, data = data)
  re <- ranef(model)
  print(dotplot(re)) # Creates the caterpillar plot
  return(summary(model))
}

# Define the function to generate the caterpillar plot
caterpillar_plot <- function(data, re) {
  # Create the caterpillar plot
  catplot(re, main = "Random Effects", 
          type = "b", 
          panel = function() {
            panel.bars()
          },
          xlab = "Group", 
          ylab = "Effect Size")
}

# Define the function to fit the linear mixed model
fit_linear_mixed_model <- function(data) {
  # Define the formula for the linear mixed model
  formula <- bp_reduction ~ age + treatment + (1|clinic_site)
  
  # Fit the linear mixed model
  model <- lmer(formula, data = data)
  
  # Print the summary of the model
  print(summary(model))
  
  # Generate the caterpillar plot of the random effects
  caterpillar_plot(data, ranef(model))
}

# Load the clinical trial data
data <- read.csv('./clinical_trial_data.csv')

# Fit the linear mixed model
fit_linear_mixed_model(data)