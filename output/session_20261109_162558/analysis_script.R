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

# Define the function to fit the logistic regression model
enhanced_logistic_regression <- function(data, formula) {
  # Fit the logistic regression model
  model <- glm(formula, data = data, family = "binomial")
  
  # Extract the predictor variable
  vars <- all.vars(formula)
  
  # Predict the probabilities
  data$predicted <- predict(model, type = "response")
  
  # Create the plot
  p <- ggplot(data, aes_string(x = vars[2], y = "predicted")) +
    geom_point(aes_string(y = vars[1]), alpha = 0.3, shape = 1) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
    theme_minimal() + labs(title = "Logistic Regression Curve")
  
  # Print the plot
  print(p)
  
  # Return the summary of the model
  return(summary(model))
}

# Define the function to fit the polynomial regression model
enhanced_polynomial_regression <- function(data, y, x, degree = 2) {
  # Create the formula for the polynomial regression
  formula <- as.formula(paste(y, "~ poly(", x, ",", degree, ", raw=TRUE)"))
  
  # Fit the polynomial regression model
  model <- lm(formula, data = data)
  
  # Create the plot
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ poly(x, degree, raw=TRUE), color = "purple") +
    theme_minimal() + labs(title = paste("Polynomial Regression (Degree", degree, ")"))
  
  # Print the plot
  print(p)
  
  # Return the summary of the model
  return(summary(model))
}

# Load the clinical trial data
data <- read.csv('./clinical_trial_data.csv')

# Fit the linear mixed model
formula <- y ~ age + treatment + (1|clinic_site)
results <- enhanced_mixed_model(data, formula)

# Print the results
print(results)

# Fit the logistic regression model
formula <- y ~ treatment + (1|clinic_site)
results <- enhanced_logistic_regression(data, formula)

# Print the results
print(results)

# Fit the polynomial regression model
formula <- y ~ age + treatment
results <- enhanced_polynomial_regression(data, y = bp_reduction, x = age, degree = 2)

# Print the results
print(results)