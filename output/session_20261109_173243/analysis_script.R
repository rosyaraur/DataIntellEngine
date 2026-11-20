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

# Define the function to fit the logistic regression model
enhanced_logistic_regression <- function(data, formula) {
  model <- glm(formula, data = data, family = "binomial")
  vars <- all.vars(formula)
  data$predicted <- predict(model, type = "response")
  p <- ggplot(data, aes_string(x = vars[2], y = "predicted")) +
    geom_point(aes_string(y = vars[1]), alpha = 0.3, shape = 1) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
    theme_minimal() + labs(title = "Logistic Regression Curve")
  print(p)
  return(summary(model))
}

# Define the function to fit the polynomial regression model
enhanced_polynomial_regression <- function(data, y, x, degree = 2) {
  formula <- as.formula(paste(y, "~ poly(", x, ",", degree, ", raw=TRUE)"))
  model <- lm(formula, data = data)
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ poly(x, degree, raw=TRUE), color = "purple") +
    theme_minimal() + labs(title = paste("Polynomial Regression (Degree", degree, ")"))
  print(p)
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
results_logistic <- enhanced_logistic_regression(data, formula)

# Print the results
print(results_logistic)

# Fit the polynomial regression model
formula <- y ~ age + treatment
results_polynomial <- enhanced_polynomial_regression(data, y = bp_reduction, x = age, degree = 2)

# Print the results
print(results_polynomial)