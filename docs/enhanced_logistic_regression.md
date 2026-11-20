# Function: enhanced_logistic_regression

## Description
Fits a logistic regression model and plots the predicted probabilities against the predictor.

## Arguments
data (data.frame): The dataset.
formula (formula): A formula where response is binary (0/1).

## R Code
```r
library(ggplot2)
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
```
