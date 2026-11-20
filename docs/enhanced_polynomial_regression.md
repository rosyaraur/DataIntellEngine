# Function: enhanced_polynomial_regression

## Description
Fits a polynomial regression model and plots the fitted non-linear curve over a scatter plot.

## Arguments
data (data.frame): The dataset.
y (character): The response variable name.
x (character): The predictor variable name.
degree (integer): The degree of the polynomial.

## R Code
```r
library(ggplot2)
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
```
