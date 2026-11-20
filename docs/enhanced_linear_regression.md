# Function: enhanced_linear_regression

## Description
Fits a simple linear regression and outputs a scatter plot with the fitted regression line and confidence interval.

## Arguments
data (data.frame): The dataset.
formula (formula): A formula of the form y ~ x.

## R Code
```r
library(ggplot2)
enhanced_linear_regression <- function(data, formula) {
  model <- lm(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[2], y = vars[1])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", fill = "pink") +
    theme_minimal() + labs(title = "Linear Regression Fit")
  print(p)
  return(summary(model))
}
```
