# Function: enhanced_anova

## Description
Fits an ANOVA model, performs Tukey HSD post-hoc test, and plots the 95% confidence intervals of differences.

## Arguments
data (data.frame): The dataset.
formula (formula): A formula of the form y ~ group.

## R Code
```r
enhanced_anova <- function(data, formula) {
  model <- aov(formula, data = data)
  tukey <- TukeyHSD(model)
  plot(tukey, col = "red", las = 1)
  return(list(ANOVA = summary(model), Tukey = tukey))
}
```
