# Function: enhanced_ttest

## Description
Performs a two-sample t-test and visualizes the distributions using a violin plot.

## Arguments
data (data.frame): The dataset.
formula (formula): A formula of the form y ~ x.

## R Code
```r
library(ggplot2)
enhanced_ttest <- function(data, formula) {
  res <- t.test(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[2], y = vars[1], fill = vars[2])) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white") +
    theme_minimal() + labs(title = paste("T-Test:", vars[1], "by", vars[2]))
  print(p)
  return(res)
}
```
