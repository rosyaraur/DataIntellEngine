# Function: enhanced_wilcoxon

## Description
Performs a non-parametric Wilcoxon Rank Sum test and plots overlapping density curves.

## Arguments
data (data.frame): The dataset.
formula (formula): A formula of the form numeric_var ~ binary_group.

## R Code
```r
library(ggplot2)
enhanced_wilcoxon <- function(data, formula) {
  res <- wilcox.test(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[1], fill = vars[2])) +
    geom_density(alpha = 0.5) +
    theme_minimal() + labs(title = "Density Plot by Group (Wilcoxon Test)")
  print(p)
  return(res)
}
```
