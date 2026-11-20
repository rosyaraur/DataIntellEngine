# Function: enhanced_summary_stats

## Description
Calculates summary statistics for a numeric variable and generates a boxplot with overlaid jitter points.

## Arguments
data (data.frame): The dataset.
var_name (character): The name of the numeric variable.

## R Code
```r
library(ggplot2)
enhanced_summary_stats <- function(data, var_name) {
  stats <- summary(data[[var_name]])
  p <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    theme_minimal() + labs(title = paste("Summary of", var_name), x = "", y = var_name)
  print(p)
  return(stats)
}
```
