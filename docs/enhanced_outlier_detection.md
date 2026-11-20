# Function: enhanced_outlier_detection

## Description
Identifies influential observations in a linear model using Cook's Distance and plots them.

## Arguments
model (lm object): A fitted linear model.

## R Code
```r
library(ggplot2)
enhanced_outlier_detection <- function(model) {
  cooksd <- cooks.distance(model)
  plot_data <- data.frame(Index = seq_along(cooksd), CooksD = cooksd)
  threshold <- 4 / length(cooksd)
  p <- ggplot(plot_data, aes(x = Index, y = CooksD)) +
    geom_bar(stat = "identity", fill = "steelblue", width = 0.5) +
    geom_hline(yintercept = threshold, color = "red", linetype = "dashed") +
    theme_minimal() + labs(title = "Cook's Distance for Outlier Detection", subtitle = paste("Threshold =", round(threshold, 4)))
  print(p)
  return(which(cooksd > threshold))
}
```
