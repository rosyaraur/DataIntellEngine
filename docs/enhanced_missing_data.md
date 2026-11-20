# Function: enhanced_missing_data

## Description
Calculates the percentage of missing values per column and visualizes the missingness pattern.

## Arguments
data (data.frame): The dataset.

## R Code
```r
library(ggplot2)
library(reshape2)
enhanced_missing_data <- function(data) {
  missing_pct <- colMeans(is.na(data)) * 100
  missing_map <- melt(is.na(data))
  p <- ggplot(missing_map, aes(x = Var2, y = Var1, fill = value)) +
    geom_tile() + scale_fill_manual(values = c("grey90", "red"), name = "Missing") +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = "Missing Data Map", x = "Variables", y = "Observations")
  print(p)
  return(sort(missing_pct, decreasing = TRUE))
}
```
