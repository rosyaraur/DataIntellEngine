# Function: enhanced_correlation_matrix

## Description
Computes a Pearson correlation matrix for numeric columns and visualizes it as a heatmap.

## Arguments
data (data.frame): The dataset containing numeric columns.

## R Code
```r
library(ggplot2)
library(reshape2)
enhanced_correlation_matrix <- function(data) {
  num_data <- data[sapply(data, is.numeric)]
  cormat <- round(cor(num_data, use = "complete.obs"), 2)
  melted_cormat <- melt(cormat)
  p <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1,1)) +
    theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    geom_text(aes(label = value), color = "black", size = 3) +
    labs(title = "Correlation Heatmap", x = "", y = "")
  print(p)
  return(cormat)
}
```
