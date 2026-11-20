# Function: enhanced_pca

## Description
Performs Principal Component Analysis (PCA) and generates a scree plot showing variance explained.

## Arguments
data (data.frame): The numeric dataset.

## R Code
```r
library(ggplot2)
enhanced_pca <- function(data) {
  num_data <- data[sapply(data, is.numeric)]
  pca_res <- prcomp(num_data, scale. = TRUE)
  var_explained <- pca_res$sdev^2 / sum(pca_res$sdev^2)
  p <- ggplot(data.frame(PC = 1:length(var_explained), Var = var_explained), aes(x = PC, y = Var)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_line(color = "red", size = 1) + geom_point(color = "red", size = 2) +
    theme_minimal() + labs(title = "PCA Scree Plot", y = "Proportion of Variance Explained")
  print(p)
  return(summary(pca_res))
}
```
