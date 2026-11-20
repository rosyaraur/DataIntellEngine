# Function: enhanced_kmeans

## Description
Performs K-Means clustering and plots the results on the first two principal components.

## Arguments
data (data.frame): The numeric dataset.
centers (integer): The number of clusters (k).

## R Code
```r
library(ggplot2)
enhanced_kmeans <- function(data, centers) {
  num_data <- scale(data[sapply(data, is.numeric)])
  set.seed(123)
  km <- kmeans(num_data, centers = centers)
  pca <- prcomp(num_data)
  plot_data <- data.frame(pca$x[, 1:2], Cluster = as.factor(km$cluster))
  p <- ggplot(plot_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point(size = 2, alpha = 0.8) + stat_ellipse() +
    theme_minimal() + labs(title = paste("K-Means Clustering (k =", centers, ")"))
  print(p)
  return(km)
}
```
