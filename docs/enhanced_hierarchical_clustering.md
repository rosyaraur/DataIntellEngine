# Function: enhanced_hierarchical_clustering

## Description
Performs hierarchical clustering on numeric data and plots a standard dendrogram.

## Arguments
data (data.frame): Numeric dataset.
method (character): Agglomeration method (e.g., 'complete', 'ward.D2').

## R Code
```r
enhanced_hierarchical_clustering <- function(data, method = "complete") {
  num_data <- scale(data[sapply(data, is.numeric)])
  dist_mat <- dist(num_data)
  hc <- hclust(dist_mat, method = method)
  plot(hc, main = paste("Cluster Dendrogram (", method, ")", sep=""), xlab = "", sub = "")
  return(hc)
}
```
