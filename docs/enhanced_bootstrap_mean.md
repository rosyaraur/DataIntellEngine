# Function: enhanced_bootstrap_mean

## Description
Performs bootstrapping to estimate the mean and 95% CI, plotting the bootstrap distribution histogram.

## Arguments
x (numeric vector): The numeric data.
B (integer): Number of bootstrap replicates.

## R Code
```r
library(ggplot2)
enhanced_bootstrap_mean <- function(x, B = 1000) {
  n <- length(x)
  boot_means <- replicate(B, mean(sample(x, n, replace = TRUE)))
  ci <- quantile(boot_means, c(0.025, 0.975))
  p <- ggplot(data.frame(Means = boot_means), aes(x = Means)) +
    geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
    geom_vline(xintercept = ci, color = "red", linetype = "dashed") +
    theme_minimal() + labs(title = "Bootstrap Distribution of the Mean", subtitle = paste("95% CI: [", round(ci[1], 2), ",", round(ci[2], 2), "]"))
  print(p)
  return(list(Mean = mean(boot_means), CI = ci))
}
```
