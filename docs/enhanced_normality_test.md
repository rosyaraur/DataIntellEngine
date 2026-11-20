# Function: enhanced_normality_test

## Description
Performs the Shapiro-Wilk test for normality and generates a Q-Q plot with a reference line.

## Arguments
x (numeric vector): The numeric data to test.

## R Code
```r
library(ggplot2)
enhanced_normality_test <- function(x) {
  res <- shapiro.test(x)
  p <- ggplot(data.frame(x = x), aes(sample = x)) +
    stat_qq(color = "darkblue", alpha = 0.6) + stat_qq_line(color = "red") +
    theme_minimal() + labs(title = "Normal Q-Q Plot", subtitle = paste("Shapiro p-value:", round(res$p.value, 4)))
  print(p)
  return(res)
}
```
