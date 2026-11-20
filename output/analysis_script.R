# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Load the dataset
data <- read.csv('./data.csv')

# Perform a t-test to compare the values between group A and group B
t_test_result <- t.test(data$group, data$value)

# Extract the results
t_test_result

# Generate a visualization of the difference
ggplot(data, aes(x = group, y = value)) +
  geom_boxplot(width = 0.5, fill = "lightblue") +
  geom_point(color = "black") +
  geom_vline(xintercept = mean(data$value), color = "red", size = 1) +
  geom_hline(yintercept = mean(data$value), color = "red", size = 1) +
  theme_minimal() + labs(title = "Boxplot of Value by Group", subtitle = "T-Test Results")

# Save the plot to a file
ggsave('./output/boxplot.png', width = 8, height = 6, dpi = 300)

# Print the t-test results
cat("t-test results:\n")
cat("t = ", t_test_result$statistic, "\n")
cat("df = ", t_test_result$df, "\n")
cat("p-value = ", t_test_result$p.value, "\n")