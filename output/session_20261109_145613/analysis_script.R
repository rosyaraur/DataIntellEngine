# Load necessary libraries
library(ggplot2)
library(tidyverse)

# Load the dataset
data <- read.csv('./data.csv')

# Perform a t-test to compare the values between group A and group B
t_test_result <- t.test(data$group, data$value)

# Print the t-test result
print(paste("t-test result: t = ", t_test_result$statistic, ", p = ", t_test_result$p.value))

# Generate a visualization of the difference
ggplot(data, aes(x = group, y = value)) +
  geom_boxplot(width = 0.5, fill = "lightblue") +
  geom_point(color = "black") +
  geom_vline(xintercept = mean(data$value), color = "red", size = 0.5) +
  theme_minimal() + labs(title = "Boxplot of Value by Group", subtitle = "t-test result") +
  ggsave('./output/session_20261109_145613/boxplot_t_test.png', width = 8, height = 6)