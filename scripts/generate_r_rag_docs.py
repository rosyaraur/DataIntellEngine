import os

# Create the docs directory if it doesn't exist
os.makedirs("docs", exist_ok=True)

# Define the 20 enhanced statistical functions
functions = [
    {
        "name": "enhanced_summary_stats",
        "desc": "Calculates summary statistics for a numeric variable and generates a boxplot with overlaid jitter points.",
        "args": "data (data.frame): The dataset.\nvar_name (character): The name of the numeric variable.",
        "code": """library(ggplot2)
enhanced_summary_stats <- function(data, var_name) {
  stats <- summary(data[[var_name]])
  p <- ggplot(data, aes(x = "", y = .data[[var_name]])) +
    geom_boxplot(fill = "lightblue", outlier.shape = NA) +
    geom_jitter(width = 0.2, alpha = 0.5, color = "darkblue") +
    theme_minimal() + labs(title = paste("Summary of", var_name), x = "", y = var_name)
  print(p)
  return(stats)
}"""
    },
    {
        "name": "enhanced_ttest",
        "desc": "Performs a two-sample t-test and visualizes the distributions using a violin plot.",
        "args": "data (data.frame): The dataset.\nformula (formula): A formula of the form y ~ x.",
        "code": """library(ggplot2)
enhanced_ttest <- function(data, formula) {
  res <- t.test(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[2], y = vars[1], fill = vars[2])) +
    geom_violin(trim = FALSE, alpha = 0.7) +
    geom_boxplot(width = 0.1, fill = "white") +
    theme_minimal() + labs(title = paste("T-Test:", vars[1], "by", vars[2]))
  print(p)
  return(res)
}"""
    },
    {
        "name": "enhanced_anova",
        "desc": "Fits an ANOVA model, performs Tukey HSD post-hoc test, and plots the 95% confidence intervals of differences.",
        "args": "data (data.frame): The dataset.\nformula (formula): A formula of the form y ~ group.",
        "code": """enhanced_anova <- function(data, formula) {
  model <- aov(formula, data = data)
  tukey <- TukeyHSD(model)
  plot(tukey, col = "red", las = 1)
  return(list(ANOVA = summary(model), Tukey = tukey))
}"""
    },
    {
        "name": "enhanced_linear_regression",
        "desc": "Fits a simple linear regression and outputs a scatter plot with the fitted regression line and confidence interval.",
        "args": "data (data.frame): The dataset.\nformula (formula): A formula of the form y ~ x.",
        "code": """library(ggplot2)
enhanced_linear_regression <- function(data, formula) {
  model <- lm(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[2], y = vars[1])) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", fill = "pink") +
    theme_minimal() + labs(title = "Linear Regression Fit")
  print(p)
  return(summary(model))
}"""
    },
    {
        "name": "enhanced_logistic_regression",
        "desc": "Fits a logistic regression model and plots the predicted probabilities against the predictor.",
        "args": "data (data.frame): The dataset.\nformula (formula): A formula where response is binary (0/1).",
        "code": """library(ggplot2)
enhanced_logistic_regression <- function(data, formula) {
  model <- glm(formula, data = data, family = "binomial")
  vars <- all.vars(formula)
  data$predicted <- predict(model, type = "response")
  p <- ggplot(data, aes_string(x = vars[2], y = "predicted")) +
    geom_point(aes_string(y = vars[1]), alpha = 0.3, shape = 1) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "blue") +
    theme_minimal() + labs(title = "Logistic Regression Curve")
  print(p)
  return(summary(model))
}"""
    },
    {
        "name": "enhanced_correlation_matrix",
        "desc": "Computes a Pearson correlation matrix for numeric columns and visualizes it as a heatmap.",
        "args": "data (data.frame): The dataset containing numeric columns.",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_pca",
        "desc": "Performs Principal Component Analysis (PCA) and generates a scree plot showing variance explained.",
        "args": "data (data.frame): The numeric dataset.",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_kmeans",
        "desc": "Performs K-Means clustering and plots the results on the first two principal components.",
        "args": "data (data.frame): The numeric dataset.\ncenters (integer): The number of clusters (k).",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_survival_km",
        "desc": "Fits a Kaplan-Meier survival curve and plots it with risk tables and confidence intervals.",
        "args": "data (data.frame): Dataset containing survival time and status.\nformula (formula): A survival formula like Surv(time, status) ~ group.",
        "code": """library(survival)
library(survminer)
enhanced_survival_km <- function(data, formula) {
  fit <- survfit(formula, data = data)
  p <- ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE,
                  risk.table = TRUE, ggtheme = theme_minimal(),
                  title = "Kaplan-Meier Survival Estimate")
  print(p)
  return(fit)
}"""
    },
    {
        "name": "enhanced_mixed_model",
        "desc": "Fits a linear mixed-effects model (LMM) and generates a caterpillar plot for random effects.",
        "args": "data (data.frame): The dataset.\nformula (formula): Formula suitable for lme4 (e.g., y ~ x + (1|random_factor)).",
        "code": """library(lme4)
library(lattice)
enhanced_mixed_model <- function(data, formula) {
  model <- lmer(formula, data = data)
  re <- ranef(model)
  print(dotplot(re)) # Creates the caterpillar plot
  return(summary(model))
}"""
    },
    {
        "name": "enhanced_chi_square",
        "desc": "Performs a Chi-Square test of independence and visualizes the contingency table using a mosaic plot.",
        "args": "var1 (vector): First categorical variable.\nvar2 (vector): Second categorical variable.",
        "code": """enhanced_chi_square <- function(var1, var2) {
  tbl <- table(var1, var2)
  res <- chisq.test(tbl)
  mosaicplot(tbl, shade = TRUE, main = "Mosaic Plot of Standardized Residuals", las = 1)
  return(res)
}"""
    },
    {
        "name": "enhanced_normality_test",
        "desc": "Performs the Shapiro-Wilk test for normality and generates a Q-Q plot with a reference line.",
        "args": "x (numeric vector): The numeric data to test.",
        "code": """library(ggplot2)
enhanced_normality_test <- function(x) {
  res <- shapiro.test(x)
  p <- ggplot(data.frame(x = x), aes(sample = x)) +
    stat_qq(color = "darkblue", alpha = 0.6) + stat_qq_line(color = "red") +
    theme_minimal() + labs(title = "Normal Q-Q Plot", subtitle = paste("Shapiro p-value:", round(res$p.value, 4)))
  print(p)
  return(res)
}"""
    },
    {
        "name": "enhanced_outlier_detection",
        "desc": "Identifies influential observations in a linear model using Cook's Distance and plots them.",
        "args": "model (lm object): A fitted linear model.",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_wilcoxon",
        "desc": "Performs a non-parametric Wilcoxon Rank Sum test and plots overlapping density curves.",
        "args": "data (data.frame): The dataset.\nformula (formula): A formula of the form numeric_var ~ binary_group.",
        "code": """library(ggplot2)
enhanced_wilcoxon <- function(data, formula) {
  res <- wilcox.test(formula, data = data)
  vars <- all.vars(formula)
  p <- ggplot(data, aes_string(x = vars[1], fill = vars[2])) +
    geom_density(alpha = 0.5) +
    theme_minimal() + labs(title = "Density Plot by Group (Wilcoxon Test)")
  print(p)
  return(res)
}"""
    },
    {
        "name": "enhanced_hierarchical_clustering",
        "desc": "Performs hierarchical clustering on numeric data and plots a standard dendrogram.",
        "args": "data (data.frame): Numeric dataset.\nmethod (character): Agglomeration method (e.g., 'complete', 'ward.D2').",
        "code": """enhanced_hierarchical_clustering <- function(data, method = "complete") {
  num_data <- scale(data[sapply(data, is.numeric)])
  dist_mat <- dist(num_data)
  hc <- hclust(dist_mat, method = method)
  plot(hc, main = paste("Cluster Dendrogram (", method, ")", sep=""), xlab = "", sub = "")
  return(hc)
}"""
    },
    {
        "name": "enhanced_missing_data",
        "desc": "Calculates the percentage of missing values per column and visualizes the missingness pattern.",
        "args": "data (data.frame): The dataset.",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_arima_forecast",
        "desc": "Fits an auto.arima model to a time series and plots the forecast with confidence intervals.",
        "args": "ts_data (ts object): A time series object.\nh (integer): Number of periods to forecast.",
        "code": """library(forecast)
enhanced_arima_forecast <- function(ts_data, h = 10) {
  fit <- auto.arima(ts_data)
  fc <- forecast(fit, h = h)
  plot(fc, main = "ARIMA Forecast", ylab = "Value", xlab = "Time")
  return(fit)
}"""
    },
    {
        "name": "enhanced_polynomial_regression",
        "desc": "Fits a polynomial regression model and plots the fitted non-linear curve over a scatter plot.",
        "args": "data (data.frame): The dataset.\ny (character): The response variable name.\nx (character): The predictor variable name.\ndegree (integer): The degree of the polynomial.",
        "code": """library(ggplot2)
enhanced_polynomial_regression <- function(data, y, x, degree = 2) {
  formula <- as.formula(paste(y, "~ poly(", x, ",", degree, ", raw=TRUE)"))
  model <- lm(formula, data = data)
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point(alpha = 0.5) +
    stat_smooth(method = "lm", formula = y ~ poly(x, degree, raw=TRUE), color = "purple") +
    theme_minimal() + labs(title = paste("Polynomial Regression (Degree", degree, ")"))
  print(p)
  return(summary(model))
}"""
    },
    {
        "name": "enhanced_bootstrap_mean",
        "desc": "Performs bootstrapping to estimate the mean and 95% CI, plotting the bootstrap distribution histogram.",
        "args": "x (numeric vector): The numeric data.\nB (integer): Number of bootstrap replicates.",
        "code": """library(ggplot2)
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
}"""
    },
    {
        "name": "enhanced_gwas_manhattan_mock",
        "desc": "Simulates GWAS p-values and generates a Manhattan plot to visualize genome-wide significance.",
        "args": "n_snps (integer): Number of SNPs to simulate.\nn_chroms (integer): Number of chromosomes.",
        "code": """library(ggplot2)
enhanced_gwas_manhattan_mock <- function(n_snps = 5000, n_chroms = 5) {
  data <- data.frame(
    SNP = paste0("rs", 1:n_snps),
    CHR = sample(1:n_chroms, n_snps, replace = TRUE),
    BP = sample(1:1000000, n_snps, replace = TRUE),
    P = runif(n_snps, min = 1e-8, max = 1)
  )
  # Inject a few significant hits
  data$P[sample(1:n_snps, 5)] <- runif(5, min = 1e-10, max = 5e-8)
  data$logP <- -log10(data$P)
  
  p <- ggplot(data, aes(x = BP, y = logP, color = as.factor(CHR))) +
    geom_point(alpha = 0.6, size = 1.5) +
    geom_hline(yintercept = -log10(5e-8), color = "red", linetype = "dashed") +
    facet_grid(. ~ CHR, scales = "free_x", space = "free_x") +
    theme_minimal() + theme(legend.position = "none", axis.text.x = element_blank()) +
    labs(title = "Mock Manhattan Plot", x = "Chromosome", y = "-log10(P-value)")
  print(p)
  return(head(data[order(data$P), ]))
}"""
    }
]

# Generate the files
for idx, func in enumerate(functions):
    filename = f"docs/{func['name']}.md"
    with open(filename, "w") as f:
        f.write(f"# Function: {func['name']}\n\n")
        f.write(f"## Description\n{func['desc']}\n\n")
        f.write(f"## Arguments\n{func['args']}\n\n")
        f.write(f"## R Code\n```r\n{func['code']}\n```\n")

print("Successfully generated 20 enhanced R documentation files in the 'docs' directory.")
