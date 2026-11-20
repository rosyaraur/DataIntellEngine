# Function: enhanced_gwas_manhattan_mock

## Description
Simulates GWAS p-values and generates a Manhattan plot to visualize genome-wide significance.

## Arguments
n_snps (integer): Number of SNPs to simulate.
n_chroms (integer): Number of chromosomes.

## R Code
```r
library(ggplot2)
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
}
```
