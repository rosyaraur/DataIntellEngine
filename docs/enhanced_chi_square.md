# Function: enhanced_chi_square

## Description
Performs a Chi-Square test of independence and visualizes the contingency table using a mosaic plot.

## Arguments
var1 (vector): First categorical variable.
var2 (vector): Second categorical variable.

## R Code
```r
enhanced_chi_square <- function(var1, var2) {
  tbl <- table(var1, var2)
  res <- chisq.test(tbl)
  mosaicplot(tbl, shade = TRUE, main = "Mosaic Plot of Standardized Residuals", las = 1)
  return(res)
}
```
