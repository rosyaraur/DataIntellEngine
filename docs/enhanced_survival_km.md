# Function: enhanced_survival_km

## Description
Fits a Kaplan-Meier survival curve and plots it with risk tables and confidence intervals.

## Arguments
data (data.frame): Dataset containing survival time and status.
formula (formula): A survival formula like Surv(time, status) ~ group.

## R Code
```r
library(survival)
library(survminer)
enhanced_survival_km <- function(data, formula) {
  fit <- survfit(formula, data = data)
  p <- ggsurvplot(fit, data = data, pval = TRUE, conf.int = TRUE,
                  risk.table = TRUE, ggtheme = theme_minimal(),
                  title = "Kaplan-Meier Survival Estimate")
  print(p)
  return(fit)
}
```
