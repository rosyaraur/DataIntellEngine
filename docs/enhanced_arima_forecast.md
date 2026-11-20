# Function: enhanced_arima_forecast

## Description
Fits an auto.arima model to a time series and plots the forecast with confidence intervals.

## Arguments
ts_data (ts object): A time series object.
h (integer): Number of periods to forecast.

## R Code
```r
library(forecast)
enhanced_arima_forecast <- function(ts_data, h = 10) {
  fit <- auto.arima(ts_data)
  fc <- forecast(fit, h = h)
  plot(fc, main = "ARIMA Forecast", ylab = "Value", xlab = "Time")
  return(fit)
}
```
