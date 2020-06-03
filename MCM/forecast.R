#***************************************************************************
# Using ARIMA Model to Forecast SST over the Next 50 Years
# Dataset: 
#     "annual_avg_sst_150.csv"      1870-2019, annually average
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library(forecast)
library(stats)
library(tis)
library(trend)
library(tseries)

annual_avg_sst = read.csv("annual_avg_sst_150.csv", row.names = 1)
loc = read.csv("loc.csv", row.names = 1)
colnames(annual_avg_sst) = loc[[1]]

first_block = ts(annual_avg_sst[,1], start = 1870, end = 2019)

## --------------------- Time Series Plot, Trend --------------------- ##

jpeg("block_1_ts.jpg", width = 600, height = 600)

plot(first_block, ylab = "Annually Average SST", xlab = "Time", lwd = 2, col = 'skyblue3', main = "Sea Surface Temperature in the Past 150 Years at Block 1")
abline(h = mean(first_block), col = "black", lwd = 2)
legend(x = "topleft", legend = "Observed Data", col = "skyblue3", lwd = 2, inset = 0.05)

dev.off()

# The null hypothesis for this test is that there is no monotonic trend in the series.
# The alternate hypothesis is that a trend exists. This trend can be positive, negative, or non-null.

mk.test(first_block)

t = seq(1870, 1870 + length(first_block)-1)
t1 = lm(first_block ~ t) 

jpeg("block_1_trend.jpg", width = 600, height = 600)

plot(first_block, ylab = "Annually Average SST", xlab = "Time", lwd = 2, col = 'skyblue3', main = "Sea Surface Temperature in the Past 150 Years at Block 1")
lines(t, t1$fit, col = "red3", lwd = 2)
legend(x = "topleft", legend = c("Observed Data", "Fitted Trend"), col = c("skyblue3", "red3"), lty = c(1,1), lwd = c(2,2), inset = 0.05)

dev.off()



## ------------------------ Model Selection ------------------------ ##

jpeg("block_1_acf.jpg", width = 600, height = 600)
tsdisplay(first_block, main = "Time Series, ACF, and PACF of the SST of the First Block") # AR(3)
dev.off()

arima310 = Arima(first_block, order = c(3,1,0), include.drift = TRUE)

plot(forecast(arima310, 50)) 

forecast(arima310, 50)$mean

arima210 = Arima(first_block, order = c(2,1,0), include.drift = TRUE)

plot(forecast(arima210, 50))

arima110 = Arima(first_block, order = c(1,1,0), include.drift = TRUE)

plot(forecast(arima110, 50))

accuracy(arima310) # Best Model
accuracy(arima210)
accuracy(arima110)





# ------------------ Forecast for all 300 blocks ------------------ #

ts.plot(annual_avg_sst)

forecast = matrix(nrow = 50, ncol = 300)

for (b in 1:300) {

  # Get the data at the b-th block: first@(-16.5, 62.5), last@(2.5, 48.5)
  past_150_yr = annual_avg_sst[,b]
  
  if (is.na(past_150_yr[1])){
    forecast[,b] = rep(NA, 50)
    
  } else {
    fit = Arima(past_150_yr, order = c(3,1,0), include.drift = TRUE)
    forecast[,b] = forecast(fit, 50)$mean
  }
}

colnames(forecast) = loc[[1]]
rownames(forecast) = seq(2020, 2069)
write.csv(forecast, file = "forecast_sst_50.csv")







