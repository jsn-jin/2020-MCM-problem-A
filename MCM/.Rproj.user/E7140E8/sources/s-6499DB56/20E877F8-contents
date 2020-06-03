#***************************************************************************
# Using ARIMA Model to Forecast SST over the Next 50 Years
# Dataset: 
#     "annual_avg_sst_150.csv"      1870-2019, annually average
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library(forecast)

annual_avg_sst = read.csv("annual_avg_sst.csv", row.names = 1)
loc = read.csv("loc.csv", row.names = 1)
colnames(annual_avg_sst) = loc[[1]]



## ------------------------ Model Selection ------------------------ ##

first_block = ts(annual_avg_sst[,1], start = 1870, end = 2019)

tsdisplay(first_block) # AR(3)

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







