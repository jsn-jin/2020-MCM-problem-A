#***************************************************************************
# Using ARIMA Model to Forecast SST over the Next 50 Years
# Dataset: 
#    	"annual_avg_sst_150.csv"      	1870-2019, annually average
# Output:
#	 	"forecast_sst_50.csv"		  	2020-2069, forecasted annually SST
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library(forecast)
library(stats)
library(tis)
library(trend)
library(tseries)
library(fpp2)

# Import data
annual_avg_sst = read.csv("annual_avg_sst_150.csv", row.names = 1)

# ------------------------ Time Series Plot ------------------------ #

first_block = ts(annual_avg_sst[,1], start = 1870, end = 2019)

plot(first_block, ylab = "Annually Average SST", xlab = "Time", lwd = 2, 
col = 'skyblue3', main = "SST in Block 1 over the Past 150 Years")
legend(x = "topleft", legend = "Seasonal Adj. Data", 
col = "skyblue3", lwd = 2, inset = 0.05)


# Moving average plot
autoplot(first_block) +
  autolayer(ma(first_block, 11), series = "10 yr MA") +
  autolayer(ma(first_block, 21), series = "20 yr MA") +
  autolayer(ma(first_block, 31), series = "30 yr MA") +
  xlab("Time") + 
  ylab("SST")


# ------------------------- Model Selection ------------------------ #

first_block = ts(annual_avg_sst[,1], start = 1870, end = 2019)

tsdisplay(first_block, main = "ACF and PACF of the SST in Block 1")

adf.test(first_block, k = 1)

# ARIMA(1,0,1)
arima101 = Arima(first_block, order = c(1,0,1), include.drift = TRUE)
plot(forecast(arima101, 50)) 
lines(arima101$fitted, col = "red")

# ARIMA(3,0,0)
arima300 = Arima(first_block, order = c(3,0,0), include.drift = TRUE)
plot(forecast(arima300, 50))) 
lines(arima300$fitted, col = "red")

# ARIMA(2,0,1)
arima201 = Arima(first_block, order = c(2,0,1), include.drift = TRUE)
plot(forecast(arima201, 50))
lines(arima201$fitted, col = "red")

# ARIMA(1,0,2)
arima102 = Arima(first_block, order = c(1,0,2), include.drift = TRUE)
plot(forecast(arima102, 50))
lines(arima102$fitted, col = "red")

## ------------------- Out of Sample Evaluation ------------------- ##

training <- subset(first_block, end=length(first_block)-10)
test <- subset(first_block, start=length(first_block)-9)

train.101 <- Arima(training, order = c(1,0,1), include.drift = TRUE)
train.101 %>%
  forecast(h = 10) %>%
  autoplot() + autolayer(test) + 
  ggtitle("Out-of-Sample Evaluation ARIMA(1,0,1)")

train.300 <- Arima(training, order = c(3,0,0), include.drift = TRUE)
train.300 %>%
  forecast(h = 10) %>%
  autoplot() + autolayer(test) + 
  ggtitle("Out-of-Sample Evaluation ARIMA(3,0,0)")

train.201 <- Arima(training, order = c(2,0,1), include.drift = TRUE)
train.201 %>%
  forecast(h = 10) %>%
  autoplot() + autolayer(test) + 
  ggtitle("Out-of-Sample Evaluation ARIMA(2,0,1)")

train.102 <- Arima(training, order = c(1,0,2), include.drift = TRUE)
train.102 %>%
  forecast(h = 10) %>%
  autoplot() + autolayer(test) + 
  ggtitle("Out-of-Sample Evaluation ARIMA(1,0,2)")

accuracy(forecast(train.101), test)
accuracy(forecast(train.300), test)
accuracy(forecast(train.201), test)
accuracy(forecast(train.102), test)

# ------------------- Forecast for all 300 blocks ------------------ #

ts.plot(annual_avg_sst)

forecast = matrix(nrow = 50, ncol = 300)

for (b in 1:300) {

  # Get the data at the b-th block: first@(-16.5, 62.5), last@(2.5, 48.5)
  past_150_yr = annual_avg_sst[,b]
  
  if (is.na(past_150_yr[1])){
    forecast[,b] = rep(NA, 50)
    
  } else {
    fit = Arima(past_150_yr, order = c(3,0,0), include.drift = TRUE)
    forecast[,b] = forecast(fit, 50)$mean
  }
}

colnames(forecast) = loc[[1]]
rownames(forecast) = seq(2020, 2069)
write.csv(forecast, file = "forecast_sst_50.csv")







