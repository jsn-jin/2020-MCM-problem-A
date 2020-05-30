#***************************************************************************
# Using ARIMA Model to Forecast SST over the Next 50 Years
# Dataset: 
#     "scot_sst.csv"      1970-2019, monthly data
#     "scot_sst_150.csv"  1870-2019, monthly data
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library(forecast)

# Load 50 Years Dataset (from 1970-2019, monthly)
sst = read_csv("scot_sst.csv")
head(sst)

# Transpose
sst = t(sst)
head(sst) # now, each row is a time, each col is a block

# Sanity check
plot(sst[,1], type = "l") # seasonal pattern observed, the data is good to go

# Time-Series
sst_ts <- ts(sst, start = c(1970,1), end = c(2019,12), freq = 12)

ts.plot(sst_ts)

forecast(sst_ts, n.ahead = 600)

plot(forecast(HoltWinters(sst_ts[,1]), 120))

tsdisplay(sst_ts[,1])


# ---------- Forecast using Monthly Data (50 Year, Monthly Data) ---------- #
auto = auto.arima(sst_ts[,1])

arima_model1 = Arima(sst_ts[,1], order = c(0, 0, 1), seasonal = c(1, 1, 0))
plot(forecast(arima_model1, 600))

arima_model2 = Arima(sst_ts[,1], order = c(0, 0, 1), seasonal = c(0, 1, 1))
plot(forecast(arima_model2, 600))

accuracy(forecast(arima_model1))
accuracy(forecast(arima_model2))



# ---- Forecast using Seasonal Adjusted Data (150 Year, Annually Data) ---- #
sst_150 = read_csv("scot_sst_150.csv") # 1870-2019, monthly data
sst_150 = t(sst_150) # transpose
sst_150_ts <- ts(sst_150, start = c(1870,1), end = c(2019,12), freq = 12)

# Prepare a vector
annual_1 = numeric(150)
# Get the data at the first block (-16.5, 62.5)
sst_150_1 = sst_150_ts[,1]

# Calculate annual mean, so we get annually average 
for (i in 0:149) {
  annual_sum = 0
  for (j in 1:12) {
    annual_sum = annual_sum + sst_150_1[12*i + j]
  }
  annual_1[i+1] = annual_sum / 12
}

annual_1_ts = ts(annual_1, start = 1870, end = 2019, freq = 1)

plot(annual_1_ts, type = "l")

tsdisplay(annual_1_ts) # AR(3)

tsdisplay(diff(annual_1_ts))

## ------------------------ Model Selection ------------------------ ##

arima_model3 = Arima(annual_1_ts, order = c(3,1,0), include.drift = TRUE) 

plot(forecast(arima_model3, 50)) 

arima_model4 = Arima(annual_1_ts, order = c(2,1,0), include.drift = TRUE)

plot(forecast(arima_model4, 50))

arima_model5 = Arima(annual_1_ts, order = c(1,1,0), include.drift = TRUE)

plot(forecast(arima_model5, 50))

accuracy(arima_model3) # Best Model
accuracy(arima_model4)
accuracy(arima_model5)



# ------------------ Forecast for all 300 blocks ------------------ #

# TODO



# ----------------------- SST Visualization ----------------------- #

library('plot.matrix')

# Example
x <- matrix(runif(35), ncol=5) # create a numeric matrix object
x
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(x)

# Actual Data
test = matrix(sst_150_ts[1800,], nrow = 15, ncol = 20, byrow = TRUE)
colnames(test) = seq(-16.5, 2.5, 1)
rownames(test) = seq(62.5, 48.5, -1)
par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(test, xlab = "Longitude", ylab = 'Latitude', main = "Dec. 2019 SST", breaks = seq(7,13,0.1),  col = topo.colors)

min(sst_150_ts, na.rm = TRUE)
# [1] 2.90923
max(sst_150_ts, na.rm = TRUE)
# [1] 19.96833






