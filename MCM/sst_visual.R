#***************************************************************************
# Visualize Predicted SST
# Dataset: 
#     "forecast_sst_50.csv"      2020-2069 
# Author: Hao Jin
#***************************************************************************

# ----------------------- SST Visualization ----------------------- #

library(tidyverse)
library('plot.matrix')
library('LSD')

forecast = read.csv("forecast_sst_50.csv", row.names = 1)
loc = read.csv("loc.csv", row.names = 1)
colnames(forecast) = loc[[1]]

# Example
x <- matrix(runif(35), ncol=5) # create a numeric matrix object
x
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(x, col = topo.colors)

# Actual Data
sst_2029 = matrix(as.numeric(forecast[10,]), nrow = 15, ncol = 20, byrow = TRUE)
colnames(sst_2029) = seq(-16.5, 2.5, 1)
rownames(sst_2029) = seq(62.5, 48.5, -1)
par(mar = c(5.1, 4.1, 4.1, 4.1))
plot(sst_2029, xlab = "Longitude", ylab = 'Latitude', main = "2029 SST", breaks = seq(8,15,0.2), col = topo.colors)

min(forecast, na.rm = TRUE)

max(forecast, na.rm = TRUE)

