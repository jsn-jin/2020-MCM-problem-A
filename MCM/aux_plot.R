#***************************************************************************
# Some TS plots of SST data in the past
# Dataset: 
#		"scot_sst_150.csv"      	1870-2019, Monthly SST data
#		"annual_avg_sst_150.csv"	1870-2019, Anually SST data (Average of 12 Months)
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library(forecast)
library(stats)
library(tis)
library(trend)
library(tseries)

sst_150 = read_csv("scot_sst_150.csv")

sst_150_ts = ts(t(sst_150), start = c(1870,1), end = c(2019,12), freq = 12)

sst_5_ts = ts(sst_150_ts[1741:1800,], start = c(2015,1), end = c(2019,12), freq = 12)

annual_avg_sst = read.csv("annual_avg_sst_150.csv", row.names = 1)

first_block = ts(annual_avg_sst[,1], start = 1870, end = 2019)

# ------------------------- Time Series Plot, Trend ------------------------- #

## ---------------- Plot 1 All Blocks, 150 Years, Monthly ---------------- ##

png("all_blocks_past_150.png", 1200, 600)

ts.plot(sst_150_ts, col = "steelblue2", lwd = 0.5, main = "Monthly SST from 1870 to 2019 of All 300 Grids", cex.main = 2, cex.lab = 2)

dev.off()

## ------------------ Plot 2 All Block, 5 Years, Monthly ------------------ ##

png("all_blocks_past_5.png", 1200, 600)

ts.plot(sst_5_ts, col = "steelblue2", lwd = 0.5, main = "Monthly SST from 2015 to 2019 of All 300 Grids", cex.main = 2, cex.lab = 2)

dev.off()


## ----------------- Plot 3 Block 1, 150 Years, Annually ----------------- ##

png("block_1_ts.png", width = 600, height = 600)

plot(first_block, ylab = "Annually Average SST", xlab = "Time", lwd = 2, col = 'skyblue3', main = "Sea Surface Temperature in the Past 150 Years at Block 1")
legend(x = "topleft", legend = "Seasonal Adj. Data", col = "skyblue3", lwd = 2, inset = 0.05)

dev.off()

## ------------------ Plot 4 block 1, 200 Months, Monthly ------------------ ##

png("block_1_ts_seasonal.jpg", width = 600, height = 600)

plot(sst_150_ts[1600:1800,1], type = "l", ylab = "Monthly SST", xlab = "Time", lwd = 2, col = 'skyblue3', main = "Sea Surface Temperature in the Past 150 Years at Block 1")
legend(x = "topleft", legend = "Observed Data", col = "skyblue3", lwd = 2, inset = 0.05)

dev.off()


## ------------- Plot 5 Trend, Block 1, 150 Years, Annually ------------- ##

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

## ------------- Plot 6 Detrend, First Block 150 Years, Annually ------------- ##

jpeg("block_1_stationary.jpg", width = 600, height = 600)

plot(diff(first_block), ylab = "Annually Change of SST", xlab = "Time", lwd = 2, col = 'skyblue3', main = "Change of SST in the Past 150 Years at Block 1")
legend(x = "topleft", legend = "Detrended Data", col = "skyblue3", lwd = 2, inset = 0.05)

dev.off()




