#***************************************************************************
# Data Wrangling
# Dataset: 
#     "scot_sst_aug.csv"  1970-2019, monthly data with location information
#     "scot_sst_150.csv"  1870-2019, monthly data
# Author: Hao Jin
#***************************************************************************

library(tidyverse)

sst_150 = read_csv("scot_sst_150.csv") # 1870-2019, monthly data
sst_150 = t(sst_150) # transpose
sst_150_ts <- ts(sst_150, start = c(1870,1), end = c(2019,12), freq = 12)

# Get annually data (the average of 12 months)
annual_avg_sst = matrix(nrow = 150, ncol = 300) # to store annually average results

for (b in 1:300) { # for each block
  
  # Prepare a vector
  b.annually = numeric(150)
  
  # Get the data at the b-th block: first@(-16.5, 62.5), last@(2.5, 48.5)
  b.monthly = sst_150_ts[,b]
  
  # Calculate annually mean 
  for (i in 0:149) {
    annual_sum = 0
    for (j in 1:12) {
      annual_sum = annual_sum + b.monthly[12 * i + j]
    }
    b.annually[i+1] = annual_sum / 12
  }
  
  annual_avg_sst[,b] = b.annually
}

annual_avg_sst = ts(annual_avg_sst, start = c(1870, 1), freq = 12)

# Get location information
location_info = read_csv("scot_sst_aug.csv")
lon = location_info[[1]]
lat = location_info[[2]]
loc = character(300)

for (i in 1:300) {
  loc[i] = paste(as.character(lat[i]), as.character(lon[i]))
}

rownames(annual_avg_sst) = seq(1870, 2019)
write.csv(annual_avg_sst, file = "annual_avg_sst_150.csv")

write.csv(loc, file = "loc.csv")
