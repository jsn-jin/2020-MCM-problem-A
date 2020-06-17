#***************************************************************************
# WORST CASE MODEL FOR HERRING
# Dataset: 
#     "forecast_sst_50_t.csv"      forecasted data from 2020-2069, transposed
# Author: Daphne Chen
#***************************************************************************
# Worst case sst range parameter: 11.05-11.25 C
#***************************************************************************

# library needed packages
library(tidyverse)
library(geosphere)

set.seed(88)
#test <- function() {
# import non-forecasted data first
sst = read_csv("forecast_sst_50_t.csv", col_names=TRUE)
head(sst)

# data frame to keep track of each run of the model
worst_case_herr_df <- data.frame(matrix(NA, nrow = 100, ncol = 51))
# track what year fish leave "within reach"
year_out <- rep(NA, times = 100)

left_edge <- seq(from=1, to=300, by=20)
right_edge <- seq(from=20, to=300, by=20)
#within <- c(53:59, 73:79, 94:99, 115:119, 136:139, 157:159, 178, 179, 199)
within <- c(95, 96, 97, 115, 116, 117, 135, 136, 137, 157)

for(i in 1:100) {
  worst_case_herr_df[i, 1] <- sst[[116, 1]]
  # start block index and year
  current_block <- 116 # 57.5, -1.5
  current_year <- 2020
  prev_block <- 0
  first_yr_out <- TRUE
  
  j <- 2
  while(current_year < 2070 & current_block > 0) {
    
    if(first_yr_out) {
      # port coordinate
      #start_loc <- c(-2.5, 56.5)
      
      #xy <- str_split(sst[[current_block, 1]], " ")
      #latitude <- as.numeric(xy[[1]][1])
      #longitude <- as.numeric(xy[[1]][2])
      #coord <- c(longitude, latitude)
      
      #d_fr_port <- distm(start_loc, coord, fun = distHaversine)/1000
      
      #if(d_fr_port > 520) {
        #year_out[i] <- current_year
        #first_yr_out <- FALSE
      #}
      
      if(!(current_block %in% within) & first_yr_out) {
        year_out[i] <- current_year
        first_yr_out <- FALSE
      }
      
    }
    
    # check if current location's temperature is still within habitable range
    current_block_temp <- sst[[current_block, as.character(current_year)]]
    if(11.05 <= current_block_temp & current_block_temp <= 11.25) {
      current_year <- current_year + 1
      worst_case_herr_df[i, j] <- sst[[current_block, 1]]
      j <- j+1
      next
    }
    
    # corresponding indices for surrounding blocks
    north <- current_block-20
    west <- current_block-1
    south <- current_block+20
    east <- current_block+1
    northwest <- current_block-21
    southwest <- current_block+19
    southeast <- current_block+21
    northeast <- current_block-19
    
    northern <- c(northwest, north, northeast)
    
    
    # vector to keep track of which adjacent blocks are within temperature range
    if(current_block %in% left_edge) {
      adjacent <- c(north, south, east, southeast, northeast)
      northern <- c(north, northeast)
    } else if(current_block %in% right_edge) {
      adjacent <- c(north, west, south, northwest, southwest)
      northern <- c(north, northwest)
    } else {
      adjacent <- c(north, west, south, east, northwest, southwest, southeast, northeast)
    }
    
    ncopy <- northern
    for(d in northern) {
      if(d <= 0 || is.na(sst[[d,2]])) {
        ncopy <- ncopy[ncopy != d]
      }
    }
    # average temperature of range in the case that none of the block are within range
    avg_temp <- 11.15
    # best option if no blocks are within temperature range
    best <- abs(avg_temp - current_block_temp)
    # initialize what the index of the new block to be migrated to
    new_block <- current_block
    adjacent_copy <- adjacent
    stoc_a <- adjacent
    
    # check whether we have data for each direction AND if the block is above or below threshold temperature
    for(direction in adjacent) {
      
      
      # if block is out of range (no data) or not within temperature range, remove the option from the adjacent vector
      if(direction > 300 || direction < 1) {
        adjacent_copy <- adjacent_copy[adjacent_copy != direction]
        stoc_a <- stoc_a[stoc_a != direction]
        next
      }
      
      temp <- sst[[direction, as.character(current_year)]]
      
      if(is.na(temp)) {
        adjacent_copy <- adjacent_copy[adjacent_copy != direction]
        stoc_a <- stoc_a[stoc_a != direction]
        next
      }
      
      
      if(!(11.05 <= temp & temp <= 11.25)) {
        if(abs(avg_temp - temp) < best) {
          best <- abs(avg_temp - temp)
          new_block <- direction
        }
        if(new_block == current_block && length(ncopy) > 0) {
          if(length(ncopy) > 1) {
            new_block <- sample(ncopy, 1)
          } else {
            new_block <- ncopy
          }
        }
        adjacent_copy <- adjacent_copy[adjacent_copy != direction]
      }
    }
    
   
    # of the blocks that are in temperature range, choose one at random to migrate to
    if(length(adjacent_copy) > 1) {
      new_block <- sample(adjacent_copy, 1)
    }
    if(length(adjacent_copy) == 1) {
      new_block <- adjacent_copy
    }
    
    # if new block is a previous block and not in temp range, move north
    if(new_block == prev_block) {
      if(!(11.05 <= current_block_temp & current_block_temp <= 11.25) && length(ncopy) > 0) {
        if(length(ncopy) > 1) {
          new_block <- sample(ncopy, 1)
        } else {
          new_block <- ncopy
        }
      }
    }
    
    prob_v <- c(0.5, rep(0.5, times=length(stoc_a)))
    new_block <- sample(c(new_block, stoc_a), size=1, prob=prob_v)
    
    
    # increment year
    current_year <- current_year + 1
    prev_block <- current_block
    current_block <- new_block
    
    # migration_pat <- c(migration_pat, sst[[new_block, 1]])
    worst_case_herr_df[i, j] <- sst[[new_block, 1]]
    j <- j + 1
  }
  
}
#}
write.csv(worst_case_herr_df, file = "wc_herring.csv")
#write.csv(year_out, file = "wch_years.csv")
write.csv(year_out, file = "wch_years.csv")
