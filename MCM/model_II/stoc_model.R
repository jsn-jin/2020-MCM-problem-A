#***************************************************************************
# Stochastic model for fish migration
# NOTE: loop has NOT been implemented yet, only models for the current year
# Dataset: 
#     "scot_sst.csv"      1970-2019, monthly data
#***************************************************************************

# library needed packages
library(tidyverse)


set.seed(88)

# import non-forcasted data first
sst = read_csv("forecast_sst_50_t.csv", col_names=TRUE)
head(sst)

# start block index and year
current_block <- 38
current_year <- 2020

# vector to keep track of where fish move each year
migration_pat <- sst[[current_block, 1]]
i <- 1


while(current_year < 2070 & current_block > 0) {
  
  # check if current location's temperature is still within habitable range
  current_block_temp <- sst[[current_block, as.character(current_year)]]
  if(9.98 <= current_block_temp & current_block_temp <= 10.2) {
    current_year <- current_year + 1
    migration_pat <- c(migration_pat, sst[[current_block, 1]])
    break
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
  
  # vector to keep track of which adjacent blocks are within temperature range
  adjacent <- c(north, west, south, east, northwest, southwest, southeast, northeast)
  
  # average temperature of range in the case that none of the block are within range
  avg_temp <- 10.09
  # best option if no blocks are within temperature range
  best <- 100
  # initialize what the index of the new block to be migrated to
  new_block <- NA
  adjacent_copy <- adjacent
  
  # check whether we have data for each direction AND if the block is above or below threshold temperature
  for(direction in adjacent) {
    
    
    # if block is out of range (no data) or not within temperature range, remove the option from the adjacent vector
    if(direction > 300 || direction < 1) {
      adjacent_copy <- adjacent_copy[adjacent_copy != direction]
      next
    }
    temp <- sst[[direction, as.character(current_year)]]
    if(is.na(temp)) {
      adjacent_copy <- adjacent_copy[adjacent_copy != direction]
      next
    } else if(!(9.98 <= temp & temp <= 10.2)) {
      if(abs(10.09 - temp) < best) {
        best <- abs(10.09 - temp)
        new_block <- direction
        adjacent_copy <- adjacent_copy[adjacent_copy != direction]
      }
    }
  }
    
    
  
  

  # of the blocks that are in temperature range, choose one at random to migrate to
  if(length(adjacent_copy) >= 1) {
    new_block <- sample(adjacent_copy, 1)
  }
  
  # increment year
  current_year <- current_year + 1
  current_block <- new_block
  
  migration_pat <- c(migration_pat, sst[[new_block, 1]])
}

migration_pat


