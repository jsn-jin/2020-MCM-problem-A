#***************************************************************************
# Test model for fish migration
# NOTE: loop has NOT been implemented yet, only models for the current year
# Dataset: 
#     "scot_sst.csv"      1970-2019, monthly data
#***************************************************************************

# import non-forcasted data first
sst = read_csv("scot_sst.csv")
head(sst)

# start block index and year
start_block <- 262
start_year <- 1970

# corresponding indices for surrounding blocks
north <- start_block-20
west <- start_block-1
south <- start_block+20
east <- start_block+1
northwest <- start_block-21
southwest <- start_block+19
southeast <- start_block+21
northeast <- start_block-18

# vector to keep track of which adjacent blocks are within temperature range
adjacent <- c(north, west, south, east, northwest, southwest, southeast, northeast)

# average temperature of range in the case that none of the block are within range
avg_temp <- 10.09
# current year
current_year <- start_year
# best option if no blocks are within temperature range
best <- NA
# initialize what the index of the new block to be migrated to
new_block <- NA
adjacent_copy <- adjacent

# check whether we have data for each direction AND if the block is above or below threshold temperature
for(direction in adjacent) {
  temp <- sst[[direction, as.character(current_year)]]
  
  # if block is out of range (no data) or not within temperature range, remove the option from the adjacent vector
  if(direction > 300 || direction < 0 || !(9.98 <= temp & temp <= 10.2)) {
    best <- abs(10.09 - temp)
    new_block <- direction
    adjacent_copy <- adjacent_copy[-1]
  }
  
}

# of the blocks that are in temperature range, choose one at random to migrate to
set.seed(88)
if(length(adjacent_copy) >= 1) {
  new_block <- sample(adjacent_copy, 1)
}

new_block


