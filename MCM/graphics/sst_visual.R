#***************************************************************************
# Visualize Predicted SST
# Dataset: 
#		"forecast_sst_50.csv"      	2020-2069
#		"bc_herring.csv"      		end coordinates of best case herring
#		"wc_herring.csv"      		end coordinates of worst case herring
#		"bc_mackerel.csv"      		end coordinates of best case mackerel
#		"wc_mackerel.csv"      		end coordinates of worst case mackerel    
# Author: Hao Jin
#***************************************************************************

# ----------------------- Forecasted SST Visualization ----------------------- #

library(tidyverse)
library('plot.matrix')
library(stringr)
library("viridis") 
library(RColorBrewer)        

forecast = read.csv("forecast_sst_50_t.csv", row.names = 1)

# Example
x <- matrix(runif(35), ncol=5) # create a numeric matrix object
x
par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
plot(x, col = topo.colors)

# Actual Data
sst_2020 = matrix(as.numeric(forecast[,1]), nrow = 15, ncol = 20, byrow = TRUE)
colnames(sst_2020) = seq(-16.5, 2.5, 1)
rownames(sst_2020) = seq(62.5, 48.5, -1)

png("2020_sst.png", 610, 541)
par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 5
plot(sst_2020, xlab = "Longitude", ylab = 'Latitude', main = "Forecasted SST for 2020", breaks = seq(8,15,0.2), col = topo.colors)
dev.off()

sst_2069 = matrix(as.numeric(forecast[,50]), nrow = 15, ncol = 20, byrow = TRUE)
colnames(sst_2069) = seq(-16.5, 2.5, 1)
rownames(sst_2069) = seq(62.5, 48.5, -1)

png("2069_sst.png", 610, 541)
par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 5
plot(sst_2069, xlab = "Longitude", ylab = 'Latitude', main = "Forecasted SST for 2069", breaks = seq(8,15,0.2), col = topo.colors)
dev.off()

min(forecast, na.rm = TRUE)

max(forecast, na.rm = TRUE)

years_of_interest = c(2020, 2024, 2029, 2034, 2039, 2044, 2049, 2054, 2059, 2064, 2069)
years_col = years_of_interest - 2019

for (n in years_col){
  
  sst = matrix(as.numeric(forecast[,n]), nrow = 15, ncol = 20, byrow = TRUE)
  colnames(sst) = seq(-16.5, 2.5, 1)
  rownames(sst) = seq(62.5, 48.5, -1)
  
  # Visualize
  file = paste("20", n+19, "_sst.png", sep = "")
  main = paste( "Forecasted SST for 20", n+19, sep = "")
  png(file, 610, 541)
  par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 5
  plot(sst, xlab = "Longitude", ylab = 'Latitude', main = main, breaks = seq(8,15,0.2), col = topo.colors)
  dev.off()
}



# ------------------ Forecasted Ending Point Visualization ------------------ #

## <--------------------------------------------------------------------------->
    ## Best Case, Herring
    ## 2019(starting)/2029/2039/2049/2059/2069

bc_herr = read_csv("bc_herring.csv", col_names = TRUE)
    
years_of_interest = c(2019, 2029, 2039, 2049, 2059, 2069)
years_col = years_of_interest - 2019 + 2

for (n in years_col){
	# Create empty vectors to store location (lat,lon) info
	latitude <- numeric(100)
	longitude <- numeric(100)

	for (i in seq_len(100)) {
		xy <- str_split(bc_herr[i,n], " ")
		latitude[i] <- as.numeric(xy[[1]][1])
		longitude[i] <- as.numeric(xy[[1]][2])
	}

	# Create empty vectors to store location (row,col) info
	row_num <- numeric(100)
	col_num <- numeric(100)

	for (i in seq_len(100)) {
		row_num[i] = 62.5 - latitude[i] + 1
		col_num[i] = longitude[i] + 16.5 + 1
	}

	# Create empty vectors to store location (block number) info

	# Up to down, left to right
	# First block: 62.5, -16.5
	# 300th block: 48.5, 2.5
	num <- numeric(100)

	for (i in seq_len(100)) {
		num[i] = 20 * row_num[i] + col_num[i]
	}

	table(num)

	# Create empty vectors to count ending block number
	counter = numeric(300)

	# Mark Land
	for (i in seq_len(300)) {
		if(is.na(forecast[i,11]))
		counter[i] = NA
	}

	for (block in num) {
		counter[block] = counter[block] + 1
	}

	# Convert counter to matrix form to be visualized on map
	loc_counter = matrix(counter, nrow = 15, ncol = 20, byrow = TRUE)

	colnames(loc_counter) = seq(-16.5, 2.5, 1)
	rownames(loc_counter) = seq(62.5, 48.5, -1)

	# Visualize
	file = paste("bch_map_20", n+17, ".png", sep = "")
	main = paste( "The Mostly Likely Location for Herring in 20", n+17, ", Best Case", sep = "")
	png(file, 610, 541)
	par(mar = c(5, 5, 5, 5)) # Set the margin on all sides to 5
	plot(loc_counter, xlab = "Longitude", ylab = 'Latitude', 
	main = main, 
	fmt.cell = '%d', col = plasma(20))
	dev.off()
}
  

    

## <--------------------------------------------------------------------------->
    ## Worst Case, Herring
    ## 2019(starting)/2029/2039/2049/2059/2069
    
wc_herr = read_csv("wc_herring.csv", col_names = TRUE)
    
years_of_interest = c(2019, 2029, 2039, 2049, 2059, 2069)
years_col = years_of_interest - 2019 + 2

for (n in years_col){
	# Create empty vectors to store location (lat,lon) info
	latitude <- numeric(100)
	longitude <- numeric(100)

	for (i in seq_len(100)) {
		xy <- str_split(wc_herr[i,n], " ")
		latitude[i] <- as.numeric(xy[[1]][1])
		longitude[i] <- as.numeric(xy[[1]][2])
	}

	# Create empty vectors to store location (row,col) info
	row_num <- numeric(100)
	col_num <- numeric(100)

	for (i in seq_len(100)) {
		row_num[i] = 62.5 - latitude[i] + 1
		col_num[i] = longitude[i] + 16.5 + 1
	}

	# Create empty vectors to store location (block number) info

	# Up to down, left to right
	# First block: 62.5, -16.5
	# 300th block: 48.5, 2.5
	num <- numeric(100)

	for (i in seq_len(100)) {
		num[i] = 20 * row_num[i] + col_num[i]
	}

	table(num)

	# Create empty vectors to count ending block number
	counter = numeric(300)

	# Mark Land
	for (i in seq_len(300)) {
	  if(is.na(forecast[i,11]))
		counter[i] = NA
	}

	for (block in num) {
		counter[block] = counter[block] + 1
	}

	# Convert counter to matrix form to be visualized on map
	loc_counter = matrix(counter, nrow = 15, ncol = 20, byrow = TRUE)

	colnames(loc_counter) = seq(-16.5, 2.5, 1)
	rownames(loc_counter) = seq(62.5, 48.5, -1)

	# Visualize
	
	file = paste("wch_map_20", n+17, ".png", sep = "")
	main = paste( "The Mostly Likely Location for Herring in 20", n+17, ", Worst Case", sep = "")
	png(file, 610, 541)
	par(mar = c(5, 5, 5, 5))
	plot(loc_counter, xlab = "Longitude", ylab = 'Latitude', 
	main = main, 
	fmt.cell = '%d', col = plasma(20))
	dev.off()
}



## <--------------------------------------------------------------------------->
    ## Best Case, Mackerel
    ## 2019(starting)/2029/2039/2049/2059/2069

bc_mack = read_csv("bc_mackerel.csv", col_names = TRUE)
    
years_of_interest = c(2019, 2029, 2039, 2049, 2059, 2069)
years_col = years_of_interest - 2019 + 2

for (n in years_col){
	# Create empty vectors to store location (lat,lon) info
	latitude <- numeric(100)
	longitude <- numeric(100)

	for (i in seq_len(100)) {
		xy <- str_split(bc_mack[i,n], " ")
		latitude[i] <- as.numeric(xy[[1]][1])
		longitude[i] <- as.numeric(xy[[1]][2])
	}

	# Create empty vectors to store location (row,col) info
	row_num <- numeric(100)
	col_num <- numeric(100)

	for (i in seq_len(100)) {
		row_num[i] = 62.5 - latitude[i] + 1
		col_num[i] = longitude[i] + 16.5 + 1
	}

	# Create empty vectors to store location (block number) info

	# Up to down, left to right
	# First block: 62.5, -16.5
	# 300th block: 48.5, 2.5
	num <- numeric(100)

	for (i in seq_len(100)) {
		num[i] = 20 * row_num[i] + col_num[i]
	}

	table(num)

	# Create empty vectors to count ending block number
	counter = numeric(300)

	# Mark Land
	for (i in seq_len(300)) {
		if(is.na(forecast[i,11]))
		counter[i] = NA
	}

	for (block in num) {
		counter[block] = counter[block] + 1
	}

	# Convert counter to matrix form to be visualized on map
	loc_counter = matrix(counter, nrow = 15, ncol = 20, byrow = TRUE)

	colnames(loc_counter) = seq(-16.5, 2.5, 1)
	rownames(loc_counter) = seq(62.5, 48.5, -1)

	# Visualize
	file = paste("bcm_map_20", n+17, ".png", sep = "")
	main = paste( "The Mostly Likely Location for Mackerel in 20", n+17, ", Best Case", sep = "")
	png(file, 610, 541)
	par(mar = c(5, 5, 5, 5))
	plot(loc_counter, xlab = "Longitude", ylab = 'Latitude', 
	main = main, 
	fmt.cell = '%d', col = plasma(20))
	dev.off()
}


## <--------------------------------------------------------------------------->
    ## Worst Case, Mackerel
    ## 2019(starting)/2029/2039/2049/2059/2069

wc_mack = read_csv("wc_mackerel.csv", col_names = TRUE)
    
years_of_interest = c(2019, 2029, 2039, 2049, 2059, 2069)
years_col = years_of_interest - 2019 + 2

for (n in years_col){
	# Create empty vectors to store location (lat,lon) info
	latitude <- numeric(100)
	longitude <- numeric(100)

	for (i in seq_len(100)) {
		xy <- str_split(wc_mack[i,n], " ")
		latitude[i] <- as.numeric(xy[[1]][1])
		longitude[i] <- as.numeric(xy[[1]][2])
	}

	# Create empty vectors to store location (row,col) info
	row_num <- numeric(100)
	col_num <- numeric(100)

	for (i in seq_len(100)) {
		row_num[i] = 62.5 - latitude[i] + 1
		col_num[i] = longitude[i] + 16.5 + 1
	}

	# Create empty vectors to store location (block number) info

	# Up to down, left to right
	# First block: 62.5, -16.5
	# 300th block: 48.5, 2.5
	num <- numeric(100)

	for (i in seq_len(100)) {
		num[i] = 20 * row_num[i] + col_num[i]
	}

	table(num)

	# Create empty vectors to count ending block number
	counter = numeric(300)

	# Mark Land
	for (i in seq_len(300)) {
		if(is.na(forecast[i,11]))
		counter[i] = NA
	}

	for (block in num) {
		counter[block] = counter[block] + 1
	}

	# Convert counter to matrix form to be visualized on map
	loc_counter = matrix(counter, nrow = 15, ncol = 20, byrow = TRUE)

	colnames(loc_counter) = seq(-16.5, 2.5, 1)
	rownames(loc_counter) = seq(62.5, 48.5, -1)

	# Visualize
	file = paste("wcm_map_20", n+17, ".png", sep = "")
	main = paste( "The Mostly Likely Location for Mackerel in 20", n+17, ", Worst Case", sep = "")
	png(file, 610, 541)
	par(mar = c(5, 5, 5, 5))
	plot(loc_counter, xlab = "Longitude", ylab = 'Latitude', 
	main = main, 
	fmt.cell = '%d', col = plasma(20))
	dev.off()
}













