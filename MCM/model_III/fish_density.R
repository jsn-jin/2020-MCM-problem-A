#***************************************************************************
# Computing Fish Density q USING OLD STARTING POINT
# Dataset: 
#		"forecast_sst_50.csv"      	2020-2069
#		"bc_herring.csv"      		end coordinates of best case herring
#		"bc_mackerel.csv"      		end coordinates of best case mackerel   
# Author: Hao Jin
#***************************************************************************

library(tidyverse)
library('plot.matrix')
library(stringr)
library("viridis") 
library(RColorBrewer)  

forecast = read.csv("forecast_sst_50_t.csv", row.names = 1)
bc_herr = read_csv("bc_herring.csv", col_names = TRUE)
bc_mack = read_csv("bc_mackerel.csv", col_names = TRUE)

# Current fishing port, old fishing boat covers
# 95,  96,  97, 
# 115, 116, 117, 
#      136, 137
#			157

# Current fishing port, new fishing boat covers
# 53, 54, 55, 56, 57, 58, 59 
# 73, 74, 75, 76, 77, 78, 79
#     94, 95, 96, 97, 98, 99
#		  115,116,117,118,119
#			  136,137,138,139
#			  	  157,158,159
#				      178,179
#						  199

# Move south, old fishing boat covers
# 137,138,139
# 157,158,159
# 	  178,179
#		  199

# Move south, old fishing boat covers
# 74, 75, 76
# 94, 95, 96
# 114,115,116
#         136


# ------------------------------------------ Herring ------------------------------------------ #

years_of_interest = seq(2019, 2069)
years_col = years_of_interest - 2019 + 2

current_herr = numeric(51)
new_boat_herr = numeric(51)
move_south_herr = numeric(51)
move_north_herr = numeric(51)

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
	
	current_herr[n-1] = sum(counter[c(	95, 96, 97,  
										115,116,117,  
											136,137,  
												157)])
														
	new_boat_herr[n-1] = sum(counter[c(	53, 54, 55, 56, 57, 58, 59,
										73, 74, 75, 76, 77, 78, 79,
										    94, 95, 96, 97, 98, 99,
										        115,116,117,118,119,
										            136,137,138,139,
										                157,158,159,
										                    178,179,
										                        199)])
	
	move_south_herr[n-1] = sum(counter[c( 137,138,139,  
										  157,158,159,
										      178,179,
										          199)])
										          
										          
	move_north_herr[n-1] = sum(counter[c( 74, 75, 76,
										  94, 95, 96,
										      115,116,
										          136)])									
}

current_herr
new_boat_herr
move_south_herr
move_north_herr

q_herr = data.frame("current" = current_herr, 
					"new boat" = new_boat_herr, 
					"move north" = move_south_herr, 
					"move_south" = move_north_herr)  

rownames(q_herr) = seq(2019,2069)					
					
write.csv(q_herr, "q_herring.csv")
					
# ------------------------------------------ Mackerel ------------------------------------------ #

years_of_interest = seq(2019, 2069)
years_col = years_of_interest - 2019 + 2

current_mack = numeric(51)
new_boat_mack = numeric(51)
move_south_mack = numeric(51)
move_north_mack = numeric(51)

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
	
	current_mack[n-1] = sum(counter[c(	95, 96, 97,  
										115,116,117,  
											136,137,  
												157)])
														
	new_boat_mack[n-1] = sum(counter[c(	53, 54, 55, 56, 57, 58, 59,
										73, 74, 75, 76, 77, 78, 79,
										    94, 95, 96, 97, 98, 99,
										        115,116,117,118,119,
										            136,137,138,139,
										                157,158,159,
										                    178,179,
										                        199)])
	
	move_south_mack[n-1] = sum(counter[c( 137,138,139,  
										  157,158,159,
										      178,179,
										          199)])
										          
										          
	move_north_mack[n-1] = sum(counter[c( 74, 75, 76,
										  94, 95, 96,
										      115,116,
										          136)])									
}

current_mack
new_boat_mack
move_south_mack
move_north_mack

q_mack = data.frame("current" = current_mack, 
					"new boat" = new_boat_mack, 
					"move north" = move_south_mack, 
					"move_south" = move_north_mack)  

rownames(q_mack) = seq(2019,2069)					
					
write.csv(q_mack, "q_mackerel.csv")














