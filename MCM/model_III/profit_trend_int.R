library(ggplot2)
library(tidyverse)
library(reshape2)

current = read_csv("current.csv", col_names = FALSE)
new_boat = read_csv("newBoat.csv", col_names = FALSE)
new_boat_int = read_csv("newBoat_int_water.csv", col_names = FALSE)
move_north_2025 = read_csv("moveNorth2025.csv", col_names = FALSE)
	
data_set = data.frame(current[,1], current[,2], new_boat_int[,2], move_north_2025[,2])
colnames(data_set) = c("Time", "Current", "Add Fridge (EEZ)", "Move North")

data_set <- melt(data_set, id.vars="Time")

# Everything on the same plot

png("profit_trend_int.png", 600, 600)

ggplot(data_set, aes(Time, value, col = variable)) + 
  geom_point() + 
  stat_smooth() +
  xlab("Time") +
  ylab("Profit Index") +
  ggtitle("Profit Trend under All Strategies") + 
  theme(text = element_text(size=18))

dev.off()
