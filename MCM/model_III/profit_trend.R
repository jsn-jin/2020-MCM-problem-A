library(ggplot2)
library(tidyverse)

current = read_csv("current.csv", col_names = FALSE)
new_boat = read_csv("newBoat.csv", col_names = FALSE)
move_north = read_csv("moveNorth.csv", col_names = FALSE)

pimin = min(current[,2], new_boat[,2], move_north[,2])
pimax = max(current[,2], new_boat[,2], move_north[,2])

plot(current, type = "l", col = "salmon", ylim = c(pimin, pimax))
lines(new_boat, type = "l", col = "deepskyblue")
lines(move_north, type = "l", col = "steelblue")

ggplot(data = current, mapping = aes(x = X1, y = X2)) + 
	geom_line() +
	geom_line(data = new_boat, mapping = aes(x = X1, y = X2)) +
	geom_line(data = move_north, mapping = aes(x = X1, y = X2)) +
	xlab("Time") +
	ylab("Profit Index")
	
data_set = data.frame(current[,1], current[,2], new_boat[,2], move_north[,2])
colnames(data_set) = c("Time", "Current", "Buy New Boat", "Move North")

library(reshape2)
data_set <- melt(data_set, id.vars="Time")

# Everything on the same plot

png("profit_trend.png", 600, 600)

ggplot(data_set, aes(Time, value, col = variable)) + 
  geom_point() + 
  stat_smooth() +
  xlab("Time") +
  ylab("Profit Index") +
  ggtitle("Profit Trend under All Strategies") + 
  theme(text = element_text(size=18))

  
dev.off()
