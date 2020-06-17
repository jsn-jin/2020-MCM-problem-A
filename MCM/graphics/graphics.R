#***************************************************************************
# BEST CASE GRAPHICS FOR HERRING
# Dataset: 
#     "bc_herring.csv"      end coordinates of best case herring
#***************************************************************************

library(tidyverse)
library(stringr)
library(ggplot2)

bc_herr <- read_csv("bc_herring.csv", col_names = TRUE)
bc_herr <- bc_herr[,-1]

longitude <- numeric(51)
latitude <- numeric(51)

end_pos <- bc_herr$X50

for(i in seq_len(100)) {
  xy <- str_split(end_pos[i], " ")
  latitude[i] <- as.numeric(xy[[1]][1])
  longitude[i] <- as.numeric(xy[[1]][2])
}

final_pos <- data.frame(cbind(longitude, latitude))

# longitude histogram
ggplot(final_pos, aes(x=longitude)) + 
  geom_histogram(aes(y=..density..), color="blue", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(-16.5, 2.5)) +
  ggtitle("Endpoint Longitude Histogram", "Best Case Herring")
ggsave("bch_long_hist.png")

# latitude histogram
ggplot(final_pos, aes(x=latitude)) + 
  geom_histogram(aes(y=..density..), color="green", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(48.5, 62.5)) +
  coord_flip() +
  ggtitle("Endpoint Latitude Histogram", "Best Case Herring")
ggsave("bch_lat_hist.png")

# scatter of end coordinates
ggplot(final_pos, aes(x = longitude, y = latitude)) +
  geom_point() +
  xlim(c(-16.5, 2.5)) +
  ylim(c(48.5, 62.5)) +
  ggtitle("Endpoint Locations", "Best Case Herring")
ggsave("bch_endpt_scatter.png")

#***************************************************************************
# WORST CASE GRAPHICS FOR HERRING
# Dataset: 
#     "wc_herring.csv"      end coordinates of worst case herring
#***************************************************************************

wc_herr <- read_csv("wc_herring.csv", col_names = TRUE)
wc_herr <- wc_herr[,-1]

longitude <- numeric(51)
latitude <- numeric(51)

end_pos <- wc_herr$X50
for(i in seq_len(100)) {
  xy <- str_split(end_pos[i], " ")
  latitude[i] <- as.numeric(xy[[1]][1])
  longitude[i] <- as.numeric(xy[[1]][2])
}

final_pos <- data.frame(cbind(longitude, latitude))

# longitude histogram
ggplot(final_pos, aes(x=longitude)) + 
  geom_histogram(aes(y=..density..), color="blue", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(-16.5, 2.5)) +
  ggtitle("Endpoint Longitude Histogram", "Worst Case Herring")
ggsave("wch_long_hist.png")

# latitude histogram
ggplot(final_pos, aes(x=latitude)) + 
  geom_histogram(aes(y=..density..), color="green", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(48.5, 62.5)) +
  coord_flip() +
  ggtitle("Endpoint Latitude Histogram", "Worst Case Herring")
ggsave("wch_lat_hist.png")

# scatter of end coordinates
ggplot(final_pos, aes(x = longitude, y = latitude)) +
  geom_point() +
  xlim(c(-16.5, 2.5)) +
  ylim(c(48.5, 62.5)) +
  ggtitle("Endpoint Locations", "Worst Case Herring")
ggsave("wch_endpt_scatter.png")

#***************************************************************************
# BEST CASE GRAPHICS FOR MACKEREL
# Dataset: 
#     "bc_mackerel.csv"      end coordinates of best case mackerel
#***************************************************************************

bc_mack <- read_csv("bc_mackerel.csv", col_names = TRUE)
bc_mack <- bc_mack[,-1]

longitude <- numeric(51)
latitude <- numeric(51)

end_pos <- bc_mack$X50
for(i in seq_len(100)) {
  xy <- str_split(end_pos[i], " ")
  latitude[i] <- as.numeric(xy[[1]][1])
  longitude[i] <- as.numeric(xy[[1]][2])
}

final_pos <- data.frame(cbind(longitude, latitude))

# longitude histogram
ggplot(final_pos, aes(x=longitude)) + 
  geom_histogram(aes(y=..density..), color="blue", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(-16.5, 2.5)) +
  ggtitle("Endpoint Longitude Histogram", "Best Case Mackerel")
ggsave("bcm_long_hist.png")

# latitude histogram
ggplot(final_pos, aes(x=latitude)) + 
  geom_histogram(aes(y=..density..), color="green", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(48.5, 62.5)) +
  coord_flip() +
  ggtitle("Endpoint Latitude Histogram", "Best Case Mackerel")
ggsave("bcm_lat_hist.png")

# scatter of end coordinates
ggplot(final_pos, aes(x = longitude, y = latitude)) +
  geom_point() +
  xlim(c(-16.5, 2.5)) +
  ylim(c(48.5, 62.5)) +
  ggtitle("Endpoint Locations", "Best Case Mackerel")
ggsave("bcm_endpt_scatter.png")

#***************************************************************************
# WORST CASE GRAPHICS FOR MACKEREL
# Dataset: 
#     "wc_mackerel.csv"      end coordinates of worst case mackerel
#***************************************************************************

wc_mack <- read_csv("wc_mackerel.csv", col_names = TRUE)
wc_mack <- wc_mack[,-1]

longitude <- numeric(51)
latitude <- numeric(51)

end_pos <- wc_mack$X50
for(i in seq_len(100)) {
  xy <- str_split(end_pos[i], " ")
  latitude[i] <- as.numeric(xy[[1]][1])
  longitude[i] <- as.numeric(xy[[1]][2])
}

final_pos <- data.frame(cbind(longitude, latitude))

# longitude histogram
ggplot(final_pos, aes(x=longitude)) + 
  geom_histogram(aes(y=..density..), color="blue", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(-16.5, 2.5)) +
  ggtitle("Endpoint Longitude Histogram", "Worst Case Mackerel")
ggsave("wcm_long_hist.png")

# latitude histogram
ggplot(final_pos, aes(x=latitude)) + 
  geom_histogram(aes(y=..density..), color="green", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  xlim(c(48.5, 62.5)) +
  coord_flip() +
  ggtitle("Endpoint Latitude Histogram", "Worst Case Mackerel")
ggsave("wcm_lat_hist.png")

# scatter of end coordinates
ggplot(final_pos, aes(x = longitude, y = latitude)) +
  geom_point() +
  xlim(c(-16.5, 2.5)) +
  ylim(c(48.5, 62.5)) +
  ggtitle("Endpoint Locations", "Worst Case Mackerel")
ggsave("wcm_endpt_scatter.png")
