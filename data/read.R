# Load Packages
library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)

dname <- "sst"
ncin <- nc_open("HadISST_sst.nc")

# Overview
print(ncin)

# Get longitude
lon <- ncvar_get(ncin, "longitude")
nlon <- dim(lon)
head(lon)

# Get latitude
lat <- ncvar_get(ncin, "latitude")
nlat <- dim(lat)
head(lat)

print(c(nlon, nlat))

# Get time
time <- ncvar_get(ncin, "time")
time

tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(time)
nt

tunits

# Get SST
sst_array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin,dname, "_FillValue")
dim(sst_array)

# Replace netCDF fill values with NA's
sst_array[sst_array == fillvalue$value] <- NA
length(na.omit(as.vector(sst_array[,,1])))
length(as.vector(sst_array[,,1]))





# Example: Get a single slice or layer (1870-01-01)
m <- 1
sst_slice <- sst_array[,,1]

lonlat <- as.matrix(expand.grid(lon, lat))
dim(lonlat)

# Vector of `sst` values
sst_vec <- as.vector(sst_slice)
length(sst_vec)

# Create dataframe and add names
sst_df01 <- data.frame(cbind(lonlat, sst_vec))
names(sst_df01) <- c("lon", "lat", paste(dname, as.character(m), sep = "_"))
head(na.omit(sst_df01), 10)

# Set path and filename
csvpath <- "~/Desktop/"
csvname <- "sst_1.csv"
csvfile <- paste(csvpath, csvname, sep = "")
write.table(sst_df01, csvfile, row.names = FALSE, sep = ",")





# Now we extract the data of interest
# scot: area near Scotland, UK
# longitude = lon[164:183] # -17 ~ 3
# latitude = lat[28:42] # 63 ~ 48

scot_sst = matrix(nrow = 300, ncol = 600)

k = 1
for (i in 0:14) {
	for (j in 0:19) {
		scot_sst[k,] = sst_array[163+j,28+i,1201:1800]
		k = k + 1
	}
}

t = seq(1970, 2020-1/12, by = 1/12)

# Test 1 (SST over 50 years at a certain point)
plot(t, scot_sst[2,], type = "l")

# Test 2 (SST of 15 blocks/points in a "column" at a time)
plot(scot_sst[,599], type = "l")


colnames(scot_sst) = t

csvpath <- "~/Desktop/"
csvname <- "scot_sst.csv"
csvfile <- paste(csvpath, csvname, sep = "")
write.table(scot_sst, csvfile, row.names = FALSE, sep = ",")





# Adding lon and lat as the first two columns
# i.e. corresponding to each row of scot_sst
scot_lon = lon[164:183] # -17 ~ 3
scot_lat = lat[28:42] 	# 63 ~ 48

scot_lonlat <- as.matrix(expand.grid(scot_lon, scot_lat))
colnames(scot_lonlat) = c("Longitude", "Latitude")

scot_sst_aug = cbind(scot_lonlat, scot_sst)

csvpath <- "~/Desktop/"
csvname <- "scot_sst_aug.csv"
csvfile <- paste(csvpath, csvname, sep = "")
write.table(scot_sst_aug, csvfile, row.names = FALSE, sep = ",")




