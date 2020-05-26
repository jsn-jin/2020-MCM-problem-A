clear
clc
close all

path = 'sst.nc'

%%

lon = ncread(path, 'longitude')
lat = ncread(path, 'latitude')
sst = ncread(path, 'sst')

%%

lon_area = (lon >= -17 & lon <= 3)
lat_area = (lat >= 48.5 & lat <= 63)

lon = lon(lon_area)
lat = lat(lat_area)
sst = sst(lon_area, lat_area, length(sst)-50*12+1 : length(sst))

%%
[X,Y] = meshgrid(lon, lat)
fig1 = sst(:,:,500)'
surf(X,Y,fig1)
hold on
plot3(-8,54,100,'r*', 'MarkerSize', 15)
plot3(-11,54,100,'r*', 'MarkerSize', 15)
view([0,90])
%%


