bbox <- st_bbox(c(xmin = -60, ymin = -63.5, xmax = -57.5, ymax = -61), crs = 4326)
bbox_sf <- st_as_sfc(bbox)  # Convert bbox to spatial object
coastline_crop <- st_crop(coastline, bbox_sf)


library("rerddap")
sstInfo <- info('jplMURSST41')
murSST <- griddap(sstInfo, latitude = c(-61.5, -63.5), longitude = c(-60, -57.5), time = c('last','last'), fields = 'analysed_sst')
bathyInfo <- info('GEBCO_2020')
bathy <- griddap(bathyInfo, latitude = c(-61.5, -63), longitude = c(-60, -55), fields = 'elevation')
bathy_df <- bathy$data

library(colorspace)
library(RColorBrewer)

ggplot() +
  geom_tile(data = bathy_df, aes(x = longitude, y = latitude, fill = elevation)) +
  scale_fill_terrain(palette = "Dem",  # "Dem" is designed for elevation maps
                     na.value = "white") +  # Set missing values to white
  theme_minimal() +
  labs(title = "GEBCO 2020 Bathymetry", x = "Longitude", y = "Latitude") +
  geom_sf(data = coastline_crop, color = "black", fill = "grey", size = 0.1, alpha = 0) +
  coord_sf(crs = st_crs(4326), 
           xlim = c(-75, -55), 
           ylim = c(-73, -61.5))

ggplot() +
  geom_tile(data = bathy_df, aes(x = longitude, y = latitude, fill = elevation)) +
  scale_fill_stepsn(n.breaks = 20, colours = viridis::viridis(20)) +
  theme_minimal() +
  labs(title = "GEBCO 2020 Bathymetry", x = "Longitude", y = "Latitude") +
  geom_sf(data = coastline_crop, color = "black", fill = "grey", size = 0.1, alpha = 0) +
  coord_sf(crs = st_crs(4326), 
           xlim = c(-60, -55), 
           ylim = c(-63, -61.5))

scale_fill_gradient2(
  low = '#36648B',
  mid = "white",
  high = '#8B7E66',
  midpoint = 0,
  na.value = "black"
) +

1981-01-01 00:00:00
# Extract the SST data from the griddap result
murSST_df <- murSST$data  # Extract the data component

ggplot() +
  geom_tile(data = murSST_df, aes(x = longitude, y = latitude, fill = analysed_sst)) +
  #scale_fill_gradientn(colors = c("darkblue", "blue", "cyan", "yellow", "red"),
  #                     name = "SST (°C)") +
  #coord_fixed() +  # Keep aspect ratio correct
  theme_minimal() +
  labs(title = "MUR SST", x = "Longitude", y = "Latitude") +
  geom_sf(data = coastline_crop, color = "white", fill = "grey", size = 0.1, alpha = 0) +
  coord_sf(crs = st_crs(4326), 
               xlim = c(-60, -57.5), 
               ylim = c(-61.5, -63.5))
  
d




data$sst=NA
library(rerddapXtracto)
library(raster)
ext=raster::extent(-77,-70,-41,-18)
xpos <- c(-77,-70)
ypos <- c(-41,-18)
sstInfo <- rerddap::info('jplMURSST41')#Temperatura

dates <- unique(na.omit(strftime(data$date[is.na(data$sst)], format="%Y-%m-%d", tz="GMT")))


for (i in 1:length(dates)) {
  sst<- rerddapXtracto::rxtracto_3D(sstInfo,parameter ='analysed_sst',xcoord=xpos, ycoord=ypos, tcoord=c(dates[i],dates[i]))
  sst=sst$analysed_sst
  sst=raster(sst[,,1])
  sst=t(flip(sst,1))
  data$sst[strftime(data$date, format="%Y-%m-%d",tz="GMT")==dates[i]]=raster::extract(sst,cbind(data$lon[strftime(data$date, format="%Y-%m-%d",tz="GMT")==dates[i]],
                                                                                                data$lat[strftime(data$date, format="%Y-%m-%d",tz="GMT")==dates[i]]))
  
}