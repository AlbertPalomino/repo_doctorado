library(diveMove)
library(ggplot2)
library(ggmap)
library(dplyr)

setwd("/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/Barbijos")
df <- read.csv("CP-01_S1.csv")
folder_path <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/Barbijos"
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
tracks <- lapply(csv_files, function(file) read.csv(file, row.names = NULL))
names(tracks) <- tools::file_path_sans_ext(basename(csv_files))

# Keep rows whith Depth and GPS data
df_depth <- subset(df, !is.na(Depth))
df_depth$date <- as.POSIXct(paste(df_depth$Date, df_depth$Time), format = "%d/%m/%Y %H:%M:%OS")
df_loc <- subset(df_depth,!is.na(location.lat))

# Plot GPS coordinates
box <- make_bbox(lon = df_loc$location.lon, lat = df_loc$location.lat, f = .1) #from ggmap
sq_map <- get_map(location = box, maptype = "terrain", source = "google")

coastline <- st_read("/media/ddonoso/Pengo2/Doctorado/ATA_adm0/ATA_adm0.shp")
coastline <- st_read("/media/ddonoso/Pengo2/Doctorado/data exploration/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp")
coastline <- st_transform(coastline, crs = 4326)

# Crop and buffer coastline
bbox <- st_bbox(c(xmin = -2822404, ymin = 1462637, xmax = -2466896, ymax = 1712945), crs = 3031)
bbox_sf <- st_as_sfc(bbox)  # Convert bbox to spatial object
coastline_crop <- st_crop(coastline, bbox_sf)
coastline_crop_4326 <- st_transform(coastline_crop, crs = 4326)
coastline_buff <- st_buffer(coastline_crop, dist=100)
coastline_buff_4326 <- st_transform(coastline_buff, crs = 4326)

# Remove points on the coast
points_sf <- st_as_sf(df_loc, coords = c("location.lon", "location.lat"), crs = 4326)

polygon_sf <- st_transform(coastline_buff, crs = st_crs(points_sf))
points_within <- st_intersects(points_sf, polygon_sf, sparse = FALSE)
points_outside <- points_sf[!points_within, ]

points_outside <- st_filter(points_sf, colony_buff_4326, .predicate = st_disjoint)

coords <- st_coordinates(points_outside$geometry)
colnames(coords) <- c("location.lon", "location.lat")
points_outside <- cbind(points_outside, coords)
write.csv(points_outside, "track_barbijo.csv", row.names = FALSE)

# Create buffer around colony
colony=data.frame(x=-59.208966,y=-62.310199)
POINT <- st_as_sf(x = colony, coords=c("x", "y"),crs= 4326)
POINT <- st_transform(POINT, crs = 3031)
colony_buff <- st_buffer(POINT, dist=200)
colony_buff_4326 <- st_transform(colony_buff, crs = 4326)

# Plot data
ggplot() + 
  geom_sf(data = coastline_crop_4326, fill = "gray90", color = "black") +
  geom_sf(data = colony_buff_4326, color = "red") +
  geom_path(data = points_outside, aes(x = location.lon, y = location.lat), 
            size = 1, lineend = "round") +
  labs(x = " ", y = " ", title = "CP01-R1 tracks") +
  theme_minimal() +
  theme(legend.position = "none") +
  coord_sf(
    xlim = c(-59.6, -58.5),
    ylim = c(-62.55, -62.2),
    expand = FALSE  # Prevents ggplot from adding padding
  )

 # Loop through tracks and extract depth observations (remove accelerometry)
depths <- lapply(tracks, function(df) {
  df <- df %>%
    filter(!is.na(Depth)) %>%    # Filter rows where Depth is not NA
    mutate(date = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%OS"))
  return(df)
  })

# Loop through depths and extract location observations
locations <- lapply(depths, function(df) {
  df <- df %>%
    filter(!is.na(location.lon)) %>%    # Filter rows where Depth is not NA
  return(df)
})

# Concatenate tracking files for each individual
tags <- read.csv("/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/chinstrap_tags.csv")
ind_tracks <- list()

for (col in names(tags)) {      # Loop through each column in tags
  selected_df <- depths[names(depths) %in% tags[[col]]]   # Get the data frames in depths matching the current col
  ind_tracks[[col]] <- bind_rows(selected_df, .id = "tag_id")   # Combine the selected data frames into one and store it in the new list
}

# Remove specified columns
cols_to_remove <- c("TagID", "X", "Y", "Z", "hdop" ,"satellites" ,"signal.strength", "Sensor.Raw", "Battery..V.", "Metadata") 

ind_tracks <- lapply(ind_tracks, function(df) {
  df[, !names(df) %in% cols_to_remove]
})

# Add a column with the name of each individual 
original_names <- names(ind_tracks)

ind_tracks <- lapply(seq_along(ind_tracks), function(i) {
  df <- ind_tracks[[i]]
  df$individual <- sub("^X", "", original_names[i])  # Remove the "X" from the name
  df
})

names(ind_tracks) <- original_names

# Change the class of specified columns 
ind_tracks <- lapply(ind_tracks, function(df) {
  df$tag_id <- as.factor(df$tag_id)
  df$Activity <- as.factor(df$Activity)
  df$individual <- as.factor(df$individual)
  df
})

# Add a "deployment" column to each data frame
ind_tracks <- lapply(ind_tracks, function(df) {
  df$deployment <- as.factor(as.numeric(as.factor(df$tag_id)))
  df
})

# Save a time-depth csv file for each individual
output_folder = "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/"

lapply(names(ind_tracks), function(name) {
  file_path <- file.path(output_folder, paste0(name, ".csv"))
  write.csv(ind_tracks[[name]], file = file_path, row.names = FALSE)
})

# Test plots for visualiation of dive data
ggplot(data=ind_tracks[[1]]) +
  geom_point(aes(x = date, y = Depth, color = tag_id))

ggplot(data=ind_tracks[[1]]) +
  geom_boxplot(aes(x = deployment, y = Depth))
