library(diveMove)
library(ggplot2)
detach("package:stats", unload = TRUE)
library(dplyr)
library(sf)
library(tidyverse)
library(RColorBrewer)

setwd("/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/Barbijos")

# Load and save a list of data frames ----
folder_path <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek"
csv_files <- list.files(path = folder_path, pattern = "^X.*\\.csv$", full.names = TRUE, recursive = FALSE)
ind_tracks <- lapply(csv_files, function(file) read.csv(file, row.names = NULL))
original_names <- tools::file_path_sans_ext(basename(csv_files))
names(ind_tracks) <- sub("^X", "", original_names)


output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/locations"
lapply(names(ind_locations), function(name) {
  clean_name <- sub("^X", "", name)
  ind_locations[[name]] %>%
    select(-Time, -Date, -location.lat, -location.lon) %>%
    relocate(date, lat, lon, deployment, segment, individual, .before = everything()) %>%
    write.csv(file.path(output_folder, paste0("locations_", clean_name, ".csv")), row.names = FALSE)
})

# Crop coastline ----
coastline <- st_read("/media/ddonoso/Pengo2/Doctorado/data exploration/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp")
#coastline <- st_read("/media/ddonoso/Pengo2/Doctorado/ATA_adm0/ATA_adm0.shp")
coastline <- st_transform(coastline, crs = 3031)
bbox <- st_bbox(c(xmin = -2822404, ymin = 1550000, xmax = -2600000, ymax = 1600000), crs = 3031)
bbox_sf <- st_as_sfc(bbox)  # Convert bbox to spatial object
coastline_crop <- st_crop(coastline, bbox_sf)
coastline_crop_4326 <- st_transform(coastline_crop, crs = 4326)

# Buffer coastline
coast_buff <- st_transform(coastline_crop_4326, crs = 3031)
coast_buff <- st_buffer(coast_buff, dist=200)
coast_buff <- st_transform(coast_buff, crs = 4326)

# Create buffer area around colony ----
colony=data.frame(x=-59.208966,y=-62.310199)
POINT <- st_as_sf(x = colony, coords=c("x", "y"),crs= 4326)
POINT <- st_transform(POINT, crs = 3031)
colony_buff <- st_buffer(POINT, dist=300)
colony_buff <- st_transform(colony_buff, crs = 4326)

# Create buffer area around harmony point
land_buff = data.frame(x = -59.21, y = -62.3045)
POINT <- st_as_sf(x = land_buff, coords=c("x", "y"),crs= 4326)
POINT <- st_transform(POINT, crs = 3031)
land_buff <- st_buffer(POINT, dist=1000)
land_buff <- st_transform(land_buff, crs = 4326)

# 30 km buffer around colony ----
colony=data.frame(x=-59.208966,y=-62.310199)
POINT <- st_as_sf(x = colony, coords=c("x", "y"),crs= 4326)
POINT <- st_transform(POINT, crs = 3031)
colony_buff30k <- st_buffer(POINT, dist=30000)
colony_buff30k <- st_transform(colony_buff30k, crs = 4326)

# Read tracking files and remove accelerometry ----
folder_path <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/Barbijos"
csv_files <- list.files(path = folder_path, pattern = "\\.csv$",  full.names = TRUE, recursive = TRUE)
#tracks <- lapply(csv_files, function(file) read.csv(file, sep= " ", row.names = NULL))

tracks <- lapply(csv_files, function(file) {
  first_line <- readLines(file, n = 1)  # Read a few lines of the file to determine the separator
  sep <- ifelse(grepl(";", first_line), ";", ",")
  read.csv(file, sep = sep, row.names = NULL) # Read the CSV file with the determined separator
})
names(tracks) <- tools::file_path_sans_ext(basename(csv_files))

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

# Loop through locations and identify foraging periods ----
locations <- lapply(locations, function(df) {
  df <- df %>%
    st_as_sf(coords = c("location.lon", "location.lat"), crs = st_crs(colony_buff)
    ) %>%
    mutate(
      within_mask = st_within(geometry, colony_buff, sparse = FALSE)[, 1], # Check if point is within colony buffer
      trip_start = within_mask & lead(!within_mask, default = FALSE), # Last point inside before leaving
      trip_end = lag(!within_mask, default = FALSE) & within_mask  # First point inside after re-entering
    ) %>%
    mutate(
      foraging = ifelse(!within_mask | trip_start | trip_end, 1, 0) # Foraging flag: 1 if leaving, outside or re-entering
    ) %>%
    mutate(
      location.lon = st_coordinates(geometry)[, 1], # Extract x coordinate
      location.lat = st_coordinates(geometry)[, 2] # Extract y coordinate
    ) %>%
    #st_as_sf(coords = c("location.lon", "location.lat"), crs = 4326 # Rebuild sf object with updated coordinates) %>%
    dplyr::select(-c(geometry, within_mask, trip_start, trip_end)) # Remove columns
})

# Add foraging column to df in depths
depths <- Map(function(loc_df, depth_df) { # Map applies a function to each pair of elements of two lists
  loc_df_clean <- loc_df %>%
    st_drop_geometry() %>%  # Remove geometry column temporarily
    select(date, foraging)  # Select the columns to be merged
  depth_df %>%
    left_join(loc_df_clean, by = "date")  # Merge based on "date"
}, locations, depths)

# Concatenate, process and export tracking files for each individual ----
tags <- read.csv("/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/chinstrap_tags.csv")
ind_tracks <- list()

for (col in names(tags)) {      # Loop through each column in df tags
  selected_df <- depths[names(depths) %in% tags[[col]]]   # Get the data frames in depths matching the current column in tags
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
  df <- df %>% 
    arrange(date)
    df$deployment <- as.factor(as.numeric(factor(df$tag_id, levels = unique(df$tag_id[order(df$date)]))))  # Reorder 'tag_id' factor levels based on the sorted 'date' column
    df
})

# Add a segment column that groups observations every 15 minutes ----
ind_tracks <- lapply(names(ind_tracks), function(name) {
  df <- ind_tracks[[name]] %>%
    mutate(segment = floor_date(date, '15 minutes')) %>%  # Create a new segment column
    group_by(segment) %>%  # Group by the new segment
    mutate(segment = as.factor(cur_group_id())) %>%  # Assign a unique factor level
    ungroup()
  return(df)
})

# Save a time-depth csv file for each individual
names(ind_tracks) <- original_names
output_folder = "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/"
lapply(names(ind_tracks), function(name) {
  file_path <- file.path(output_folder, paste0(name, ".csv"))
  write.csv(ind_tracks[[name]], file = file_path, row.names = FALSE)
})

# Loop through ind_tracks, extract location observations and foraging trips ----
ind_locations <- lapply(ind_tracks, function(df) {
  df <- df %>%
    filter(!is.na(location.lon) # Filter rows where Depth is not NA
    ) %>%    
    mutate(foraging_trip = { 
        # Initialize trip_id and result vector
        trip_id <- 0
        foraging_trip <- numeric(length(foraging))
        # Iterate to assign trip IDs
        for (i in seq_along(foraging)) {
          if (foraging[i] == 1 && (i == 1 || foraging[i - 1] == 0)) {
            trip_id <- trip_id + 1 # Increment trip ID on transition
          }
          foraging_trip[i] <- ifelse(foraging[i] == 1, trip_id, 0)
        }
        foraging_trip
      }
    ) %>%
    mutate(foraging_trip = as.factor(foraging_trip))
  df
    })

names(ind_locations) <- original_names

# Remove foraging_trip when all points are on land and short trips ----
foraging_trips <- lapply(ind_locations, function(df) {
  df <- df %>%
    st_as_sf(coords = c("location.lon", "location.lat"), crs = 4326) %>%  # Convert to sf object
    group_by(foraging_trip) %>%
    mutate(
      # Determine if the group meets the filtering conditions
      meets_criteria = !all(st_within(geometry, land_buff, sparse = FALSE)[, 1]) & 
        (difftime(max(date), min(date), units = "hours") >= 1) & 
        n() > 4
    ) %>%
    ungroup() %>%
    mutate(
      # Set foraging_trip to 0 if the group does not meet the criteria
      foraging_trip = if_else(meets_criteria, as.numeric(foraging_trip), 0),
      foraging_trip = foraging_trip,  # Keep the same foraging trip otherwise
    ) %>%
    mutate(
      foraging_trip = dense_rank(foraging_trip), # Rename foraging trips with consecutive numbers
      foraging_trip = foraging_trip - 1, # Subtract 1 so foraging_trip levels start at 0
      foraging_trip = as.factor(foraging_trip)
    ) %>%
    mutate(coords = st_coordinates(geometry)) %>%  # Extract coordinates
    mutate(location.lon = coords[, 1], location.lat = coords[, 2]) %>%  # Add coordinates as columns
    select(-starts_with("coords"), -geometry, -meets_criteria, -Date, -Time) %>% # Remove temporary columns 
    relocate(date, location.lat, location.lon, deployment, segment, foraging, foraging_trip, individual, .before = everything())
  df
})

# Save a foraging trip CSV for each individual
output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/locations"
lapply(names(foraging_trips), function(name) {
  clean_name <- sub("^X", "", name)
  foraging_trips[[name]] %>%
  write.csv(file.path(output_folder, paste0("locations_", clean_name, ".csv")), row.names = FALSE)
})

# Save a separate CSV file for each foraging trip
output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/foraging_trips"
lapply(seq_along(foraging_trips), function(i) {
  df <- foraging_trips[[i]]
  # Loop through each unique foraging trip
  for (trip_id in unique(df$foraging_trip)) {
    # Filter the data for the current trip
    trip_df <- df %>% filter(foraging_trip == trip_id)
    individual <- unique(trip_df$individual)
    trip <- unique(trip_df$foraging_trip)
    file_name <- paste0(output_folder, "/", individual, "_", trip, ".csv")
    write.csv(trip_df, file_name, row.names = FALSE)
  }
})

# Assign a foraging trip number to each dive ----

# Add foraging trip number to ind_tracks
new_tracks <- Map(function(tracks_df, foraging_df) { # Map applies a function to each pair of elements of two lists
  foraging_df_clean <- foraging_df %>%
    st_drop_geometry() %>%  # Remove geometry column temporarily
    #mutate(date = as.POSIXct(date)) %>% 
    select(date, foraging_trip)  # Select the columns to be merged
  tracks_df %>%
    #mutate(date = as.POSIXct(date)) %>% 
    select(-foraging_trip) %>%
    left_join(foraging_df_clean, by = "date")  # Merge based on "date"
}, ind_tracks, foraging_trips)


# Fill foraging and foraging_trip downward then upward
new_tracks <- lapply(new_tracks, function(df) {
df <- df %>%
  fill(foraging, foraging_trip, .direction = "down") %>%  # Fill downward first
  fill(foraging, foraging_trip, .direction = "up")       # Fill upward for the rows before the first observation
})

# Save new_tracks CSV for each individual
output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek"
lapply(names(new_tracks), function(name) {
  new_tracks[[name]] %>%
    write.csv(file.path(output_folder, paste0("X", name, ".csv")), row.names = FALSE)
})

# Add foraging trip number to ind_dives
new_dives <- Map(function(dives, tracks) {
  tracks_clean <- tracks %>%
    #st_drop_geometry() %>%  # Remove geometry column temporarily
    mutate(date = as.POSIXct(date)) %>% 
    select(date, foraging_trip, segment, deployment)  # Select the columns to be merged
  dives %>%
    mutate(date = as.POSIXct(begdesc)) %>% 
    left_join(tracks_clean, by = "date")  # Merge based on "date"
}, ind_dives, new_tracks)

# Save a dives CSV for each individual
output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/dives"
lapply(names(new_dives), function(name) {
  new_dives[[name]] %>%
    write.csv(file.path(output_folder, paste0("dives_", name, ".csv")), row.names = FALSE)
})


# Plot data ----
ggplot() + 
  geom_sf(data = coastline_crop_4326, fill = "gray90", color = "black") +
  geom_sf(data = land_buff, color = "red", fill=NA) +
  geom_sf(data = colony_buff, color = "red", fill=NA) +
  #geom_sf(data = colony_buff30k, color = "red", fill=NA) +
  geom_path(data = foraging_trips[[11]], aes(x = location.lon, y = location.lat, color = foraging_trip), linewidth = 1, lineend = "round") +
  geom_point(data = test[[11]], aes(x = location.lon, y = location.lat), color = "black", size = 0.5) +
  labs(x = " ", y = " ", title = "", color = "Foraging Trip") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_sf(
    xlim = c(-59.45, -58.95),
    ylim = c(-62.35, -62.25),
    expand = FALSE  # Prevents ggplot from adding padding
  )

# Load and save a list of data frames ----
folder_path <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/dives"
csv_files <- list.files(path = folder_path, pattern = "^dives.*\\.csv$", full.names = TRUE, recursive = FALSE)
ind_dives <- lapply(csv_files, function(file) read.csv(file, row.names = NULL))
original_names <- tools::file_path_sans_ext(basename(csv_files))
names(ind_tracks) <- sub("^dives_", "", original_names)

output_folder <- "/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek/locations"
lapply(names(ind_locations), function(name) {
         clean_name <- sub("^X", "", name)
         ind_locations[[name]] %>%
           select(-Time, -Date, -location.lat, -location.lon) %>%
           relocate(date, lat, lon, deployment, segment, individual, .before = everything()) %>%
           write.csv(file.path(output_folder, paste0("locations_", clean_name, ".csv")), row.names = FALSE)
       })

# Create TDR objects, detect dives and extract statistics in one file ----

# if necessary load file pengo1a, otherwise extract it from the df list ind_tracks
#path <- file.path("/media/ddonoso/Pengo2/antartida_22_23/harmony/AxyTrek","pengo1a.csv")
#sfp <- system.file(path, package = "diveMove")
#srcfn <- basename(sfp)
#pengo1a <- read.csv(path, sep = ",")
#pengo1a$date <- as.POSIXct(pengo1a$date)

tdr <- createTDR(time = ind_tracks[[1]]$date,
                 depth = ind_tracks[[1]]$Depth,
                 concurrentData = ind_tracks[[1]][, -c(1:5)],
                 dtime= 1,
                 file = basename(system.file(ind_tracks[[1]], package = "diveMove")))
plotTDR(tdr)
dcalib <- calibrateDepth(tdr, dry.thr = 60, dive.thr = 1, zoc.method = "offset", offset = 0.5)
plotTDR(dcalib, diveNo=seq(1950:2000), surface=TRUE)
plotDiveModel(dcalib, diveNo=100)
stats <- diveStats(dcalib, depth.deriv = FALSE)

# Loop through ind_tracks to create TDR objects, detect dives and extract statistics ----

dcalib_list <- lapply(ind_tracks, function(df) {

  df <- df[!duplicated(df$date), ]   # Ensure unique timestamps in the 'date' column
  # Create the TDR object
  tdr <- createTDR(
    time = df$date,
    depth = df$Depth,
    concurrentData = df[, -c(1:5)], # Exclude first five columns
    dtime = 1,  # Sampling interval in seconds
    file = basename(tempfile())  # Temporary file name
  )
  # Detect dives
  calibrateDepth(
    tdr,
    dry.thr = 60,       # Dry threshold
    dive.thr = 1,       # Dive threshold
    zoc.method = "offset",
    offset = 0.5
  )
})

# Create list with dive statistics
stats_list <- lapply(dcalib_list, function(dcalib) {
  diveStats(dcalib, depth.deriv = FALSE)
})

# Rename with original names
names(dcalib_list) <- names(ind_tracks)
names(stats_list) <- names(ind_tracks)

# Add a column with the name of each individual 
original_names <- names(stats_list)
stats_list <- lapply(seq_along(stats_list), function(i) {
  df <- stats_list[[i]]
  df$individual <- sub("^X", "", original_names[i])  # Remove the "X" from the name
  df
})
names(stats_list) <- original_names

# Compare unimodal and smooth spline dive models ----
diveNo <- 1900
diveX <- as.data.frame(extractDive(dcalib, diveNo=diveNo))
diveX.m <- cbind(as.numeric(row.names(diveX[-c(1, nrow(diveX)), ])),
                 diveX$depth[-c(1, nrow(diveX))],
                 diveX$time[-c(1, nrow(diveX))])
## calibrateDepth() default unimodal regression. Number of inner knots is either 10 or the number of samples in the dive, whichever is larger.
(phases.uni <- diveMove:::.cutDive(diveX.m, smooth.par=0.2, knot.factor=20,
                                   dive.model="unimodal",
                                   descent.crit.q=0.01, ascent.crit.q=0))
## Smoothing spline model, using default smoothing parameter.
(phases.spl <- diveMove:::.cutDive(diveX.m, smooth.par=0.2, knot.factor=20,
                                   dive.model="smooth.spline",
                                   descent.crit.q=0.01, ascent.crit.q=0))
plotDiveModel(phases.spl, diveNo=paste(diveNo, ", smooth.par=", 0.2, sep=""))
plotDiveModel(phases.uni, diveNo=paste(diveNo, ", unimodal", sep=""))

# Test plots for visualisation of dive data ----
ggplot(data=ind_tracks[[1]]) +
  geom_point(aes(x = date, y = Depth, color = tag_id))

ggplot(data=ind_tracks[[1]]) +
  geom_boxplot(aes(x = deployment, y = Depth))
