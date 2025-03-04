library(dplyr)
library(lubridate)

# Import station data ----
setwd("/media/ddonoso/KINGSTON/station_series")

file_list <- list.files(pattern = "^validation_", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too
station_list <- lapply(file_list, read.csv)
names(station_list) <- gsub("validation_|\\.csv$", "", basename(file_list))

# Rename files
data_frames <- station_list

data_frames <- lapply(data_frames, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
  })

# Fill hours o'clock with 00:00:00 if needed
data_frames <- lapply(data_frames, function(df) {
  df %>%
    mutate(date = as.character(date),
           date = if_else(nchar(date) == 10, paste0(date, " 00:00:00"), date),  # Append "00:00:00" if missing
           date = ymd_hms(date))  # Convert to proper datetime format
})

# Rename data frames to match those of reanalyses
names(data_frames)[names(data_frames) == "fossil"] <- "fossilbluff"
names(data_frames)[names(data_frames) == "king"] <- "kingsejong"

# Export datasets
output_directory <- "/media/ddonoso/KINGSTON/station_series/"

for (name in names(data_frames)) {
  data_frames[[name]]$date <- format(data_frames[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0("validation_", name, ".csv"))
  write.csv(data_frames[[name]], file = file_path, row.names = FALSE)
}

# Calculate elevation difference and distance between station and grid point ----
stations_validations <- stations_validations %>%
  group_by(station) %>%
  mutate(elevation_diff = elevation - elevation[dataset == "station"]) %>%
  ungroup()

# Calculate distances using distHaversine for all points
coords1 <- as.matrix(sations_validations[c(1:12), c(4, 3)])  # Extract station coordinates from df stations (longitude, latitude)
coords2 <- as.matrix(stations[, c(5, 4)])  # Extract reanalysis coordinates from df stations (longitude, latitude)
distances <- distHaversine(coords1, coords2)  # Distance in meters
stations$distance <- distances / 1000 # Convert distances to kilometers and add as a new column

library(geosphere)
library(dplyr)

# Number of groups (each has 12 rows)
n_groups <- nrow(stations_validations) / 12  

# Initialize an empty vector for distances
distances <- numeric(nrow(stations_validations))

# Loop through each group of 12 rows
for (i in seq_len(n_groups)) {
  
  start_idx <- 1 + (12 * (i - 1))
  end_idx <- 12 + (12 * (i - 1))
  
  # Extract station coordinates (longitude in col 4, latitude in col 3)
  coords1 <- as.matrix(stations_validations[1:12, c(4, 3)])
  
  # Extract reanalysis coordinates (next 12 rows)
  coords2 <- as.matrix(stations_validations[start_idx:end_idx, c(4, 3)])
  
  # Compute Haversine distance (meters) and convert to km
  distances[start_idx:end_idx] <- distHaversine(coords1, coords2) / 1000  
}

# Add distances to the data frame
stations_validations$distance <- distances
output_directory <- "/media/ddonoso/KINGSTON"
file_path <- file.path(output_directory, paste0("coords_dist.csv"))
write.csv(stations_validations, file = file_path, row.names = FALSE)

# Plot logarithmic attenuation of wind ----
{
  # Known values
  x1 <- 0.1
  y1 <- 0
  x2 <- 10
  y2 <- 11.684628 #unknown    # replace with vel value at 10
  
  # Assuming y = a * log(x) + c, use two points to find a and c
  # Logarithmic model fitting
  model <- lm(c(y1, y2) ~ log(c(x1, x2)))
  
  # Extract coefficients
  a <- coef(model)[2]  # slope
  c <- coef(model)[1]  # intercept
  
  # Define function
  log_function <- function(x) {
    a * log(x) + c
  }
  
  # Calculate intermediate values
  intermediate_x <- seq(0.1, 10, length.out = 100)
  intermediate_values <- log_function(intermediate_x)
  value_at_3 <- log_function(3)
  
  # Plot results
  plot(intermediate_values, intermediate_x, type = 'l', main = 'Logarithmic Attenuation of Wind Speed', xlab = 'Wind speed (m/s)', ylab = 'Height (m)')
  points(c(y1, y2), c(x1, x2), col = 'red', pch = 19)  # points at known values
  points(c(value_at_3), c(3), col = 'blue', pch = 19)  # points at known values
  }
# Import and process ERA5 and ERA5-Land ----

#path <- "/media/ddonoso/KINGSTON/era5land"
#path <- "/media/ddonoso/KINGSTON/era5land_fossilbluff"
path <- "/media/ddonoso/KINGSTON/era5"
setwd(path)

file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too

df_list <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
#names(df_list) <- gsub("^era5land_|\\.csv$", "", basename(file_list))
names(df_list) <- gsub("^era5_|\\.csv$", "", basename(file_list))

# Remove unwanted stations
remove <- c("ferraz", "byers", "dismal", "kirkwood", "racer", "hugo", "gdg","hurd")
df_list <- df_list[!names(df_list) %in% remove]

# Store original names
original_names <- names(df_list)

# Convert Julian hours in the date column for each data frame
df_list <- lapply(df_list, function(df) {
  df %>%
    #mutate(date = as_datetime(date* 3600, origin = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"), tz = "UTC")) ERA5-Land
     mutate(date = as_datetime(date, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), tz = "UTC")) # ERA5 and ERA5-Land fossil bluff
})

# Subset study periods
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    dplyr::filter(
      (date >= as.POSIXct("2013-10-11 15:00:00", tz="UTC") & date < as.POSIXct("2014-11-30 03:00:00", tz="UTC")) |
        (date >= as.POSIXct("2017-07-27 22:00:00", tz="UTC") & date < as.POSIXct("2019-05-15 22:00:00", tz="UTC")) |
        (date >= as.POSIXct("2021-06-07 19:00:00", tz="UTC") & date < as.POSIXct("2022-09-23 00:00:00", tz="UTC"))) %>%
    select(date, everything())
})


# Convert temp, dew and skin temp from Kelvin to Celsius for each data frame
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(across(any_of(c("temp","dew","skt")), ~ . - 273.15))  # Kelvin to Celsius
})

# Calculate relative humidity
library(humidity)
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(hr = RH(temp, dew, isK = FALSE)) %>%
    select(-dew)
})

# Temperature correction using temperature environmental vertical lapse rate of 6.5 C/km
stations_validations$temp_diff <- (stations_validations$elevation_diff) / 1000 * 6.5
#elev_diff <- stations_validations[stations_validations$dataset == "era5land", c(2,8,9)]
elev_diff <- stations_validations[stations_validations$dataset == "era5", c(2,8,9)]

df_list <- lapply(seq_along(df_list), function(i) {
  
  dataset_name <- names(df_list)[i]
  
  temp_adjustment <- elev_diff$temp_diff[elev_diff$station == dataset_name]
  
  df_list[[i]] <- df_list[[i]] %>%
    rename(
      old_temp = any_of("temp"),
      old_skt = any_of("skt")) %>%
    mutate(
      temp = if ("old_temp" %in% names(.)) old_temp + temp_adjustment,
      skt = if ("old_skt" %in% names(.)) old_skt + temp_adjustment)
})

names(df_list) <- original_names

# Transform u10 and v10 to wind speed and direction for each data frame
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(
      vel10 = sqrt(u10^2 + v10^2),  # Calculate wind speed
      dir = (270 - (atan2(v10, u10) * (180 / pi))) %% 360  # Calculate wind direction in degrees
    )
})

# Calculate wind speed at 3 m based on a logarithmic attenuation
df_list <- lapply(df_list, function(df) {
  
  # Vector to store the value at x = 3 for each row
  df$vel <- NA
  
  # Iterate through each row to calculate the value at x = 3
  for (i in 1:nrow(df)) {
    y2 <- df$vel10[i]  # Get the vel value for the current row
    x1 <- 0.0001
    y1 <- 0
    x2 <- 10
    
    # Calculate the coefficients a and c based on the current y2
    a <- (y2 - y1) / (log(x2) - log(x1))
    c <- y1 - a * log(x1)
    
    # Define the logarithmic function
    log_function <- function(x) {
      a * log(x) + c
    }
    
    # Calculate the value at x = 3 for the current row
    df$vel[i] <- log_function(3)
  }
  return(df)
})

# In ERA5: Transform Mean Sea Level Pressure from Pa to hPa
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(pres = pres / 100)
})

names(df_list) <- original_names

# Accumulate precipitation and snowfall over 3h periods
df_list <- lapply(df_list, function(df) {
  df %>%
    arrange(date) %>%  # Ensure data is sorted by time
    mutate(
      date = as.POSIXct(date, tz = "UTC"),
      prec_3hr = prec + lag(prec, 1, default = 0) + lag(prec, 2, default = 0),
      snowfall_3hr = snowfall + lag(snowfall, 1, default = 0) + lag(snowfall, 2, default = 0)
    )
})

# Define the times to keep and filter rows in each df
times_to_keep <- c("00:00:00", "03:00:00", "06:00:00", "09:00:00", "12:00:00", "15:00:00", "18:00:00", "21:00:00")
df_list <- lapply(df_list, function(df) {
  df %>%
    filter(format(date, "%H:%M:%S") %in% times_to_keep)
})

# Save each data frame and rename the df list
output_directory <- "/media/ddonoso/KINGSTON/validations/era5"

df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, "%H:%M:%S", tz = "UTC"))
})

for (name in names(df_list)) {
  df_list[[name]]$date <- format(df_list[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(df_list[[name]], file = file_path, row.names = FALSE)
}

df_list <- era5
era5 <- df_list
era5land <- df_list
era5land_fossil <- df_list
era5land$fossilbluff <- era5land_fossil$fossilbluff

# Calculate mean and SD in each data series ----
setwd("/media/ddonoso/KINGSTON/validations")

# Date columns in POSIXct format
era5land <- lapply(era5land, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
})

# Create function to extract variable columns from each data frame list
calculate_stats <- function(df_station, df_era5, df_era5land, df_name) {
  
  cols_to_analyze <- c("temp", "vel", "pres")
  stats_df <- data.frame(df_name = df_name, stringsAsFactors = FALSE)
  
  # Join datasets by date, ensuring correct suffixes
  merged_df <- df_station %>%
    select(date, all_of(cols_to_analyze), any_of("hr")) %>%
    rename_with(~paste0(., "_station"), -date) %>%
    full_join(df_era5 %>%
                select(date, all_of(cols_to_analyze), any_of("hr")) %>%
                rename_with(~paste0(., "_era5"), -date),
              by = "date") %>%
    full_join(df_era5land %>%
                select(date, all_of(cols_to_analyze), any_of("hr")) %>%
                rename_with(~paste0(., "_era5land"), -date),
              by = "date")
  
  # Compute statistics for temp, vel, and prec
  for (col in cols_to_analyze) {
    col_station <- paste0(col, "_station")
    col_era5 <- paste0(col, "_era5")
    col_era5land <- paste0(col, "_era5land")
    
    stats <- merged_df %>%
      select(all_of(c(col_station, col_era5, col_era5land))) %>%
      na.omit() %>%
      summarise(
        !!paste0(col_station, "_mean") := round(mean(.data[[col_station]]), 2),
        !!paste0(col_station, "_sd") := round(sd(.data[[col_station]]), 2),
        !!paste0(col_era5, "_mean") := round(mean(.data[[col_era5]]), 2),
        !!paste0(col_era5, "_sd") := round(sd(.data[[col_era5]]), 2),
        !!paste0(col_era5land, "_mean") := round(mean(.data[[col_era5land]]), 2),
        !!paste0(col_era5land, "_sd") := round(sd(.data[[col_era5land]]), 2)
      )
    
    stats_df <- bind_cols(stats_df, stats)
  }
  
  # Compute hr statistics only if hr is present
  if ("hr_station" %in% names(merged_df)) {
    hr_stats <- merged_df %>%
      select(hr_station, hr_era5, hr_era5land) %>%
      na.omit() %>%
      summarise(
        hr_station_mean = round(mean(hr_station), 2),
        hr_station_sd = round(sd(hr_station), 2),
        hr_era5_mean = round(mean(hr_era5), 2),
        hr_era5_sd = round(sd(hr_era5), 2),
        hr_era5land_mean = round(mean(hr_era5land), 2),
        hr_era5land_sd = round(sd(hr_era5land), 2)
      )
    
    stats_df <- bind_cols(stats_df, hr_stats)
  }
  
  return(stats_df)
}

# Apply function to each dataset in lists
results_list <- purrr::map(names(data_frames), 
                    ~ calculate_stats(data_frames[[.x]], era5[[.x]], era5land[[.x]], .x))

# Combine all results into a single data frame
mean_sd_table <- bind_rows(results_list)

write.csv(mean_sd_table, "mean_sd.csv", row.names = FALSE)

