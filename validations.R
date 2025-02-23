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

# Export datasets
output_directory <- "/media/ddonoso/KINGSTON/station_series/"

for (name in names(data_frames)) {
  data_frames[[name]]$date <- format(data_frames[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0("validation_", name, ".csv"))
  write.csv(data_frames[[name]], file = file_path, row.names = FALSE)
}

# Calculate elevation difference between station and grid point ----
stations_validations <- stations_validations %>%
  group_by(station) %>%
  mutate(elevation_diff = elevation - elevation[dataset == "station"]) %>%
  ungroup()

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
# Import and process ERA5-Land ----
setwd("/media/ddonoso/KINGSTON/era5land")

file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too

era5land <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(era5land) <- gsub("^era5land_|\\.csv$", "", basename(file_list))

df_list <- era5land

# Remove unwanted stations
remove <- c("ferraz", "byers", "dismal", "kirkwood", "racer", "hugo", "gdg","hurd")
df_list <- df_list[!names(df_list) %in% remove]

# Store original names
original_names <- names(df_list)

# Convert Julian hours in the date column for each data frame
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(date = as_datetime(date * 3600, origin = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"), tz = "UTC"))
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
    mutate(across(4:6, ~ . - 273.15))  # Kelvin to Celsius
})

# Transform u10 and v10 to wind speed and direction for each data frame
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(
      vel10 = sqrt(u10^2 + v10^2),  # Calculate wind speed
      dir = (270 - (atan2(v10, u10) * (180 / pi))) %% 360  # Calculate wind direction in degrees
    )
})

# Calculate wind speed at 3m based on a logarithmic attenuation
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

# Temperature correction using temperature environmental vertical lapse rate of 6.5 C/km
stations_validations$temp_diff <- (stations_validations$elevation_diff) / 1000 * 6.5
elev_diff <- stations_validations[stations_validations$dataset == "era5land", c(2,8,9)]

df_list <- lapply(seq_along(df_list), function(i) {
  
  dataset_name <- names(df_list)[1]
  
  temp_adjustment <- elev_diff$temp_diff[elev_diff$station == dataset_name]
  
  df_list[[i]] %>%
    rename(
      old_temp = temp,
      old_dew = dew,
      old_skt = skt) %>%
    mutate(
      temp = old_temp + temp_adjustment,
      dew = old_dew + temp_adjustment,
      skt = old_skt + temp_adjustment)
})

#file_path= "/media/ddonoso/Pengo2/Doctorado/datos_netcdf_rema/era5land/station_locations.csv"
#df <- read.csv(file_path, sep = ",", header = TRUE)
#df$elev_station <- c(8, 45, 10, 5, 12, 70, 15, 10, 25, 17, 10, 25, 7, 12, 5, 93, 4, 63, 30) # Elevation of weather stations
#df$elev_diff <- df$elev - df$elev_station
#df$temp_diff <- (df$elev_diff) / 1000 * 6.5
#temp_diff <- df$temp_diff[c(3,9,5,8,4,17,15,13)]
#elev_diff <- df$elev_diff[c(3,9,5,8,4,17,15,13)]
#coords <- df

names(df_list) <- original_names

# Calculate relative humidity
library(humidity)
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(hr = RH(old_temp, old_dew, isK = FALSE))
})

# Adjust pressure according to difference in elevation
df_list <- lapply(seq_along(df_list), function(i) { i=1
  P0 <- 101325  # Pa
  g <- 9.81     # m/s²
  M <- 0.029    # kg/mol
  R <- 8.314    # J/(mol·K)
  T <- df_list[[i]]$old_temp + 273.15 # degrees Kelvin
  
  dataset_name <- names(df_list)[i]

  delta_h <- elev_diff$elevation_diff[elev_diff$station == dataset_name]
  delta_P <- P0 * (g * M / (R * T)) * delta_h  # Calculate delta_P for each station's elevation difference 
  
  df_list[[i]] %>%
    rename(old_pres = pres) %>% 
    mutate(pres = as.numeric(old_pres) + delta_P) %>% 
    mutate(pres = pres / 100)  # Convert resulting pressure back to hPa
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

# Save each dataframe in the list as CSV
output_directory <- "/media/ddonoso/KINGSTON/validations/era5land"

for (name in names(df_list)) {
  df_list[[name]]$date <- format(df_list[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(df_list[[name]], file = file_path, row.names = FALSE)
}

