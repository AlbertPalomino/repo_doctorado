

setwd("/media/ddonoso/KINGSTON/era5land")

file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

era5land <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(era5land) <- gsub("^era5land_|\\.csv$", "", basename(file_list))

#col_names <- c("u10", "v10", "dew", "temp", "skt", "snowc", "sde", "sf", "pres", "prec", "date")
#era5 <- map(era5, ~ setNames(.x, col_names)) # Rename columns in each data frame

df_list <- era5land

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
        (date >= as.POSIXct("2021-06-07 19:00:00", tz="UTC") & date < as.POSIXct("2022-09-23 00:00:00", tz="UTC"))
    )
})

# Convert columns 3, 4, and 5 from Kelvin to Celsius for each data frame
era5land_list_celsius <- lapply(era5land_list, function(df) {
  df %>%
    mutate(across(3:5, ~ . - 273.15))  # Convert columns 3, 4, and 5 from Kelvin to Celsius
})

# Transform u and v to wind speed and direction for each data frame
era5land_list_wind <- lapply(era5land_list_celsius, function(df) {
  df %>%
    mutate(
      vel10 = sqrt(u10^2 + v10^2),  # Calculate wind speed
      dir = (atan2(v10, u10) * (180 / pi)) %% 360  # Calculate wind direction in degrees
    )
})

# Logarithmic attenuation of wind
{
  # Known values
  x1 <- 0.1
  y1 <- 0
  x2 <- 10
  y2 <- unknown    # replace with vel value at 10
  
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
  plot(intermediate_x, intermediate_values, type = 'l', main = 'Logarithmic Curve', xlab = 'X', ylab = 'Y')
  points(c(x1, x2), c(y1, y2), col = 'red', pch = 19)  # points at known values
  points(c(3), c(value_at_3), col = 'blue', pch = 19)  # points at known values
}

# Calculate wind speed at 3m based on a logarithmic attenuation
era5land_list_vel3 <- lapply(era5land_list_wind, function(df) {
  
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
file_path= "/media/ddonoso/Pengo2/Doctorado/datos_netcdf_rema/era5land/station_locations.csv"
df <- read.csv(file_path, sep = ",", header = TRUE)
df$elev_station <- c(8, 45, 10, 5, 12, 70, 15, 10, 25, 17, 10, 25, 7, 12, 5, 93, 4, 63, 30) # Elevation of weather stations
df$elev_diff <- df$elev - df$elev_station
df$temp_diff <- (df$elev_diff) / 1000 * 6.5
temp_diff <- df$temp_diff[c(3,9,5,8,4,17,15,13)]
elev_diff <- df$elev_diff[c(3,9,5,8,4,17,15,13)]
coords <- df

# Correct temperature in era5land data
era5land_list_temp <- lapply(seq_along(era5land_list_vel3), function(i) {
  era5land_list_vel3[[i]] %>%
    rename(
      old_temp = temp,
      old_dew = dew) %>%
    mutate(
      temp = old_temp + temp_diff[i],
      dew = old_dew + temp_diff[i])
})

# Calculate relative humidity
era5land_list_hr <- lapply(era5land_list_temp, function(df) {
  df %>%
    mutate(hr = RH(old_temp, old_dew, isK = FALSE))
})

# Adjust pressure for each dataframe in the list
era5land_list <- lapply(seq_along(era5land_list_hr), function(i) {
  P0 <- 101325  # Pa
  g <- 9.81     # m/s²
  M <- 0.029    # kg/mol
  R <- 8.314    # J/(mol·K)
  T <- era5land_list_hr[[i]]$old_temp + 273.15 # degrees Kelvin
  delta_h <- elev_diff[i]
  delta_P <- P0 * (g * M / (R * T)) * delta_h  # Calculate delta_P for each station's elevation difference 
  
  era5land_list_hr[[i]] %>%
    rename(old_pres = pres) %>% 
    mutate(pres = as.numeric(old_pres) + delta_P) %>% 
    mutate(pres = pres / 100)  # Convert resulting pressure back to hPa
})

# Rename and save each dataframe in the list as CSV
names(era5land_list) <- gsub("^datos|\\.csv$", "", basename(file_list))
output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/processed_era5land"
for (name in names(era5land_list)) {
  file_path <- file.path(output_directory, paste0(name, "_era5land.csv"))
  write.csv(era5land_list[[name]], file = file_path, row.names = FALSE)
}






# Subset study periods
period1 <- lapply(era5land, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    filter(date >= as.POSIXct("2012-01-01 00:00:00") & date < as.POSIXct("2014-01-01 00:00:00"))
})

period1 <- lapply(data_frames, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    dplyr::filter(date >= as.POSIXct("2013-10-11 15:00:00") & date < as.POSIXct("2014-11-30 03:00:00"))
})

period2 <- lapply(data_frames, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    dplyr::filter(date >= as.POSIXct("2017-07-27 22:00:00") & date < as.POSIXct("2019-05-15 22:00:00"))
})

period3 <- lapply(data_frames, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    dplyr::filter(date >= as.POSIXct("2021-06-07 19:00:00") & date < as.POSIXct("2022-09-23 00:00:00"))
})


# Define the times to keep and filter rows in each df
times_to_keep <- c("00:00:00", "03:00:00", "06:00:00", "09:00:00", 
                   "12:00:00", "15:00:00", "18:00:00", "21:00:00")

era5 <- lapply(era5land, function(df) {
  df %>%
    filter(format(as.POSIXct(date), "%H:%M:%S") %in% times_to_keep)
})


times_to_keep <- c("00:00:00", "03:00:00", "06:00:00", "09:00:00", 
                   "12:00:00", "15:00:00", "18:00:00", "21:00:00")

era5_acummulated <- lapply(era5, function(df) {
  
  # Accumulate the 'prec' and 'snowfall' for the previous 3 hours
  df <- df %>%
    mutate(date = as.POSIXct(date))
  df_prec_snowfall <- df %>%
    mutate(
      time_lagged = format(date - hours(3), "%H:%M:%S")) %>% # Calculate the time window (3 hours before the current time)
    group_by(date = as.Date(date), time = format(date, "%H:%M:%S")) %>%
    summarise(
      prec = sum(prec[format(date, "%H:%M:%S") %in% c(time_lagged, time)], na.rm = TRUE),
      snowfall = sum(snowfall[format(date, "%H:%M:%S") %in% c(time_lagged, time)], na.rm = TRUE)
    ) %>%
    ungroup()
})
  # Filter the times_to_keep
  df <- df %>%
    filter(format(date, "%H:%M:%S") %in% times_to_keep)
  
  # Merge the accumulated values back to the filtered data
  df <- df %>%
    left_join(df_prec_snowfall, by = c("date" = "date", "time" = "time"))
  
  return(df)
})





# Save each dataframe in the list as CSV
output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/3hourly_era5land"

for (name in names(era5land)) {
  file_path <- file.path(output_directory, paste0(name, "_era5land.csv"))
  write.csv(era5land[[name]], file = file_path, row.names = FALSE)
}

