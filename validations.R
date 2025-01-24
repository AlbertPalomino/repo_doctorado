
#setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/processed_era5land")
setwd("/home/ddonoso/Desktop/datos_Albert/era5/downloads")

file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

era5 <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(era5) <- gsub("^era5_|\\.csv$", "", basename(file_list))

#col_names <- c("u10", "v10", "dew", "temp", "skt", "snowc", "sde", "sf", "pres", "prec", "date")
#era5 <- map(era5, ~ setNames(.x, col_names)) # Rename columns in each data frame

# Convert Julian hours in the date column for each data frame
era5 <- lapply(era5, function(df) {
  df %>%
    mutate(date = as_datetime(date, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), tz = "UTC"))
})

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

