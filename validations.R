
#setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/processed_era5land")
setwd("/media/ddonoso/Pengo2/Doctorado/datos_netcdf_rema/station_series_era5land")

file_list <- list.files(pattern = "^datos.*\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

era5land <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(era5land) <- gsub("^datos|\\.csv$", "", basename(file_list))

col_names <- c("u10", "v10", "dew", "temp", "skt", "snowc", "sde", "sf", "pres", "prec", "date")
era5land <- map(era5land, ~ setNames(.x, col_names)) # Rename columns in each data frame

# Convert Julian hours in the date column for each data frame
era5land_date <- lapply(era5land, function(df) {
  df %>%
    mutate(date = as_datetime(date * 3600, origin = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"), tz = "UTC"))
})

# Subset study periods
period1 <- lapply(era5land, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    filter(date >= as.POSIXct("2012-01-01 00:00:00") & date < as.POSIXct("2014-01-01 00:00:00"))
})

# Define the times to keep and filter rows in each df
times_to_keep <- c("00:00:00", "03:00:00", "06:00:00", "09:00:00", 
                   "12:00:00", "15:00:00", "18:00:00", "21:00:00")

era5land <- lapply(era5land, function(df) {
  df %>%
    filter(format(as.POSIXct(date), "%H:%M:%S") %in% times_to_keep)
})

# Save each dataframe in the list as CSV
output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/3hourly_era5land"

for (name in names(era5land)) {
  file_path <- file.path(output_directory, paste0(name, "_era5land.csv"))
  write.csv(era5land[[name]], file = file_path, row.names = FALSE)
}

