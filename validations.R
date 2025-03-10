library(dplyr)
library(tidyr)
library(lubridate)
library(DTWBI)
setwd("/Volumes/Pengo2/Doctorado/validations")

# Import station data ----
setwd("/media/ddonoso/Pengo2/Doctorado/station_series")
setwd("/Volumes/Pengo2/Doctorado/station_series")

file_list <- list.files(pattern = "^validation_", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too
station_list <- lapply(file_list, read.csv)
names(station_list) <- gsub("validation_|\\.csv$", "", basename(file_list))

# Rename files
stations <- station_list

# Rename data frames to match those of reanalyses
names(stations)[names(stations) == "fossil"] <- "fossilbluff"
names(stations)[names(stations) == "king"] <- "kingsejong"

# Fill hours o'clock with 00:00:00 if needed
stations <- lapply(stations, function(df) {
  df %>%
    mutate(date = as.character(date),
           date = if_else(nchar(date) == 10, paste0(date, " 00:00:00"), date),  # Append "00:00:00" if missing
           date = ymd_hms(date))  # Convert to proper datetime format
})

stations <- lapply(stations, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
})

rm(station_list)

# Export datasets
output_directory <- "/media/ddonoso/Pengo2/Doctorado/station_series/"

for (name in names(stations)) {
  stations[[name]]$date <- format(stations[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0("validation_", name, ".csv"))
  write.csv(stations[[name]], file = file_path, row.names = FALSE)
}

# Calculate elevation difference and distance between station and grid point ----

# Read weather stations and grid points
stations <- read.csv("all_coords_long.csv", sep = ",", header = TRUE)

# Rename models and stations - recode factor levels
stations$station_type <- {recode(stations$dataset, 
                                 station = "Weather Stations", 
                                 era5land = "ERA5 Land", 
                                 era5 = "ERA5", 
                                 racmo5.5 = "RACMO 5.5km", 
                                 racmo11 = "RACMO 11km")
} 
stations$station_label <- {recode(stations$station, 
                                  ferraz = "Ferraz", 
                                  escudero = "Frei", 
                                  carlini = "Carlini", 
                                  prat = "Prat", 
                                  jci = "Juan Carlos I",
                                  byers = "Byers",
                                  gdg = "Gabriel de Castilla",
                                  ohiggins = "O'Higgins",
                                  esperanza = "Esperanza",
                                  racer = "Racer Rock",
                                  palmer = "Palmer",
                                  hugo = "Hugo Island",
                                  vernadsky = "Vernadsky",
                                  dismal = "Dismal Island",
                                  sanmartin = "San Martín",
                                  hurd = "Hurd Glacier",
                                  rothera = "Rothera",
                                  fossilbluff = "Fossil Bluff",
                                  kirkwood = "Kirkwood Island",
                                  kingsejong = "King Sejong")
}

stations_racmo11 <- read.csv("coords_racmo11.csv", sep = ",", header = TRUE)
stations_racmo11 <- stations_racmo11 %>% rename(station = Var2, racmo_lat = racmo55_lat, racmo_lon = racmo55_lon, racmo_elev = racmo55_elev)
stations$racmo_elev <- NA
stations <- stations %>%
  left_join(stations_racmo11 %>% select(station, racmo_elev), 
            by = "station") %>%
  mutate(elevation = if_else(dataset == "racmo11", racmo_elev, elevation)) %>%
  select(-racmo_elev)  # Remove redundant columns

stations_racmo55 <- read.csv("coords_racmo55.csv", sep = ",", header = TRUE)
stations_racmo55 <- stations_racmo55 %>% rename(station = Var2, racmo_elev = racmo55_elev)
stations <- stations %>%
  left_join(stations_racmo55 %>% select(station, racmo_elev), 
            by = "station") %>%
  mutate(elevation = if_else(dataset == "racmo5.5", racmo_elev, elevation)) %>%
  select(-racmo_elev)

new_stations <- read.csv("elevation_stations_090325.csv", sep = ";", header = TRUE)
stations <- stations %>%
  left_join(new_stations %>% select(station, new_elevation), 
            by = "station") %>%
  mutate(elevation = if_else(dataset == "station", new_elevation, elevation)) %>%
  select(-new_elevation)

# Remove stations excluded from validations
stations_validations <- stations %>%
  dplyr::filter(!station %in% c("hurd", "byers", "ferraz", "dismal", "kirkwood", "racer", "hugo", "gdg"))

stations_validations <- stations_validations %>%
  group_by(station) %>%
  mutate(elevation_diff = elevation - elevation[dataset == "station"]) %>%
  ungroup()

# Calculate distances using distHaversine for all points
library(geosphere)
library(dplyr)

# Number of groups (each has 12 rows)
n_groups <- nrow(stations_validations) / 12  

# Initialize an empty vector for distances
stations_validations$distance <- 0

# Loop through each group of 12 rows
for (i in seq_len(n_groups)) {
  
  start_idx <- 1 + (12 * (i - 1))
  end_idx <- 12 + (12 * (i - 1))
  
  # Extract station coordinates (longitude in col 4, latitude in col 3)
  coords1 <- as.matrix(stations_validations[1:12, c(4, 3)])
  
  # Extract reanalysis coordinates (next 12 rows)
  coords2 <- as.matrix(stations_validations[start_idx:end_idx, c(4, 3)])
  
  # Compute Haversine distance and convert to km
  stations_validations$distance[start_idx:end_idx] <- distHaversine(coords1, coords2) / 1000  
}

output_directory <- "/Volumes/Pengo2/Doctorado/validations"
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
# Process ERA5 and ERA5-Land ----

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
output_directory <- "/media/ddonoso/Pengo2/Doctorado/validations/era5"

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

# Import processed ERA5 and ERA5-Land series ----
setwd("/media/ddonoso/Pengo2/Doctorado/validations/era5")
setwd("/Volumes/Pengo2/Doctorado/validations/era5")
file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too
df_list <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(df_list) <- gsub("\\.csv$", "", basename(file_list))
era5 <- df_list
era5 <- lapply(era5, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
})

setwd("/Volumes/Pengo2/Doctorado/validations/era5land")
file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = FALSE)
df_list <- lapply(file_list, read.csv)
names(df_list) <- gsub("\\.csv$", "", basename(file_list))
era5land <- df_list
era5land <- lapply(era5land, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz="UTC"))
})

# Calculate mean and SD in each data series ----
setwd("/media/ddonoso/Pengo2/Doctorado/validations")
setwd("/Volumes/Pengo2/Doctorado/validations")

# Create function to extract variable columns from each data frame list
calculate_mean_sd <- function(df_station, df_era5, df_era5land, df_amps, df_name) {
  
  cols_to_analyze <- c("temp", "vel", "pres")
  stats_df <- data.frame(df_name = df_name, stringsAsFactors = FALSE)
  
  # Join data sets by date, adding data set suffixes
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
  
  # Conditionally join AMPS if available
  if (!is.null(df_amps)) {
    merged_df <- full_join(merged_df,
                           df_amps %>%
                             select(date, all_of(cols_to_analyze), any_of("hr")) %>%
                             rename_with(~paste0(., "_amps"), -date),
                           by = "date")
  }
  
  # Compute statistics for temp, vel, and pres
  for (col in cols_to_analyze) {
    col_station <- paste0(col, "_station")
    col_era5 <- paste0(col, "_era5")
    col_era5land <- paste0(col, "_era5land")
    col_amps <- paste0(col, "_amps")
    
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
    
    # If AMPS exists, add its statistics
    if (col_amps %in% names(merged_df)) {
      amps_stats <- merged_df %>%
        select(any_of(col_amps)) %>%
        na.omit() %>%
        summarise(
          !!paste0(col_amps, "_mean") := round(mean(.data[[col_amps]]), 2),
          !!paste0(col_amps, "_sd") := round(sd(.data[[col_amps]]), 2)
        )
      
      stats <- bind_cols(stats, amps_stats)
    }
    
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
    
    # If AMPS exists, add hr statistics
    if ("hr_amps" %in% names(merged_df)) {
      hr_amps_stats <- merged_df %>%
        select(hr_amps) %>%
        na.omit() %>%
        summarise(
          hr_amps_mean = round(mean(hr_amps), 2),
          hr_amps_sd = round(sd(hr_amps), 2)
        )
      
      hr_stats <- bind_cols(hr_stats, hr_amps_stats)
    }
    
    stats_df <- bind_cols(stats_df, hr_stats)
  }
  
  return(stats_df)
}

# Apply function to all data sets
use_amps <- exists("amps") # Check if AMPS exists in the global environment

results_list <- purrr::map(names(stations), ~ {
  station_name <- .x
  
  calculate_mean_sd(
    df_station = stations[[station_name]], 
    df_era5 = era5[[station_name]], 
    df_era5land = era5land[[station_name]], 
    df_amps = if (use_amps) amps[[station_name]] else NULL, 
    df_name = station_name
  )
})

# Combine all results into a single data frame
mean_sd_table <- bind_rows(results_list)

write.csv(mean_sd_table, "mean_sd.csv", row.names = FALSE)

# Calculate nbias, nrmse, nmae ----

calculate_stats <- function(stations, era5, era5land, amps) {
  
  cols_to_analyze <- c("temp", "vel", "pres", "hr")
  datasets <- list(era5 = era5, era5land = era5land)
  
  # Initialise data frames where results will be stored
  results_df <- data.frame(
    station = character(),
    variable = character(),
    dataset = character(),
    nbias = numeric(),
    nrmse = numeric(),
    nmae = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each station (data frame name)
  for (station_name in names(stations)) { # station_name = "carlini"
    
    df_station <- stations[[station_name]]
    
    # Loop through each variable
    for (col in cols_to_analyze) { # col = "temp"
      
      # Loop through each dataset (era5, era5land)
      for (dataset_name in names(datasets)) { # dataset_name = "era5land"
        
        df_reanalysis <- datasets[[dataset_name]][[station_name]]
        
        # Check if both data frames have the column and the date column exists
        if (col %in% names(df_station) && col %in% names(df_reanalysis) && "date" %in% names(df_station) && "date" %in% names(df_reanalysis)) {
          
          # Merge data frames by date
          merged_df <- full_join(
            df_station %>% select(date, any_of(cols_to_analyze)),
            df_reanalysis %>% select(date, all_of(cols_to_analyze)),
            by = "date",
            suffix = c("_station", paste0("_", dataset_name))
          )
          
          # Remove rows with NA in either column
          valid_rows <- merged_df %>% filter(!is.na(!!sym(paste0(col, "_station"))), !is.na(!!sym(paste0(col, "_", dataset_name))))
          
          # Compute yearly mean for normalization
          valid_rows <- valid_rows %>%
            group_by(year = year(date)) %>%
            mutate(mean_col_year = mean(!!sym(paste0(col, "_station")), na.rm = TRUE)) %>%
            ungroup()
          
          if (nrow(valid_rows) > 0) {
            # Calculate normalised bias
            nbias_value <- sum((valid_rows[[paste0(col, "_", dataset_name)]] - valid_rows[[paste0(col, "_station")]]) /
                                 valid_rows$mean_col_year, na.rm = TRUE) / nrow(valid_rows)
            # Calculate normalised RMSE
            nrmse_value <- sqrt(sum(((valid_rows[[paste0(col, "_", dataset_name)]] - valid_rows[[paste0(col, "_station")]]) / 
                                           valid_rows$mean_col_year)^2, na.rm = TRUE) / nrow(valid_rows))
            # Calculate normalised MAE
            nmae_value <- compute.nmae(valid_rows[[paste0(col, "_", dataset_name)]], valid_rows[[paste0(col, "_station")]])
              
          } else {
            nbias_value <- NA
            nrmse_value <- NA
            nmae_value <- NA
          }
          
          # Store result
          results_df <- results_df %>%
            bind_rows(data.frame(
              station = station_name,
              variable = col,
              dataset = dataset_name,
              nbias = round(nbias_value, 3),
              nrmse = round(nrmse_value, 3),
              nmae = round(nmae_value, 3)
            ))
        } else {
          # Store NA for missing columns
          results_df <- results_df %>%
            add_row(station = station_name, 
                    variable = col, 
                    dataset = dataset_name, 
                    nbias = NA,
                    nrmse = NA,
                    nmae = NA)
        }
      }
    }
  }

  return(results_df)
}

# Apply function to all data sets and save results
stats_results <- calculate_stats(stations, era5, era5land)

# Reshape the results into a wide format
stats_results_wide <- stats_results %>%
  pivot_wider(
    names_from = c(variable, dataset),
    values_from = c(nbias, nrmse, nmae),
    names_sep = "_"
  )

write.csv(stats_results, "stats_results.csv", row.names = FALSE)
write.csv(stats_results_wide, "stats_results_wide.csv", row.names = FALSE)

# nbias, nrmse, nmae per period ----

calculate_stats_period <- function(stations, era5, era5land) {
  
  cols_to_analyze <- c("temp", "vel", "pres", "hr")
  datasets <- list(era5 = era5, era5land = era5land)
  
  # Initialize an empty results data frame
  results_df <- data.frame(
    station = character(),
    variable = character(),
    dataset = character(),
    period = character(),
    nbias = numeric(),
    nrmse = numeric(),
    nmae = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Define period groups
  get_period <- function(date) {
    if (date < as.Date("2015-01-01")) {
      return("2013/14")
    } else if (date > as.Date("2016-01-01") & date < as.Date("2020-12-31")) {
      return("2017/19")
    } else if (date > as.Date("2020-01-01")) {
      return("2021/22")
    } else {
      return(NA)  # For missing or invalid dates
    }
  }
  
  # Loop through each station (data frame name)
  for (station_name in names(stations)) {
    
    df_station <- stations[[station_name]]
    
    # Loop through each variable
    for (col in cols_to_analyze) {
      
      # Loop through each dataset (era5, era5land)
      for (dataset_name in names(datasets)) {
        
        df_reanalysis <- datasets[[dataset_name]][[station_name]]
        
        # Check if both data frames have the column and the date column exists
        if (col %in% names(df_station) && col %in% names(df_reanalysis) && "date" %in% names(df_station) && "date" %in% names(df_reanalysis)) {
          
          # Merge data frames by date
          merged_df <- full_join(
            df_station %>% select(date, any_of(cols_to_analyze)),
            df_reanalysis %>% select(date, all_of(cols_to_analyze)),
            by = "date",
            suffix = c("_station", paste0("_", dataset_name)) # Add suffix to columns from x and y joined data frames
          ) %>%
            mutate(period = sapply(date, get_period))  # Assign period group
          
          # Loop through each period
          for (period in unique(merged_df$period)) {
            
            period_df <- merged_df %>% filter(period == !!period)
            
            if (nrow(period_df) > 0) {
              
              # Remove rows with NA in either column
              valid_rows <- period_df %>% filter(!is.na(!!sym(paste0(col, "_station"))), !is.na(!!sym(paste0(col, "_", dataset_name))))
              
              # Calculate yearly mean for normalization
              valid_rows <- valid_rows %>%
                group_by(year = year(date)) %>%
                mutate(mean_col_year = mean(!!sym(paste0(col, "_station")), na.rm = TRUE)) %>%
                ungroup()
              
              if (nrow(valid_rows) > 0) {
                # Calculate normalised bias
                nbias_value <- sum((valid_rows[[paste0(col, "_", dataset_name)]] - valid_rows[[paste0(col, "_station")]]) /
                                     valid_rows$mean_col_year, na.rm = TRUE) / nrow(valid_rows)
                # Calculate normalised RMSE
                nrmse_value <- sqrt(sum(((valid_rows[[paste0(col, "_", dataset_name)]] - valid_rows[[paste0(col, "_station")]]) / 
                                           valid_rows$mean_col_year)^2, na.rm = TRUE) / nrow(valid_rows))
                # Calculate normalised MAE
                nmae_value <- compute.nmae(valid_rows[[paste0(col, "_", dataset_name)]], valid_rows[[paste0(col, "_station")]])
                
              } else {
                nbias_value <- NA
                nrmse_value <- NA
                nmae_value <- NA
              }
              
              # Store result
              results_df <- results_df %>%
                bind_rows(data.frame(
                  station = station_name,
                  variable = col,
                  dataset = dataset_name,
                  period = period,
                  nbias = round(nbias_value, 3),
                  nrmse = round(nrmse_value, 3),
                  nmae = round(nmae_value, 3)
                  ))
            }
          }
          
        } else {
          # Store NA for missing columns
          results_df <- results_df %>%
            add_row(station = station_name, 
                    variable = col, 
                    dataset = dataset_name, 
                    period = NA, 
                    nbias = NA,
                    nrmse = NA,
                    nmae = NA)
        }
      }
    }
  }
  
  return(results_df)
}

# Apply function to all datasets and save results
stats_period_results <- calculate_stats_period(stations, era5, era5land)

# Reshape the results into a wide format
stats_period_results_wide <- stats_period_results %>%
  pivot_wider(
    names_from = c(variable, dataset, period),
    values_from = c(nbias, nrmse, nmae),
    names_sep = "_"
  )

write.csv(stats_period_results, "stats_period_results.csv", row.names = FALSE)
write.csv(stats_period_results_wide, "stats_period_results_wide.csv", row.names = FALSE)

# Seasonal bias ----

# Add a Season factor to data frames
assign_season <- function(date) {
  month <- as.numeric(format(date, "%m"))
  if (month %in% c(12, 1, 2)) {
    return("DJF")
  } else if (month %in% c(3, 4, 5)) {
    return("MAM")
  } else if (month %in% c(6, 7, 8)) {
    return("JJA")
  } else if (month %in% c(9, 10, 11)) {
    return("SON")
  }
}

stations <- lapply(stations, function(df) {
  df$season <- sapply(df$date, assign_season)
  return(df)
})

calculate_seasonal_bias <- function(stations, era5, era5land) {
  
  cols_to_analyze <- c("temp", "vel", "pres", "hr")
  datasets <- list(era5 = era5, era5land = era5land)
  
  # Initialise data frames where results will be stored
  results_df <- data.frame(
    station = character(),
    variable = character(),
    dataset = character(),
    season = character(),
    bias = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through each station (data frame name)
  for (station_name in names(stations)) { # station_name = "carlini"
    
    df_station <- stations[[station_name]]
    
    # Loop through each variable
    for (col in cols_to_analyze) { # col = "temp"
      
      # Loop through each dataset (era5, era5land)
      for (dataset_name in names(datasets)) { # dataset_name = "era5land"
        
        df_reanalysis <- datasets[[dataset_name]][[station_name]]
        
        # Check if both data frames have the column and the date column exists
        if (col %in% names(df_station) && col %in% names(df_reanalysis) && "date" %in% names(df_station) && "date" %in% names(df_reanalysis)) {
          
          # Merge data frames by date
          merged_df <- full_join(
            df_station %>% select(date, season, any_of(cols_to_analyze)),
            df_reanalysis %>% select(date, all_of(cols_to_analyze)),
            by = "date",
            suffix = c("_station", paste0("_", dataset_name))
          )
          
          # Remove rows with NA in either column
          valid_rows <- merged_df %>% filter(!is.na(!!sym(paste0(col, "_station"))), !is.na(!!sym(paste0(col, "_", dataset_name))))
          
          # Compute yearly mean for normalization
          valid_rows <- valid_rows %>%
            group_by(year = year(date)) %>%
            mutate(mean_col_year = mean(!!sym(paste0(col, "_station")), na.rm = TRUE)) %>%
            ungroup()
          
          if (nrow(valid_rows) > 0) {
            # Calculate seasonal bias
            bias_value <- valid_rows %>%
              group_by(season) %>%
              summarise(
                bias = round(mean((!!sym(paste0(col, "_", dataset_name)) - !!sym(paste0(col, "_station"))), na.rm = TRUE), 6)
              )
          } else {
            bias_value <- NA

          }
          
          # Store result
          results_df <- results_df %>%
            bind_rows(data.frame(
              station = station_name,
              variable = col,
              dataset = dataset_name,
              season = bias_value$season,
              bias = bias_value$bias
            ))
        } else {
          # Store NA for missing columns
          results_df <- results_df %>%
            add_row(station = station_name, 
                    variable = col, 
                    dataset = dataset_name, 
                    season = NA,
                    bias = NA)
        }
      }
    }
  }
  
  return(results_df)
}

# Apply function to all data sets and save results
seasonal_bias <- calculate_seasonal_bias(stations, era5, era5land)

# Reshape the results into a wide format
seasonal_bias_wide <- seasonal_bias %>%
  pivot_wider(
    names_from = c(variable, dataset, season),
    values_from = c(bias),
    names_sep = "_"
  )

write.csv(seasonal_bias, "seasonal_bias.csv", row.names = FALSE)
write.csv(seasonal_bias_wide, "seasonal_bias_wide.csv", row.names = FALSE)



calculate_seasonal_bias_period <- function(stations, era5, era5land) {
  
  cols_to_analyze <- c("temp", "vel", "pres", "hr")
  datasets <- list(era5 = era5, era5land = era5land)
  
  # Initialize an empty results data frame
  results_df <- data.frame(
    station = character(),
    variable = character(),
    dataset = character(),
    period = character(),
    season = character(),
    bias = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Define period groups
  get_period <- function(date) {
    if (date < as.Date("2015-01-01")) {
      return("2013/14")
    } else if (date > as.Date("2016-01-01") & date < as.Date("2020-12-31")) {
      return("2017/19")
    } else if (date > as.Date("2020-01-01")) {
      return("2021/22")
    } else {
      return(NA)  # For missing or invalid dates
    }
  }
  
  # Loop through each station (data frame name)
  for (station_name in names(stations)) {
    
    df_station <- stations[[station_name]]
    
    # Loop through each variable
    for (col in cols_to_analyze) {
      
      # Loop through each dataset (era5, era5land)
      for (dataset_name in names(datasets)) {
        
        df_reanalysis <- datasets[[dataset_name]][[station_name]]
        
        # Check if both data frames have the column and the date column exists
        if (col %in% names(df_station) && col %in% names(df_reanalysis) && "date" %in% names(df_station) && "date" %in% names(df_reanalysis)) {
          
          # Merge data frames by date
          merged_df <- full_join(
            df_station %>% select(date, season, any_of(cols_to_analyze)),
            df_reanalysis %>% select(date, all_of(cols_to_analyze)),
            by = "date",
            suffix = c("_station", paste0("_", dataset_name)) # Add suffix to columns from x and y joined data frames
          ) %>%
            mutate(period = sapply(date, get_period))  # Assign period group
          
          # Loop through each period
          for (period in unique(merged_df$period)) {
            
            period_df <- merged_df %>% filter(period == !!period)
            
            if (nrow(period_df) > 0) {
              
              # Remove rows with NA in either column
              valid_rows <- period_df %>% filter(!is.na(!!sym(paste0(col, "_station"))), !is.na(!!sym(paste0(col, "_", dataset_name))))
 
              if (nrow(valid_rows) > 0) {
                # Calculate seasonal bias
                bias_value <- valid_rows %>%
                  group_by(season) %>%
                  summarise(
                    bias = round(mean((!!sym(paste0(col, "_", dataset_name)) - !!sym(paste0(col, "_station"))), na.rm = TRUE), 6)
                  )
              } else {
                bias_value <- NA
              }
              
              # Store result
              results_df <- results_df %>%
                bind_rows(data.frame(
                  station = station_name,
                  variable = col,
                  dataset = dataset_name,
                  period = period,
                  season = bias_value$season,
                  bias = bias_value$bias
                ))
            }
          }
          
        } else {
          # Store NA for missing columns
          results_df <- results_df %>%
            add_row(station = station_name, 
                    variable = col, 
                    dataset = dataset_name, 
                    period = NA, 
                    season = NA,
                    bias = NA)
        }
      }
    }
  }
  
  return(results_df)
}

# Apply function to all datasets and save results
seasonal_bias_period_results <- calculate_seasonal_bias_period(stations, era5, era5land)

# Reshape the results into a wide format
seasonal_bias_period_results_wide <- seasonal_bias_period_results %>%
  pivot_wider(
    names_from = c(variable, dataset, period, season),
    values_from = c(bias),
    names_sep = "_"
  )

write.csv(seasonal_bias_period_results, "seasonal_bias_period_results.csv", row.names = FALSE)
write.csv(seasonal_bias_period_results_wide, "seasonal_bias_period_results_wide.csv", row.names = FALSE)

# Analyse daily data ----
  # Process daily ERA5 and ERA5-Land ----

#path <- "/media/ddonoso/KINGSTON/era5land"
#path <- "/media/ddonoso/KINGSTON/era5land_fossilbluff"
path <- "/media/ddonoso/KINGSTON/era5"
path <- "/Volumes/Pengo2/Doctorado/validations/era5monthly"
path <- "/Volumes/Pengo2/Doctorado/validations/era5land_daily"
path <- "/Volumes/Pengo2/Doctorado/validations/era5land_monthly"
path <- "/Volumes/Pengo2/Doctorado/validations/era5land_fossilbluff_daily"
path <- "/Volumes/Pengo2/Doctorado/validations/era5land_fossilbluff_monthly"
path <- "/Volumes/Pengo2/Doctorado/validations/racmo11"
setwd(path)

file_list <- list.files(pattern = "*\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too

df_list <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(df_list) <- gsub("^era5land_|\\.csv$", "", basename(file_list))
names(df_list) <- gsub("^era5_|\\.csv$", "", basename(file_list))
names(df_list) <- gsub("\\.csv$", "", basename(file_list))

# Remove unwanted stations
remove <- c("ferraz", "byers", "dismal", "kirkwood", "racer", "hugo", "gdg","hurd")
df_list <- df_list[!names(df_list) %in% remove]

# Store original names
original_names <- names(df_list)

# Daily data - Create date column
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(paste(Year, Month, Day, sep = "-"), format = "%Y-%m-%d", tz = "UTC")) %>%
    select(date, everything())  %>%    # Reorder columns, date first
    select(-c(Year, Month, Day))
})

# Monthly data - Create date column
df_list <- lapply(df_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d", tz = "UTC")) %>% 
    select(date, everything())  %>% 
    select(-c(Year, Month))
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

stations_validations <- read.csv("/Volumes/Pengo2/Doctorado/validations/coords_dist.csv", header = T, sep = ",")
elev_diff <- stations_validations[stations_validations$dataset == "era5land", c(2,8,9)]
#elev_diff <- stations_validations[stations_validations$dataset == "era5", c(2,8,9)]

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

# In ERA5-Land: Adjust pressure according to elevation difference
df_list <- lapply(seq_along(df_list), function(i) {

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

# Save each data frame and rename the df list
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5daily"
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5monthly"
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5land_daily"
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5land_monthly"
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5land_fossilbluff_daily"
output_directory <- "/Volumes/Pengo2/Doctorado/validations/era5land_fossilbluff_monthly"

for (name in names(df_list)) {
  df_list[[name]]$date <- format(df_list[[name]]$date, "%Y-%m-%d")
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(df_list[[name]], file = file_path, row.names = FALSE)
}

era5land_monthly$fossilbluff <- era5land_fossil_monthly$fossilbluff
era5land_fossil_monthly <- df_list
era5land_daily$fossilbluff <- era5land_fossil_daily$fossilbluff
era5land_fossil_daily <- df_list
era5land_monthly <- df_list
era5land_daily <- df_list
era5monthly <- df_list
era5daily <- df_list
era5 <- df_list
era5land <- df_list
era5land_fossil <- df_list
era5land$fossilbluff <- era5land_fossil$fossilbluff

