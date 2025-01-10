library(rNOMADS)

grib_data <- ReadGrib(file_path, levels = NULL, variables = NULL)

print(grib_data)


library(metR)
library(data.table)

file_path <- "/mnt/dat4/grib_amps/amps2003_05/2003091912_MM5_d6_f006.grb"
data <- fread(file_path)

# Inspect the data
print(data)


library(gribr)

# Read the GRIB file
file_path <- "/path/to/your/file.grb"
grib_data <- grib_open(file_path)

# Explore GRIB metadata
grib_list <- grib_ls(grib_data)
print(grib_list)

# Read a specific variable
data <- grib_get(grib_data, "variable_name")
print(data)

