# Load necessary libraries
detach("package:stats", unload = TRUE)
library (tidyverse)
library (fastDummies)
library(rjson)
library(gridExtra)
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)
library(ggplot2)
library(purrr)
library(stats)
library(humidity)
#library(ecmwfr)

# Processing penguin colony database ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration")

# Dataset with all penguin counts available on penguinmap.com
d <- read.csv("CountQuery_V_4_1.csv")
d$site_id <- as.factor(d$site_id)
d <- d %>% rename(longitude = longitude_epsg_4326)
d <- d %>% rename(latitude = latitude_epsg_4326)
d$common_name <- gsub(" penguin", "", d$common_name) # subtract "_penguin" from column "common name"

# count number of nests, chicks and adults. Subset only nests, sites with >1 census in area 48.1
d <- subset(d, count_type=="nests")

# remove duplicates in years with multiple counts
d1 <- d %>%
  group_by(site_id, common_name, season_starting) %>%
  arrange(accuracy, desc(penguin_count)) %>%
  slice(1) # Group by colony and year, and filter rows based on the conditions highest accuracy (1 is highest) and highest penguin count 

d1 <- d1 %>%
  group_by(site_id) %>%
  arrange(desc(latitude)) %>%
  ungroup(site_id) # Rearrange df by latitude and colony

# a <- count(d,site_id)
# d <- merge(d,a,by="site_id")
# d2 <- subset(d2, d2$cammlr_region=="48.1")
# d2 <- subset(d, d$n>1) # selecting colonies >= 1 counts
# d10 <- subset(d, d$n>9) # selecting colonies >= 10 counts

# take only 1st record from each colony for mapping purposes
locations <- distinct(d1, site_id, .keep_all=TRUE)
write.csv(locations, "/Volumes/Pengo1/Doctorado/data exploration/locations.csv", row.names=FALSE)

col2 <- joined_data %>%
  mutate_at(vars(2:7), ~ifelse(.<2, NA, .))

col10 <- joined_data %>%
  mutate_at(vars(2:7), ~ifelse(.<10, NA, .))

#New variable (0,1) showing all colonies <10 counts
col10$no_count <- ifelse(rowSums(is.na(col10[, 2:7])) == length(col10[, 2:7]), 1, NA) 
col10$no_count <- as.factor(col10$no_count)

write.csv(col10, "/Volumes/Pengo1/Doctorado/data exploration/col10.csv", row.names=FALSE)

  # separating database by species ----

adpe <- subset(d1, common_name=="adelie")
n <- count(adpe, site_id)

adpe <- merge(adpe,n,by="site_id")
adpe <- arrange(adpe, desc(latitude))
Adelie <- subset(adpe, adpe$n>1) # colonies with at least 2 counts
Adelie10 <- subset(adpe, adpe$n>9) # colonies with at least 10 counts

acol <- distinct(Adelie10, site_id, .keep_all = TRUE)
write.csv(acol, "acol.csv", row.names=FALSE)

chpe <- subset(d1, common_name=="chinstrap")
n <- count(chpe, site_id)
chpe <- merge(chpe, n, by="site_id")
chpe <- arrange(chpe, desc(latitude))
Chinstrap <- subset(chpe, chpe$n>1)
Chinstrap10 <- subset(chpe, chpe$n>9)

chcol <- distinct(Chinstrap10, site_id, .keep_all = TRUE)
write.csv(chcol, "chcol.csv", row.names=FALSE)

gepe <- subset(d1, common_name=="gentoo")
n <- count(gepe, site_id)
gepe <- merge(gepe, n, by="site_id")
gepe <- arrange(gepe, desc(latitude))
Gentoo <- subset(gepe, gepe$n>1)
Gentoo10 <- subset(gepe, gepe$n>9)

gecol <- distinct(Gentoo10, site_id, .keep_all = TRUE)
write.csv(gecol, "gecol.csv", row.names=FALSE)

# extract chinstrap df for modeling ----

Chinstrap10 <- Chinstrap10 %>%
  filter( season_starting > 1979) %>%
  mutate(season = Chinstrap10$season_starting - (min(Chinstrap10$season_starting) -1)) %>%
  select(-cammlr_region,-count_type,-vantage,-reference) %>%
  rename(
    site = site_id
    )

chinstrap <- Chinstrap10 %>%
  select(-longitude, -latitude, -site_name, -common_name, -day, -month, -year, -season_starting, -n) %>%
  rename(Y = penguin_count)

write.csv(chinstrap, "chinstrap.csv", row.names = FALSE)

start_year <- chinstrap %>%
  group_by(site) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-Y, -accuracy)

write.csv(start_year, "first_year.csv", row.names = FALSE)

# timeline plot for each colony ----
df <- adpe
years <- seq(min(df$season_starting), max(df$season_starting)) # Generate a sequence of years
blank_df <- data.frame(year = years) # Create a blank dataframe with years
result <- left_join(blank_df, df, by = "year", multiple = "all") # Perform left join with df2

result <- spread(result, key = site_id, value = penguin_count, fill = NA) # Spread: each level of site_id into a new column, fill with penguin_count values
result <- result %>%
  select(1,15:ncol(.)) # build new df selecting only columns for colonies

result <- result %>%
  group_by(year) %>%
  summarise_all(~first(na.omit(.))) # group all rows from same year and summarizes values in each colony column, keeping first non-NA when there are several and otherwise omitting NAs

adpe <- adpe %>%
  mutate(presence = ifelse(is.na(penguin_count), 0, 1)) # add 0 and 1 column to adpe representing presence of census data in each year

adpe <- adpe %>%
  filter(presence == 1)

ggplot(adpe, aes(x = season_starting, xend = lead(season_starting), y = site_name, yend = site_name, color = factor(presence))) +
  geom_segment() +
  scale_color_manual(values = c("red", "green"), labels = c("Absent", "Present")) +
  labs(title = "Presence or Absence of Data", x = "Year", y = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")
  
  labs(title = "Adélie time series", x = "Year", y = "nest count") +
  scale_color_discrete(name = "site_id")

  
df <- adpe
all_years <- seq(min(df$season_starting), max(df$season_starting))
all_colonies <- unique(df$site_name)
complete_data <- expand.grid(season_starting = all_years, site_name = all_colonies) # Generate a complete set of combinations of years and colonies

df_filled <- left_join(complete_data, df, by = c("season_starting", "site_name")) %>%
  mutate(presence = ifelse(is.na(penguin_count), NA, 1)) # Left join with df_ and create variable Presence with 0 (NAs) and 1 (count years)

df_filled <- df_filled %>%
  filter(presence == 1)

# Plot count years
df_list <- list(Adelie, Adelie10, Chinstrap, Chinstrap10, Gentoo, Gentoo10)
df_names <- c("Adelie", "Adelie", "Chinstrap", "Chinstrap", "Gentoo", "Gentoo")

for (i in seq_along(df_list)) {
  plot_title <- paste(df_names[i], "penguin count") 
  plot <- ggplot(df_list[[i]], aes(x = season_starting, y = reorder(site_name, latitude))) +
  geom_point() +
  labs(title = plot_title, x = "Year", y = "Colony") +
  theme_minimal()
  print(plot)
}

# Mapping colonies ----
library(mapdata)
library(rgdal)
library(ggmap)
library(raster)
library(mapproj)
library(sf)
library(ggplot2)
# s <- shapefile("~/Documents/doctorado/data exploration/add_coastline_medium_res_polygon_v7_4"/ "add_coastline_medium_res_line_v7_4.shp") # shapefile not loading

shp_file <- "/Volumes/Pengo2/Doctorado/data exploration/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp"
shp_file <- "/Users/albert.pg/Desktop/data/ATA_adm0.shp"

shp <- st_read(shp_file)
shp.df <- as_Spatial(shp)

shp1 <- subset(shp, long < -55) # remove parts of the shapefile outside the peninsula
shp1 <- subset(shp1, long > -70)

ggplot() +
  geom_sf(data = shp)

# plots 
m1<- ggplot() +  
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-68, -61), xlim = c(-67, -54)) +
  geom_point(data=col, aes(x=longitude_epsg_4326, y=latitude_epsg_4326), color="red",size=0.5, alpha=0.5) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 2 counts")
m1

d10 <- subset(d2, d2$n>10)
col10 <- distinct(d10,d10$site_id,.keep_all=TRUE)
m2<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-66, -61), xlim = c(-66, -55)) +
  geom_point(data=col10, aes(x=longitude_epsg_4326, y=latitude_epsg_4326), color="red", size=1, alpha= 0.5) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts")
m2

m3<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65, -61.8), xlim = c(-64, -55)) +
  geom_point(data=col, aes(x=longitude_epsg_4326, y=latitude_epsg_4326), color="red",size=0.5, alpha=0.5) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 2 counts")
m3

m4<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=col10, aes(x=longitude_epsg_4326, y=latitude_epsg_4326), color="red", size=1.5, alpha= 0.5) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts")
m4

    # Map colonies - dot size reflecting number of counts----
m1<- ggplot(data = col10, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = chinstrap), color = "green", alpha = 0.4) +
  scale_size_continuous(range = c(1, 10)) +
  geom_point(aes(color = no_count), size = 0.7, show.legend = FALSE) +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-66, -61.5), xlim = c(-67, -54))
m1

m1<- ggplot(data = col10, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = gentoo), color = "blue", alpha = 0.4) +
  scale_size_continuous(range = c(1, 10)) +
  geom_point(aes(color = no_count), size = 0.7, show.legend = FALSE) +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-68, -61), xlim = c(-67, -54))
m1

m1<- ggplot(data = col10, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = adelie), color = "green", alpha = 0.4) +
  scale_size_continuous(range = c(1, 10)) +
  geom_point(aes(color = no_count), size = 0.7, show.legend = FALSE) +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-68, -61), xlim = c(-67, -54))
m1

m1<- ggplot(data = col10, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = chinstrap), color = "red", alpha = 0.2) +
  geom_point(aes(size = gentoo), color = "blue", alpha = 0.2, show.legend = TRUE, fill = "transparent") +
  geom_point(aes(size = adelie), color = "green", alpha = 0.2, show.legend = TRUE) +
  scale_size_continuous(range = c(1, 10)) +
  geom_point(aes(color = no_count), size = 0.7, show.legend = FALSE) +
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-68, -61), xlim = c(-67, -54))
m1

ggplot() +
geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-68, -61), xlim = c(-67, -54))

  geom_point(aes(size = chinstrap), color = "red", alpha = 0.2) +
  geom_point(aes(size = gentoo), color = "blue", alpha = 0.2, show.legend = TRUE, fill = "transparent") +
  geom_point(aes(size = adelie), color = "green", alpha = 0.2, show.legend = TRUE) +
  scale_size_continuous(range = c(1, 10)) +
  geom_point(aes(color = no_count), size = 0.7, show.legend = FALSE)


    # Data on most visited locations ----

c <- read.csv("Most_Visited_Locations_ARLIEDATA.csv", sep = ";")
colnames(c) <- c("location","long","lat")
c <- subset(c, c$lat < -55)

m5 <- m4 + geom_point(data=c, aes(x=long, y=lat), color="black", size=1.5, shape=17) +
  theme(legend.title = element_text(colour="blue", size=16, face="bold"))

m5
dev.copy2pdf(file="m5.pdf", width = 6, height = 6)

# putting together colonies and visites sites for the map
c2 <- col10[,c(1,4,5)]
names(c2) <- c("location", "long", "lat")
c2$tag <- "Penguin colony"
c$tag <- "Visited site"
c3 <- rbind(c,c2)

m6<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.4) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=c3, aes(x=long, y=lat, shape=tag, colour=tag), size=0.5, alpha= 0.8) +

  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts") +
  theme(legend.title = element_blank())
m6
dev.copy2pdf(file="m3.pdf", width = 6, height = 6)

    # adding layer with antarctic stations ----

e <- read.csv("COMNAP_Antarctic_Facilities_Master.csv", sep = ",")
e <- subset(e[,c(2,4,11,12)])
names(e) <- c("location", "country", "lat", "long")
e1 <- subset(e, e$lat < -55)
e1 <- subset(e1, e$long < -55)
e1 <- subset(e1[,c(1,3,4)])
e1 <- e1[c("location", "long", "lat")]
e1$tag <- "Station"
c4 <- rbind(e1,c3)

#m7<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.4) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=c4, aes(x=long, y=lat, shape=tag, colour=tag), size=0.5, alpha= 0.8) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts") +
  theme(legend.title = element_blank()) +
  geom_label(data=c4 %>% filter(tag=="Station"), # Filter data first
    aes(x=long, y=lat, label=location),
    label.size = 0.1
  )
#m7
#dev.copy2pdf(file="m7.pdf", width = 6, height = 6)

m6<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.4) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=c4, aes(x=long, y=lat, shape=tag, colour=tag), size=0.5, alpha= 0.9) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts") +
  theme(legend.title = element_blank())
m6
dev.copy2pdf(file="m6.pdf", width = 6, height = 6)

    # Map for colonies with over 2 counts ----
f <- col[,c(1,4,5)]
names(f) <- c("location", "long", "lat")
f$tag <- "Penguin colony"
c$tag <- "Visited site"
f2 <- rbind(c, e1, f)

m8<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.4) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=f2, aes(x=long, y=lat, shape=tag, colour=tag), size=0.5, alpha= 0.8) +
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts") +
  theme(legend.title = element_blank())
m8
dev.copy2pdf(file="m8.pdf", width = 6, height = 6)

# Meteorology ----

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)
library(purrr)
library(humidity)

    # Escudero----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_escudero")

df <- read.csv("950001_XXXX_Temperatura_.csv", sep = ";",header = FALSE)
df = df[-1,]
colnames(df) <- c("station", "date", "ts")
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
temp <- df

df <- read.csv("950001_XXXX_TMinima_.csv", sep = ";",header = FALSE)
df = df[-1,]
colnames(df) <- c("station", "date", "tmin", "time")
df$date <- as.POSIXct(df$date, format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
df$time <- as.POSIXct(df$time, format= "%d-%m-%Y %H:%M", tz="UTC")
tmin <- df

df <- read.csv("950001_XXXX_TMaxima_.csv", sep = ";",header = FALSE)
df = df[-1,]
colnames(df) <- c("station", "date", "tmax", "time")
df$date <- as.POSIXct(df$date, format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
df$time <- as.POSIXct(df$time, format= "%d-%m-%Y %H:%M", tz="UTC")
tmax <- df

df <- read.csv("950001_XXXX_PuntoRocio_.csv", sep = ";",header = FALSE)
df = df[-1,-1]
colnames(df) <- c("date", "dew")
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
dew <- df

df <- read.csv("950001_XXXX_Humedad_.csv", sep = ";",header = FALSE)
df = df[,-1]
colnames(df) <- c( "date", "humidity")
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
hum <- df

df <- read.csv("950001_XXXX_Agua6Horas_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "prec", "trace")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
prec <- df

df <- read.csv("950001_XXXX_PresionQFF_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "pres")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
pres <- df

df <- read.csv("950001_XXXX_Viento_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "dir", "speed", "VRB_wind")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
wind <- df

df <- read.csv("950001_XXXX_DiarioRR_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date","Total_Valor","Traza_Valor","Parcial_Valor","NumDatos_Valor","FechaPro_Valor")
df = df[-1,]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
diarioRR <- df

merged <- merge(dew, hum, by="date", all.x = T)
merged <- merge(merged, prec, by="date", all.x = T)
merged <- merge(merged, pres, by="date", all.x = T)
merged <- merge(merged, temp, by="date", all.x = T)
merged <- merge(merged, wind, by="date", all.x = T)

escudero <- merged[merged$date > as.POSIXct("1979-12-31 21:00:00", tz="UTC"),]
write.csv(escudero, "escudero.csv", row.names = FALSE)
rm(hum,dew,pres,prec,temp,wind,tmax,tmin,diarioRR)

# fill escudero with hourly data ----

# Step 1: Determine min and max dates 
min_date <- as.POSIXct(min(escudero$date))
max_date <- as.POSIXct(max(escudero$date))

# Step 2: Create a vector of hourly dates
hourly_dates <- data.frame(date = seq(min_date, max_date, by = "hour"))

# Step 3: Fill missing hours by performing a full join
escudero_filled <- hourly_dates %>%
  full_join(escudero, by = "date") %>%
  arrange(date)  # Ensure data is sorted by date

# Step 4: Filter for specific times and select desired columns
times_to_keep <- c("00:00:00", "03:00:00", "06:00:00", "09:00:00", 
                   "12:00:00", "15:00:00", "18:00:00", "21:00:00")

escudero_3h <- escudero_filled %>%
  filter(format(as.POSIXct(date), "%H:%M:%S") %in% times_to_keep) %>%
  select(-station, -trace, -VRB_wind) %>%
  rename(
    temp = ts,
    hr = humidity,
    vel = speed
  )

# View the resulting data frame 
head(escudero_3h)

    # O'higgins ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_ohiggins")

df <- read.csv("950003_XXXX_Temperatura_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "temp")
df = df[-1,]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
temp <- df

df <- read.csv("950003_XXXX_PuntoRocio_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "dew")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
dew <- df

df <- read.csv("950003_XXXX_Humedad_.csv", sep = ";",header = FALSE)
colnames(df) <- c( "station", "date", "humidity")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
hum <- df

df <- read.csv("950003_XXXX_Agua6Horas_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "prec", "trace")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
prec <- df

df <- read.csv("950003_XXXX_PresionQFF_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "pres")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
pres <- df

df <- read.csv("950003_XXXX_Viento_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "dir", "speed", "VRB_wind")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
wind <- df

merged <- merge(dew, hum, by="date", all.x = T)
merged <- merge(merged, prec, by="date", all.x = T)
merged <- merge(merged, pres, by="date", all.x = T)
merged <- merge(merged, temp, by="date", all.x = T)
merged <- merge(merged, wind, by="date", all.x = T)

ohiggins <- merged[merged$date > as.POSIXct("1979-12-31 21:00:00", tz="UTC"),]
rm(df,merged,hum,dew,pres,prec,temp,wind,tmax,tmin,diarioRR)
write.csv(ohiggins, "ohiggins.csv", row.names = FALSE)

    # Prat -----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_prat")

df <- read.csv("950002_XXXX_Temperatura_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "temp")
df = df[-1,]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
temp <- df

df <- read.csv("950002_XXXX_PuntoRocio_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "dew")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
dew <- df

df <- read.csv("950002_XXXX_Humedad_.csv", sep = ";",header = FALSE)
colnames(df) <- c( "station", "date", "hr")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
hum <- df

df <- read.csv("950002_XXXX_Agua6Horas_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "prec", "trace")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
prec <- df

df <- read.csv("950002_XXXX_PresionQFF_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "pres")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
pres <- df

df <- read.csv("950002_XXXX_Viento_.csv", sep = ";",header = FALSE)
colnames(df) <- c("station", "date", "dir", "vel", "VRB_wind")
df = df[-1,-1]
df$date <- as.POSIXct(df$date,format= "%d-%m-%Y %H:%M:%OS", tz="UTC")
wind <- df

merged <- merge(dew, hum, by="date", all.x = T)
merged <- merge(merged, prec, by="date", all.x = T)
merged <- merge(merged, pres, by="date", all.x = T)
merged <- merge(merged, temp, by="date", all.x = T)
merged <- merge(merged, wind, by="date", all.x = T)

prat <- merged[merged$date > as.POSIXct("1979-12-31 21:00:00", tz="UTC"),]
rm(df,merged,hum,dew,pres,prec,temp,wind,tmax,tmin,diarioRR)

prat$date <- format(prat$date, "%Y-%m-%d %H:%M:%S")

write.csv(prat, "prat.csv", row.names = FALSE)

    # Dismal Island, Hugo Island, Racer Rock ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo")

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)

# Define the path to folder containing the .txt files
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_dismal"
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_hugo"
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_racer"
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_fossilbluff"

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "^1hr.*\\.txt$", full.names = TRUE)

# Initialize an empty list to store dataframes
df_list <- list()

process_df <- function(df) {
  # Column 1: year
  # Column 3: month
  # Column 4: day
  # Column 5: time
  
  df <- df %>%
    # Convert columns to character type if needed
    mutate(across(everything(), as.character)) %>%
    # Convert time to a formatted string
    mutate(
      time_formatted = str_pad(V5, width = 4, side = "left", pad = "0"), # Pad time to ensure 4 digits
      datetime = as.POSIXct(paste(V1, V3, V4, substr(time_formatted, 1, 2), substr(time_formatted, 3, 4), sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")
    ) %>%
    select(-V5) # Drop the original time column if no longer needed
  
  return(df)
}

# Loop through each file and process it
for (file in file_list) {
  df <- read.delim(file, sep = "", header = FALSE, skip = 2, dec = ".") # sep="" accounts for single and multiple spaces in .txt
  df <- process_df(df)   # Standardize column types
  df_list[[length(df_list) + 1]] <- df   # Add the dataframe to the list
}
  
# Combine all dataframes into one
combined_df <- bind_rows(df_list)

# Rename columns
combined_df <- combined_df %>%
  rename(
    year = V1,
    julianday = V2,
    month = V3,
    day = V4,
    temp = V6,
    pres = V7,
    vel = V8,
    ddd = V9,
    rh = V10,
    deltat = V11,
    date = datetime)

# Save the combined dataframe to a .csv file
write.csv(combined_df, "dismal.csv", row.names = FALSE)
write.csv(combined_df, "hugo.csv", row.names = FALSE)
write.csv(combined_df, "racer.csv", row.names = FALSE)

    # Fossil Bluff ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_fossilbluff")

df <- read.delim("fossil_bluff_synop.txt", skip=2, sep = "", header = FALSE, dec = ".")

process_df <- function(df) {
  df <- df %>%
    # Convert columns to character type if needed
    mutate(across(everything(), as.character)) %>%
    # Convert time to a formatted string
    mutate(
      datetime = as.POSIXct(paste(V1, V2, V3, V4, V5, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")
    ) %>%
    select(-c(1,2,3,4,5,12,13)) # Drop the original time column if no longer needed
  
  return(df)
}
add_leading_zero <- function(x) {
  formatted <- sub("^(\\-?)(\\.\\d+)$", "\\10\\2", x)
  return(formatted)
}

df <- process_df(df)   # Standardize column types

#  Filter df to keep observations from 1980 onward
df <- df %>%
  filter(datetime >= "1999-02-27 12:00:00") %>%
  filter(datetime < "2023-12-31 23:00:00")

# Rename columns 
df <- df %>%
  rename(
    dir = V6,
    vel = V7,
    pres = V8,
    temp = V9,
    d2m = V10,
    hr = V11,
    date = datetime)

df$temp <- sapply(df$temp, add_leading_zero) # Apply the function to format the values
df$vel <- sapply(df$vel, add_leading_zero) # Apply the function to format the values

# Convert the POSIXct column to character
df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")

# Save the combined dataframe to a .csv file
write.csv(df, "fossil.csv", row.names = FALSE)


    # PENDIENTE! Palmer ----

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)
library(data.table)

setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer")
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_palmer")

# Define the path to folder containing the .txt files
#folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_palmer"
folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer"

# Correct file for 17 oct 2022, removing a blank column
#df <- fread("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer/2022/October/AR221017_BASE.txt", fill = TRUE, na.strings = "NA", skip=1, header = FALSE)
#df <- df %>%
#  select(-2)
#write.table(df, "AR221017_BASE.txt", sep ="\t", row.names = FALSE)

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "_BASE.txt$", full.names = FALSE, recursive = TRUE) # recursive to read within subfolders too

# Initialize an empty list to store data frames
df_list <- list()

process_df <- function(df) {
  df <- df %>%
    select(c(1:14)) %>% 
    #select(-V1, -V2, -V3, -V11, -V12) %>%
    mutate(across(3:14, as.numeric)) %>% 
    mutate(across(2, as.character)) 
  return(df)
}

# Read files
for (file in file_list) {
  df <- fread(file, fill = TRUE, na.strings = "NA", skip=1, header = FALSE)
  df <- process_df(df)   # Standardize column types
  df_list[[length(df_list) + 1]] <- df   # Add the dataframe to the list
}

#Rename data frames as the csv files
names(df_list) <- sub("\\.txt$", "", basename(file_list))

# Process date column separately
df_list <- lapply(names(df_list), function(name) {
  df <- df_list[[name]]
  
  if (name %in% c("AR221018_BASE", "AR221017_BASE")) {
  df <- df %>% 
    mutate(V1 = as.POSIXct(V1, format = "%m/%d/%Y %H:%M", tz = "UTC"))
} else {
  df <- df %>% 
    mutate(V1 = as.POSIXct(V1, format = "%Y/%m/%d %H:%M:%S", tz = "UTC"))
}
return(df)
})

#Rename data frames as the csv files
names(df_list) <- sub("\\.txt$", "", basename(file_list))

# Combine all dataframes into one
df <- bind_rows(df_list)

# Remove unnecessary columns and rename the rest
df <- df %>%
  select(-V2,-V10,-V11) %>%
  rename(
    date = V1,
    vel = V3,
    dir = V4,
    gust_vel = V5,
    gust_dir = V6,
    temp = V7,
    hr = V8,
    dew = V9,
    pres = V12,
    snow_depth = V13,
    prec = V14)

# Check for NAs and shifted columns 
check <- df %>% filter(gust_vel == 360)
na_check <- sapply(df_list, function(df) any(is.na(df$date)))
na_dfs <- df_list[na_check] # Keep only the data frames that contain NA in V1

# Fill missing rows using pipe notation
merged_df <- df %>%
  # Create a complete sequence of datetimes
  summarize(start_time = min(date), end_time = max(date)) %>%
  { 
    complete_time <- seq(from = .$start_time, to = .$end_time, by = "min")
    
    # Create a complete data frame
    complete_df <- data.frame(date = complete_time)
    
    # Merge with the original data frame
    complete_df %>%
      left_join(df, by = "date")
  }

# Extract rows for each hour (minute 00, second 00)
hourly <- merged_df %>%
  filter(minute(date) == 0, second(date) == 0)

# Next step is replacing NAs with nearest values. na_counts to compare NAs across strategies
na_counts <- sapply(hourly, function(x) sum(is.na(x)))




# Apply the function to fill NA rows for all hours
filled_df <- fill_na_for_all_hours(merged_df)

# Function to fill NA rows for each hour in the data frame
fill_na_for_all_hours <- function(df) {
##### df instead of merged_df
  unique_hours <- unique(floor_date(merged_df$date, "hour"))  # Get unique hours

####### target_hour instead of 1  
  #for (target_hour in unique_hours) { 
    # Identify rows where all columns except 'date' are NA
###### replace hourly by df
    na_rows <- hourly %>%
##### uncomment next row
      # filter(date == target_hour) %>%
      filter(rowSums(is.na(select(., -date))) == ncol(select(., -date)))

    for (row in 1:nrow(na_rows)) {
      target_date <- na_rows$date[row]  # Get the target date from na_rows
      
      # Get indices in df where the date matches the target date
      idx <- which(df$date == target_date)
      
      for (i in 1:length(idx)) {  # Loop over valid indices
        # Ensure idx[i] is not NA (it should never be since we're filtering)
        start_idx <- max(1, idx[i] - 5)
        end_idx <- min(nrow(df), idx[i] + 5)
        
        # Extract the surrounding rows
        surrounding_rows <- df[start_idx:end_idx, ]
        
        # Check for non-NA values
        non_na_rows <- surrounding_rows %>%
          filter(rowSums(is.na(select(., -date))) < ncol(select(., -date)))  # Keep non-NA rows
        
        # If there are non-NA rows, replace the NA values
        if (nrow(non_na_rows) > 0) {
          # Calculate the time difference
          time_diff <- abs(difftime(non_na_rows$date, df$date[idx[i]], units = "secs"))
          
          # Find the index of the minimum time difference
          closest_index <- which.min(time_diff)
          
          # Replace the NA values with the closest non-NA row
          df[idx[i], -1] <- non_na_rows[closest_index, -1]  # Exclude the date column
        }
      }
    }


# Save the combined dataframe to a .csv file
write.csv(df, "palmer_3oct24.csv", row.names = FALSE)

      # PENDIENTE! Process months in 2019 with different structure ----
library(data.table)

setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/palmer_other_2019")

# Define the path to folder containing the .txt files
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/palmer_other_2019"

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "_BASE.txt$", full.names = FALSE, recursive = TRUE) # recursive to read within subfolders too

process_df <- function(df) {
  df <- df %>%
    mutate(V1 = as.POSIXct(paste(V1, V2, sep = " "), format = "%Y/%m/%d %H:%M:%S", tz = "UTC")) %>%
#    mutate(across(3:19, as.numeric)) %>% 
  return(df)
}

# Function to determine separator
detect_separator <- function(file) {
  first_line <- readLines(file, n = 2)[2]  # Read the first data line (skipping header line)
  if (grepl("\t", first_line)) {
    return("\t")  # Tab separator
  } else if (grepl("  ", first_line)) {
    return(" ")  # Double space separator
  } else {
    stop("Other separator in file", file)
  }
}

# Initialize an empty list to store data frames
df_list <- list()

for (file in file_list) {
  separator <- detect_separator(file)  # Detect separator
  df <- fread(file, fill = TRUE, na.strings = "NA", skip=1, header = FALSE)
  df <- process_df(df)
  df_list[[length(df_list) + 1]] <- df # Add the dataframe to the list
}

#Rename data frames as the txt files
names(df_list) <- sub("\\.txt$", "", basename(file_list))

# Loop through each dataframe and remove specified columns based on column count
df_list <- lapply(df_list, function(df) {
  num_cols <- ncol(df)
  if (num_cols == 19) {
    df <- df[, -c(2, 16:19)]     # Remove column 2 and columns 16 to 19
  } else if (num_cols == 20) {
    df <- df[, -c(15:20)]     # Remove columns 15 to 20
  }
  return(df)
})

combined_df <- bind_rows(df_list)
names(combined_df) <- c("date",	"time", "ID", "vel", "dir",	"gust_vel",	"gust_dir",	"temp",	"hr",	"dew",	"pyranometer", "pres", "snow_depth", "prec",	"visibility",	"CBase1", "CBase2", "CBase3",	"vert_vis")

# Combine all dataframes into one
df <- bind_rows(df_list)

# Check NAs and shifted columns 
check <- df %>% filter(V4 == 360)
na_check <- sapply(df_list, function(df) any(is.na(df$V1)))
na_dfs <- df_list[na_check] # Keep only the data frames that contain NA in V1


    # Palmer_clean_2019 ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer")

palmer2019 <- read.csv("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer/palmer_clean_2019.csv", sep = ",", header = TRUE)
palmer2019 <- palmer2019 %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  mutate(across(2:11, as.numeric))

palmer <- read.csv("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer/palmer_3oct24.csv")
palmer <- palmer %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  mutate(across(2:11, as.numeric))

names(palmer2019) <- names(palmer)
palmer_complete <- rbind(palmer,palmer2019)

write.csv(palmer_complete,"palmer_28dec24.csv", row.names = FALSE)

    # Palmer 2001-2015 ----
folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer"

file_list <- list.files(path = folder_path, pattern = "\\.100$", recursive = TRUE, full.names = TRUE)

# Read each .100 file into a data frame and store in a list
df_list <- lapply(file_list, function(file) {
  read.delim(file, skip = 1, header = TRUE)
})

new_list <- lapply(df_list, function(df) {
  df <- df %>%
    mutate(date = as.POSIXct(paste(Date, Time, sep = " "), format = "%y/%m/%d %H:%M:%S", tz = "UTC")) %>%   # Convert date to a formatted string
    select(date, everything())  %>%    # Reorder columns, date first
    select(c(1,4,5,6,8,10,12,13,14,17,18,19)) %>%   # Drop the original time columns and unnecessary columns
    mutate(across(4:11, as.numeric))    # Change class of numeric variables
  return(df)
})

# Standardize column names across all data frames in new_list
col_names <- c("date", "Date", "Time", "vel", "dir", "gust_vel",	"temp",	"hr",	"dew", "pres", "snow_depth", "prec")

new_list <- lapply(new_list, function(df) {
  # Ensure the number of columns matches
  if(ncol(df) == length(col_names)) {
    colnames(df) <- col_names
  } else {
    warning("Skipping a dataframe with different column count")     # Handle the case where the number of columns is different (if needed)
  }
  return(df)
})

combined_df <- Reduce(rbind, new_list)

# Remove rows where the date is NA
combined_df <- combined_df %>% 
  filter(!is.na(date)) %>%
  mutate(date = floor_date(date, unit = "minute")) %>% # Set seconds to 00
  mutate(gust_dir = NA) %>%
  select(c(-Date,-Time))

# Join data frames from period 2001-15 and 2015-24
palmer <- read.csv("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer/palmer_28dec24.csv", sep = ",", header = TRUE)
palmer <- palmer %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
  mutate(across(2:11, as.numeric))

# Remove rows from combined_df where the date is in palmer
filtered_combined_df <- combined_df %>%
  filter(!date %in% palmer$date)

# Bind the two data frames
palmer_complete <- bind_rows(filtered_combined_df, palmer_complete)
palmer_complete  <- palmer_complete2
write.csv(palmer_complete,"/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer/palmer_9jan25.csv", row.names = FALSE)

# Remove anomalous observations

    # Vernadsky ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_vernadsky")

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)
library(humidity)

folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_vernadsky"

df <- read.delim("faraday.txt", sep = "\t", header = TRUE, dec = ".")

df <- df %>%
  rename(
    year = Obtime
  )

process_df <- function(df) {
  df <- df %>%
    # Convert columns to character
    mutate(across(everything(), as.character)) %>%
    # Convert time to a formatted string
    mutate(
      hour_formatted = str_pad(hour, width = 2, side = "left", pad = "0"), # Pad time to ensure 2 digits
      min_formatted = str_pad(min, width = 2, side = "left", pad = "0"), # Pad time to ensure 2 digits
      datetime = as.POSIXct(paste(year, month, day, hour_formatted, min_formatted, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")
    ) %>%
    #select(-hour,-min) # Drop the original time column if no longer needed
  
  return(df)
}

df <- process_df(df)

df1 <- df %>% # filter df to keep observations from 1980 onwards
  filter(datetime >= "1979-12-31 21:00:00"
)

df2 <- read.delim("vernadsky_bas.txt", sep = "\t", header = TRUE,skip = 1, dec = ".")

# Rename columns
df2 <- df2 %>%
  rename(
    year = Obtime,
    month = X,
    day = X.1,
    hour = X.2,
    min = X.3
    )

df2 <- process_df(df2)

df2 <- df2 %>%
  mutate(
    DRYBULB_TEMPERATURE = as.numeric(DRYBULB_TEMPERATURE),
    DEWPOINT = as.numeric(DEWPOINT),
    RH = RH(DRYBULB_TEMPERATURE, DEWPOINT, isK = FALSE)
  )

    # Ferraz ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_ferraz")

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)
library(tidyr)
library(purrr)

# Define the path to folder containing the .txt files
folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_ferraz"

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "^ferraz_.*\\.txt$", full.names = TRUE)

# Initialize an empty list to store dataframes
df_list <- list()

process_df <- function(df, file_name) {
  df <- df %>%
    
      select(-c(12, 23)) %>% # Drop columns by index (e.g., 12 and 23)
      rename(date = V1, # Rename columns
             "00:00" = V2,
             "01:00" = V3,
             "02:00" = V4,
             "03:00" = V5,
             "04:00" = V6,
             "05:00" = V7,
             "06:00" = V8,
             "07:00" = V9,
             "08:00" = V10,
             "09:00" = V11,
             "10:00" = V13,
             "11:00" = V14,
             "12:00" = V15,
             "13:00" = V16,
             "14:00" = V17,
             "15:00" = V18,
             "16:00" = V19,
             "17:00" = V20,
             "18:00" = V21,
             "19:00" = V22,
             "20:00" = V24,
             "21:00" = V25,
             "22:00" = V26,
             "23:00" = V27) %>% 
      pivot_longer(cols = -date, names_to = "hour", values_to = "observation") %>% # Pivot all columns except the Date column
      mutate(datetime = as.POSIXct(paste(date, hour, sep = "-"), format = "%d-%m-%Y-%H:%M", tz = "UTC")) %>% 
      select(-date, -hour) %>% 
      rename(!!file_name := observation)
    
  return(df)
}

# Loop through each file and process it
for (file in file_list) {
  # Extract the base name of the file (without the path, extension and prefix)
  file_name <- tools::file_path_sans_ext(basename(file))
  file_name <- sub("^ferraz_", "", file_name)
  
  df <- read.delim(file, sep = "", header = FALSE, skip = 1, dec = ",") # sep="" accounts for single and multiple spaces in .txt
  df <- process_df(df, file_name)   # Pass the file name to process_df
  df_list[[length(df_list) + 1]] <- df   # Add the dataframe to the list
}

# Combine all data frames into a single data frame if needed
combined_df <- reduce(df_list, full_join, by = "datetime")
combined_df <- combined_df %>%
  select(datetime, everything())

# Replace commas with periods in all character columns
combined_df[] <- lapply(combined_df, function(x) {
  if (is.character(x)) {
    gsub(",", ".", x)
  } else {
    x
  }
})

combined_df <- combined_df %>%
  rename(
    dew = dewpoint,
    hr = hum,
    pres = press,
    dir = winddir,
    vel = windspeed)
# Save the combined dataframe to a .csv file
write.csv(combined_df, "ferraz.csv", row.names = FALSE)



    # Vernadsky and Faraday ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_vernadsky")

process_df <- function(df) {
  df <- df %>%
    # Convert columns to character type if needed
    mutate(across(everything(), as.character)) %>%
    
    # Convert time to a formatted string
    mutate(datetime = as.POSIXct(paste(V1, V2, V3, V4, V5, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    
    # Drop the original time columns
    select(-V1, -V2, -V3, -V4, -V5) %>%
    
    # Reorder columns, date first
    select(datetime, everything())
  
  return(df)
}

# Function to add leading zero when necessary
add_leading_zero <- function(x) {
  formatted <- sub("^(\\-?)(\\.\\d+)$", "\\10\\2", x)
  return(formatted)
}

# Read data from Vernadsky prior to 1995
df <- read.delim("faraday.txt", sep = "", header = FALSE, skip = 1, dec = ".")

df <- process_df(df)
df <- df %>%
  rename( 
    date = datetime,
    vel = V6,
    dir = V7,
    present_weather = V8,
    past_weather = V9,
    pres = V10,
    temp = V11,
    rh = V12,
    d2m = V13,
    new_temp = V14) 

# Split data at 1st Jan 1986 (3h data before, 1h data afterwards)
faraday_1h <- df %>%
  filter(date >= "1985-12-31 21:00:00")
# Convert the POSIXct column to character
faraday_1h$date <- format(faraday_1h$date, "%Y-%m-%d %H:%M:%S")

faraday_3h <- df %>%
  filter(date < "1985-12-31 21:00:00")
faraday_3h$date <- format(faraday_3h$date, "%Y-%m-%d %H:%M:%S")

write.csv(faraday_1h, file = "faraday_1h.csv", row.names = FALSE)
write.csv(faraday_3h, file = "faraday_3h.csv", row.names = FALSE)

# Data at Vernadsky from BAS, 1996 to 2024
df <- read.delim("vernadsky.txt", sep = "", header = FALSE, skip = 1, dec = ".")
df <- process_df(df)
df <- df %>%
  rename(
    date = datetime,
    dir = V6,
    vel = V7,
    present_weather = V8,
    past_weather = V9,
    pres = V10,
    temp = V11,
    d2m = V12)

df$temp <- sapply(df$temp, add_leading_zero) # Apply function to add leading zeros
df$vel <- sapply(df$vel, add_leading_zero)

# Exclude data in the specified period, as it contains 3h data instead of 1h.
df <- df %>%
  filter(!(date > as.POSIXct("2014-02-28 23:00:00") & 
             date <= as.POSIXct("2018-08-31 23:00:00")))

df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")

write.csv(df, file = "vernadsky.csv", row.names = FALSE)

# Read data from Vernadsky from Ukraine. Read data after 2011 separately and remove an excess column
df <- read.delim("vernadsky_ukr.txt", sep = "\t", header = TRUE, dec = ".") 
df <- df %>%
  filter(yyyy == 2011.0)
df <- df %>%
  select(-X)

df1 <- read.delim("vernadsky_ukr.txt", sep = "\t", skip = 2861, header = FALSE, dec = ".", row.names = NULL) 
df1 <- df1 %>%
  select(-V5)

# Rename df1 using column names from df
names(df1) <- names(df)

# Bind both df 
df <- rbind(df, df1)

# Replace -9999 with NA in all columns
df <- df %>%
  mutate(across(everything(), ~ na_if(., -9999)))

process_df <- function(df) {
  df <- df %>%
    # Convert time to a formatted string
    mutate(
      minute = "00",
      datetime = as.POSIXct(paste(yyyy, mm, dd, hh, minute, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    # Drop the original time columns
    select(-yyyy, -mm, -dd, -hh, -minute) %>%
    select(datetime, everything())
  
  return(df)
}

df <- process_df(df)

df <- df %>%
  select(-h, -VV, -N, -StLP, -TendCode, -TendSize, -Nh, -Cl, -Cm, -Ch, -MaxT, -MinT) %>%
  rename(
    date = datetime,
    dir = d,
    vel = ff,
    temp = T,
    d2m = Td,
    pressure = SLP,
    precip = Precip,
    present_weather = ww,
    past_weather = W1W2,
    terrain = Terr,
    rh = RH.)

df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")

write.csv(df, file = "vernadsky_ukr.csv", row.names = FALSE)

    # Carlini, Esperanza, San Martin ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo")
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_argentina"
file_list <- list.files(path = folder_path, pattern = "*.txt$", full.names = TRUE)

process_df <- function(df) {
 df <- df %>%
    # Convert columns to character type if needed
    mutate(across(everything(), as.character)) %>%
    # Convert time to a formatted string
    mutate(
      hora = str_pad(hora, width = 2, side = "left", pad = "0"), # Pad time to ensure 4 digits
      minuto = "00",
      datetime = as.POSIXct(paste(fecha, hora, minuto, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    select(-fecha, -hora, -minuto) %>% # Drop the original time column if no longer needed
    select(datetime, everything())
  return(df)
}

# Loop through each file and process it
for (file in file_list) {
  file_name <- tools::file_path_sans_ext(basename(file))   # Extract the base name of the file (without the path and extension)
  df <- read.delim(file, sep = "", header = TRUE, skip = 2, dec = ",")
  df <- process_df(df)
  csv_file_name <- paste0(file_name, ".csv")
  write.csv(df, file = csv_file_name, row.names = FALSE)
  
  # Print a message to confirm the file was saved
  cat("Exported", csv_file_name, "\n")
}

    # Rothera ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_rothera")

process_df <- function(df) {
  df <- df %>%
    # Convert time to a formatted string
    mutate(date = as.POSIXct(paste(year, month, day, hour, min, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    # Drop the original time columns and unnecessary columns
    select(-c(1:5)) %>% #,9,14,15,17:20,22:34)) %>%
    # Reorder columns, date first
    select(date, everything())  %>% 
    # Change class of numeric variables
    mutate(across(2:8, as.numeric))
  return(df)
}

# Read and process data from Rothera
df <- read.delim("rothera2025.txt", sep = "", header = TRUE, skip = 0, dec = ".")
df <- process_df(df)
df <- df %>%
  rename( 
    dir = V7,
    vel = V8,
    pres_weather = V10,
    past_weather = V11,
    pres = V12,
    temp = V13,
    rh =  V16,
    dew = V21)

# Save the combined dataframe to a .csv file
df$date <- format(df$date, "%Y-%m-%d %H:%M:%S")

write.csv(df, "rothera2025.csv", row.names = FALSE)

    # Kirkwood ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/")

process_df <- function(df) {
  df <- df %>%
    mutate(min_gmt = 00) %>%
    mutate(date = as.POSIXct(paste(year, month_gmt, day_gmt, hour_gmt, min_gmt, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%   # Convert date to a formatted string
    select(-c(1:4,6,9,10,13:18)) %>%   # Drop the original time columns and unnecessary columns
    select(date, everything())  %>%    # Reorder columns, date first
    mutate(across(2:5, as.numeric))    # Change class of numeric variables
  return(df)
}

df <- read.csv("meteo_kirkwood/awsK.csv", sep = ",", header = TRUE)
df <- process_df(df)

# Transform wind direction to reference North and range from 0 to 360
df <- df %>%
  mutate(
    dir = case_when(
      wind_speed == 0 & wind_dir_e == 0 ~ 0, # If speed and direction are 0, keep as 0. It is likely a measurement error (e.g. frozen anemometer)
      wind_dir_e > 0 ~ (90 - wind_dir_e) + 360,  # Positive: Counterclockwise from East
      wind_dir_e <= 0 ~ (90 - wind_dir_e)  # Negative: Clockwise from East
    ) %% 360  # Ensure range is 0-360
  )

df <- df %>%
  rename(
    pres = press_bar,
    vel = wind_speed,
    temp = temp_air,
    hr = humidity ) %>%
  select(-wind_dir_e) %>%
  mutate(date = format(date, "%Y-%m-%d %H:%M:%S"))

df$date <- as.character(df$date)

# Save the dataframe to a .csv file
write.csv(df, "kirkwood.csv", row.names = FALSE)

# Plot meteo stations and nearest grid points ----
setwd("/media/ddonoso/KINGSTON")
library(mapdata)
library(rgdal)
library(ggmap)
library(raster)
library(mapproj)
library(sf)
library(ggplot2)
library(ggrepel)
# Load the raster file
tif_file <- "/home/ddonoso/Desktop/datos_Albert/output_GEBCOIceTopo.tif"
r <- raster(tif_file)

# Convert to a data frame for ggplot
r_df <- as.data.frame(r, xy = TRUE)
# Rename the raster value column (optional)
names(r_df)[3] <- "value"  # Assuming a single-band raster

coastline <- st_read("/home/ddonoso/Desktop/datos_Albert/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp")
#coastline <- st_read("/media/ddonoso/Pengo2/Doctorado/ATA_adm0/ATA_adm0.shp")
coastline <- st_transform(coastline, crs = 4326)
bbox <- st_bbox(c(xmin = -70, ymin = -73, xmax = -50, ymax = -61), crs = 4326)
bbox_sf <- st_as_sfc(bbox)  # Convert bbox to spatial object
coastline_crop <- st_crop(coastline, bbox_sf)

#shp_file <- "/home/ddonoso/Desktop/datos_Albert/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp"
#shp_file <- "/Volumes/Pengo2/Doctorado/ATA_adm0/ATA_adm0.shp"
shp <- st_read(coastline_crop)
shp.df <- as_Spatial(coastline_crop)

# Read weather stations and grid points
stations <- read.csv("all_coords_long.csv", sep = ",", header = TRUE)
stations_filtered <- stations %>%
  dplyr::filter(dataset == "station")
stations_filtered1 <- stations_filtered %>%
  dplyr::filter(lat > -64)

stations$station_type <- recode(stations$dataset, 
                                     station = "Weather Stations", 
                                     era5land = "ERA5 Land", 
                                     era5 = "ERA5", 
                                     racmo5.5 = "RACMO 5.5km", 
                                     racmo11 = "RACMO 11km")
# Plot
m1 <- ggplot() +  
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = c("#36648B","#4F94CD", "#AFEEEE","#8B7E66", "#2F4F4F","white", "black"), 
                       values = scales::rescale(c(-5000, -200, 0, 1, 500, 1000, 2500)),
                       guide = "none") +
 
  #coord_cartesian(ylim = c(-72, -62), xlim = c(-70, -55)) +
  coord_sf(crs = st_crs(r)) + 
  geom_point(data = stations, aes(x = long, y = lat, color = station_type), size = 0.5) +
  geom_text_repel(data= stations_filtered, aes(x = long, y = lat, label = station), box.padding = 0.5, point.padding = 0.3) +
  xlim(-76,-54) +
  ylim(-72,-61) +
  scale_color_manual(name = "", 
                     values = c("Weather Stations" = "black", 
                                "ERA5 Land" = "tomato", 
                                "ERA5" = "darkolivegreen3", 
                                "RACMO 5.5km" = "plum3", 
                                "RACMO 11km" = "gold"),
                     guide = guide_legend(order = 1)) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_line(colour = scales::alpha("white", 0.2)),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE) +
  labs(x = "", y = "")

m1

ggsave("peninsula.png", plot = m1, width = 8, height = 8, dpi = 300)

m2 <- ggplot() +  
  geom_raster(data = r_df, aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(colors = c("#36648B","#4F94CD", "#AFEEEE","#8B7E66", "#2F4F4F","white", "black"), 
                       values = scales::rescale(c(-5000, -331, -330, 0, 100, 500, 2500)),
                       guide = "none") +
  geom_sf(data = coastline_crop, color = "black", fill = "grey", size = 0.05 ,alpha = 0) +
  #coord_cartesian(ylim = c(-72, -62), xlim = c(-70, -55)) +
  coord_sf(crs = st_crs(r), xlim = c(-62,-56.5), ylim = c(-63.6,-61.8)) + 
  geom_point(data = stations, aes(x = long, y = lat, color = station_type), size = 1) +
  geom_text_repel(data= stations_filtered1, aes(x = long, y = lat, label = station), box.padding = 1, point.padding = 0.3) +
  scale_color_manual(name = "", 
                     values = c("Weather Stations" = "black", 
                                "ERA5 Land" = "tomato", 
                                "ERA5" = "darkolivegreen3", 
                                "RACMO 5.5km" = "plum3", 
                                "RACMO 11km" = "gold"),
                     guide = guide_legend(order = 1)) +
  theme(panel.background = element_blank(), 
        panel.grid.major = element_line(colour = scales::alpha("white", 0.2)),
        panel.grid.minor = element_blank(),
        panel.ontop = TRUE) +
  labs(x = "", y = "")

m2

ggsave("shetlands.png", plot = m2, width = 8, height = 8, dpi = 300)

m2<- ggplot() +  
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-63.5, -61.6), xlim = c(-62, -56)) +
  geom_point(data=stations, aes(x=target_long, y=target_lat), color="black",size=0.5) +
  geom_point(data = stations, aes(x = nearest_lon, y = nearest_lat), color = "red", size = 0.5, alpha = 0.5) +
  geom_text(data = stations, aes(x = target_long, y = target_lat, label = location), 
            vjust = -1, color = "black", size = 3) + 
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Weather stations (black) and ERA5-Land grid points (red)")
m2
ggsave("ws_shetlands.png", plot = m2, width = 8, height = 8, dpi = 300)

library(geosphere)

# Calculate distances using distHaversine for all points
coords1 <- as.matrix(stations[, c(3, 2)])  # Extract station coordinates from df stations (longitude, latitude)
coords2 <- as.matrix(stations[, c(5, 4)])  # Extract reanalysis coordinates from df stations (longitude, latitude)
distances <- distHaversine(coords1, coords2)  # Distance in meters
stations$distance <- distances / 1000 # Convert distances to kilometers and add as a new column


# Process meteo time series (remove extra columns and outliers) ----

setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/flags_3h_tolerance15")

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too
data_frames <- lapply(file_list, read.csv)
names(data_frames) <- sub("\\.csv$", "", basename(file_list))
remove_cols <- c("skt", "prec", "dew", "velracha", "dirracha", "gustvel", "gustdir", "value", "trace")

data_frames <- lapply(data_frames, function(df) {
  df <- df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    select(-matches(paste(remove_cols, collapse = "|"))) # Remove columns that match any pattern in remove_cols, to include flag columns
  return(df)
})

data_frames <- lapply(data_frames, function(df) {
 if ("ts" %in% names(df)) {
    df <- df %>% rename(temp = ts,
                        temp_escudero = ts_escudero)
   }
 if ("speed" %in% names(df)) {
    df <- df %>% rename(vel = speed,
                        vel_escudero = speed_escudero)
   }
  return(df)
})

# Reorder columns based on the order specified
data_frames <- lapply(data_frames, function(df) {
  col_order <- c("date", "temp", "pres", "hr", "vel", "dir")
  df <- df[, c(intersect(col_order, names(df)), setdiff(names(df), col_order))]
  return(df)
})

# Transform necessary variables, remove wrong values

df_names <- c("sanmartin", "esperanza", "carlini") # Wind speed and direction from Argentinian stations
data_frames[df_names] <- lapply(data_frames[df_names], function(df) {
  df$dir <- df$dir * 10   # transform wind direction deca to degrees
  df$vel <- df$vel / 3.6  # transform wind speed from km/h to m/s
  return(df)
})

data_frames <- lapply(data_frames, function(df) { # Remove wind directions > 360
  df <- df %>%
    mutate(dir = ifelse(dir > 360, NA, dir))
  return(df)
})

data_frames <- lapply(data_frames, function(df) { # Remove pres < 900
  df <- df %>%
    mutate(pres = ifelse(pres < 900, NA, pres))
  return(df)
})

df_names <- c("escudero", "ohiggins", "prat", "rothera") # Transform wind speed from knots to m/s
data_frames[df_names] <- lapply(data_frames[df_names], function(df) {
  df$vel <- df$vel  * 0.514444
  return(df)
})

data_frames <- lapply(data_frames, function(df) { # Remove Relative Humidity > 100
  if ("hr" %in% names(df)) {
    df <- df %>%
      mutate(hr = ifelse(hr > 100, NA, hr))
  }
  return(df)
})

# Check temperature  and hr failure in Palmer
df <- data_frames$palmer
subset_df <- df[df$date >= as.Date("2010-05-30") & df$date <= as.Date("2010-10-15"), ]
plot(subset_df$date, subset_df$temp) 

data_frames$palmer <- data_frames$palmer %>%
  mutate(temp = ifelse(date >= as.Date("2010-05-30") & date <= as.Date("2010-10-15"), NA, temp))

df <- data_frames$palmer
subset_df <- df[df$date >= as.Date("2010-09-07") & df$date <= as.Date("2010-10-13"), ]
plot(subset_df$date, subset_df$hr) 

data_frames$palmer <- data_frames$palmer %>%
  mutate(hr = ifelse(date >= as.Date("2010-09-07") & date <= as.Date("2010-10-13"), NA, hr))

df <- data_frames$palmer
subset_df <- df[df$date >= as.Date("2024-06-11") & df$date <= as.Date("2024-07-01"), ]
plot(subset_df$date, subset_df$hr) 

data_frames$palmer <- data_frames$palmer %>%
  mutate(hr = ifelse(date >= as.Date("2024-06-11") & date <= as.Date("2024-07-01"), NA, hr))

# Export modified files
output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/flags_edit"

for (name in names(data_frames)) {
  data_frames[[name]]$date <- format(data_frames[[name]]$date, "%Y-%m-%d %H:%M:%S")
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(data_frames[[name]], file = file_path, row.names = FALSE)
}

# Identify study periods with less than 15 days of NAs per year ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/flagged_jan")

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too
data_frames <- lapply(file_list, read.csv)
names(data_frames) <- sub("\\_flagged.csv$", "", basename(file_list))

data_frames <- lapply(data_frames, function(df) {
  names(df) <- sub(".*\\.", "", names(df)) # Rename columns to remove everything before the dot
  return(df)
})

remove_cols <- c("skt", "prec", "dew", "velracha", "dirracha", "gustvel", "gustdir", "value", "trace")
data_frames <- lapply(data_frames, function(df) {
  df <- df %>%
    mutate(date = as.POSIXct(date, tz="UTC")) %>%
    select(-matches(paste(remove_cols, collapse = "|"))) # Remove columns that match any pattern in remove_cols, to include flag columns
  return(df)
})


# Extract flag columns and date
flags <- lapply(data_frames, function(df) {
  cols_to_extract <- c("date", grep("_", names(df), value = TRUE))   # Identify columns that contain "_" or are named "date"
  df_subset <- df[, cols_to_extract, drop = FALSE]   # Subset the data frame to include only these columns
  df_subset <- df_subset %>%
    relocate(date, .before = everything())  # Move 'date' column to the first position
  names(df_subset) <- gsub("_.*$", "", names(df_subset))   # Remove suffix from headers
  return(df_subset)
})

    # Line plots of flags ----
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# Function to create flag plot
plot_flags <- function(df_long, plot_title) {
  ggplot(df_long, aes(x = date, y = variable, colour = factor(value))) +
    geom_point() +
    labs(title = plot_title, x = "", y = "") +
    theme_minimal() +
    scale_color_manual(values = c("0" = "black", "1" = "darkolivegreen4")) +
    scale_x_datetime(
      #breaks = "1 year",  # Set breaks at 1-year intervals
      labels = date_format("%Y")) + # Show only the year in labels
    guides(colour = guide_legend(title = "Valid periods (green)"))
}

# Create a vector of names
df_names <- c("Carlini", "Dismal Island", "Escudero", "Esperanza", "Fossil Bluff", "Gabriel  de Castilla", "Hugo Island", "Juan Carlos I", "King Sejong", "Kirkwood Island", "O'Higgins", "Palmer", "Prat", "Racer Rock", "Rothera", "San Martin", "Vernadsky")

# Loop through data frame list 'flags' using their position
for (i in seq_along(flags)) {
  # Get the current data frame
  df_long <- flags[[i]]  %>%
    pivot_longer(cols = -date, names_to = "variable", values_to = "value")
  plot_title <- df_names[i]
  plot <- plot_flags(df_long, plot_title)
  png(filename = paste0(plot_title, ".png"), width = 3000, height = 1000, res = 300)
  print(plot)
  dev.off()
}

names(plots) <- names(flags)

# Display all plots
for (plot in plots) {
  print(plot)
}

# Save each plot as a separate file
lapply(plots, function(x) {
  ggsave(
    filename = paste0(x, "_valid_periods.png"),
    plot = plots[[x]],
    width = 10,
    height = 10,
    dpi = 300
  )
})

    # Plot time series with shaded areas ----
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)


df_names <- c("Carlini", "Dismal Island", "Escudero", "Esperanza", "Fossil Bluff", "Gabriel  de Castilla", "Hugo Island", "Juan Carlos I", "King Sejong", "Kirkwood Island", "O'Higgins", "Palmer", "Prat", "Racer Rock", "Rothera", "San Martin", "Vernadsky")

# Iterate through each df in data_frames list
for (i in seq_along(data_frames)) {
  df <- data_frames[[i]]
  df_name <- names(data_frames)[i]
  df_title <- df_names[i]
  
  # Identify valid columns (exclude 'date' and columns with '_')
  valid_columns <- names(df)[!grepl("_", names(df)) & names(df) != "date"]
  
  # List to store plots for the current data frame
  plot_list <- list()
  
  # Iterate through valid columns
  for (j in seq_along(valid_columns)) {
    col_name <- valid_columns[j]
    col_flag <- grep(paste0("^", col_name, "_"), names(df), value = TRUE)
    
    # Create shading ranges based on the flag column
    shading_ranges <- df %>%
      mutate(is_shaded = !!sym(col_flag) == 1) %>%
      group_by(group = cumsum(!is_shaded)) %>%
      dplyr::filter(is_shaded) %>%
      summarize(
        xmin = first(date),
        xmax = last(date),
        .groups = "drop"
      )
    
    # Calculate mean and sd and add them to time series plot
    #col_mean <- mean(df[[col_name]], na.rm = TRUE)
    #col_sd <- sd(df[[col_name]], na.rm = TRUE)
    
    df_filtered <- df[!is.na(df[[col_name]]), ]
    
    # Plot raw meteo data with valid periods as shaded areas
    plot <- ggplot(df_filtered) +
      geom_rect(data = shading_ranges, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "darkolivegreen3", alpha = 0.5) +
      geom_line(aes(x = date, y = .data[[col_name]]), color = "black", linewidth = 0.5) +
      labs(title = "", x = "", y = col_name) +
      theme_minimal()
    
    # Store the plot in the list
    plot_list[[j]] <- plot
  }
  
  # Export plots for the current data frame
  combined_plot <- grid.arrange(
    grobs = c(
      list(textGrob(
        paste0(df_title, " time series"),
        gp = gpar(fontsize = 12, fontface = "bold"),
        just = "center")), plot_list),
    ncol = 1,
    heights = c(1, rep(10, length(plot_list))) # Adjust heights: 1 for title, 10 for each plot
  )
  
  png(filename = paste0(df_title, "_timeseries.png"), width = 3000, height = 3000, res = 300)
  grid.draw(combined_plot) 
  dev.off()  
  
}

    # Identify study periods ----

# Create a list of data frames
# Process files 15 NA days
{ohiggins15 <- ohiggins_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_ohiggins)
carlini15 <- carlini_flagged %>%
  select(-c(2:8,))%>%
  select(-prec_carlini)
esperanza15 <- esperanza_flagged %>%
  select(-c(2:6,)) %>%
  select(-prec_esperanza)
jci15 <- jci_flagged %>%
  select(-c(2:8,)) %>%
  select(-prec_jci) %>%
  select(-skt_jci)
palmer15 <- palmer_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_palmer)
prat15 <- prat_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_prat)
rothera15 <- rothera_flagged %>%
  select(-c(2:6,))
sanmartin15 <- sanmartin_flagged %>%
  select(-c(2:6,)) %>%
  select(-prec_sanmartin)
vernadsky15 <- vernadsky_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_vernadsky)
}
# Process files 30 NA days 
{ohiggins30 <- ohiggins_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_ohiggins)
carlini30 <- carlini_flagged %>%
  select(-c(2:8,))%>%
  select(-prec_carlini)
esperanza30 <- esperanza_flagged %>%
  select(-c(2:6,)) %>%
  select(-prec_esperanza)
jci30 <- jci_flagged %>%
  select(-c(2:8,)) %>%
  select(-prec_jci) %>%
  select(-skt_jci)
palmer30 <- palmer_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_palmer)
prat30 <- prat_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_prat)
rothera30 <- rothera_flagged %>%
  select(-c(2:6,))
sanmartin30 <- sanmartin_flagged %>%
  select(-c(2:6,)) %>%
  select(-prec_sanmartin)
vernadsky30 <- vernadsky_flagged %>%
  select(-c(2:7,)) %>%
  select(-prec_vernadsky)
}

# df_list15 <- list(ohiggins15, carlini15, esperanza15, jci15, prat15, rothera15, sanmartin15, vernadsky15)
# df_list30 <- list(ohiggins30, carlini30, esperanza30, jci30, prat30, rothera30, sanmartin30, vernadsky30)

# Remove stations with incomplete records
remove <- c("dismal", "kirkwood", "racer", "hugo", "gdc")
flags <- flags[!names(flags) %in% remove]

# Combine all data frames by date and filter
df <- flags %>%
  Reduce(function(x, y) full_join(x, y, by = "date"), .) %>%
  dplyr::filter(if_all(-date, ~ . == 1))

# Identify the full sequence of dates without gaps
full_dates <- seq(min(df$date), max(df$date), by = "3 hours")

# Identify missing dates
missing_dates <- setdiff(full_dates, df$date)

# Create a column to identify groups of continuous data
periods <- df %>%
  arrange(date) %>%
  mutate(gap = c(0, diff(as.numeric(date)) != 3 * 3600),  # Identify gaps of 3 hours
         group = cumsum(gap)) %>%
  group_by(group) %>%
  dplyr::filter(all(!date %in% missing_dates)) %>%  # Keep groups without missing dates
  summarise(start = min(date), end = max(date), .groups = 'drop')

periods <- periods %>%
  mutate(ymin = 0, 
         ymax = 1)

plot <- ggplot(periods) +
  geom_segment(
    aes(x = start, xend = end, y = 0.5, yend = 0.5), # Horizontal line at y = 0.5 
    color = "darkolivegreen3", linewidth = 10) +
  labs(
    title = "Validation Periods", x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  scale_y_continuous(expand = c(0, 0)) # Keep y-axis minimal

png(filename = "valid_periods.png", width = 2000, height = 500, res = 300)
grid.draw(plot) 
dev.off()  

    # Extract study period from weather stations ----

data_frames <- lapply(data_frames, function(df) { # Remove flag columns
  df <- df %>%
    dplyr::select(-dplyr::contains("_")) 
})

# Remove stations with incomplete records
remove <- c("dismal", "kirkwood", "racer", "hugo", "gdc")
data_frames <- data_frames[!names(data_frames) %in% remove]


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


{ohiggins <- ohiggins_flagged %>%
  select(-c(8:13,)) %>%
  select(-prec)
carlini <- carlini_flagged %>%
  select(-c(7:11,)) %>%
  select(-prec)
esperanza <- esperanza_flagged %>%
  select(-c(7:11,)) %>%
  select(-prec)
jci <- jci_flagged %>%
  select(-c(9:15,)) %>%
  select(-prec) %>%
  select(-skt)
palmer <- palmer_flagged %>%
  select(-c(8:13,)) %>%
  select(-prec)
prat <- prat_flagged %>%
  select(-c(8:13,)) %>%
  select(-prec)
rothera <- rothera_flagged %>%
  select(-c(7:11,))
sanmartin <- sanmartin_flagged %>%
  select(-c(7:11,)) %>%
  select(-prec)
vernadsky <- vernadsky_flagged %>%
  select(-c(8:13,)) %>%
  select(-prec)
}
stations <- list(carlini, esperanza, jci, ohiggins, prat, rothera, sanmartin, vernadsky)

# Rewrite Rothera with new data  ----
process_df <- function(df) {
  df <- df %>%
    # Convert time to a formatted string
    mutate(date = as.POSIXct(paste(year, month, day, hour, min, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    # Reorder columns, date first
    select(date, everything())  %>% 
    # Change ckass of numeric variables
    mutate(across(6:11, as.numeric)) %>%
    mutate(vel = vel * 0.514444) %>% # Transform wind speed from knots to m/s
    select(-c(2:6))
  return(df)
}
df1 <- read.delim("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_rothera/rothera_validation.txt", sep = "", header = TRUE, dec = ".")
df1 <- process_df(df1)
stations[[6]] <- merge(df1, stations[[6]], by = "date", all.y = TRUE, suffixes = c("_df1", ""))
stations[[6]] <- stations[[6]][, !names(stations[[6]]) %in% c("dir", "vel", "temp", "pres", "hr", "dew")]
names(stations[[6]]) <- sub("_df1$", "", names(stations[[6]]))

# Remove wind directions > 360 in station datasets
stations <- lapply(stations, function(df) {
  df %>%
    mutate(dir = ifelse(dir>360, NA, dir))
  return(df)
})

# Study period is 2012-01-01 00:00:00 to 2014-01-01 00:00:00 
stations <- lapply(stations, function(df) {
  df %>%
    filter(date >= as.POSIXct("2012-01-01 00:00:00") & date < as.POSIXct("2014-01-01 00:00:00"))
})

names(stations) <- c("carlini", "esperanza", "jci", "ohiggins", "prat", "rothera", "sanmartin", "vernadsky")

# Transform wind speed and direction of the Argentinian stations
df_names <- c("sanmartin", "esperanza", "carlini")
stations[df_names] <- lapply(stations[df_names], function(df) {
    df$dir <- df$dir * 10   # transform wind direction deca to degrees
    df$vel <- df$vel / 3.6  # transform wind speed from km/h to m/s
      return(df)
})
  
# Transform u and v to wind speed and direction for each data frame
era5land_list_wind <- lapply(era5land_list_celsius, function(df) {
    df %>%
      mutate(
        vel10 = sqrt(u10^2 + v10^2),  # Calculate wind speed
        dir = (atan2(v10, u10) * (180 / pi)) %% 360  # Calculate wind direction in degrees
      )
  })

# Transform wind speed from knots to m/s
df_names <- c("ohiggins", "prat")
stations[df_names] <- lapply(stations[df_names], function(df) {
    df$vel <- df$vel  * 0.514444  # transform wind speed from knots to m/s
    return(df)
  })
  
  # Remove odd values in Relative Humidity
stations <- lapply(stations, function(df) {
   if ("hr" %in% names(df)) {  # Check if 'hr' column exists
    df <- df %>%
      mutate(hr = ifelse(hr > 100, NA, hr))  # Replace values greater than 100 with NA
  }
  return(df)
  })
  
# Save each dataframe in the list as CSV
output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/3hourly_stations"
  
for (name in names(stations)) {
  file_path <- file.path(output_directory, paste0(name, "_station.csv"))
  write.csv(stations[[name]], file = file_path, row.names = FALSE)
}
  
    # Process ERA5-Land ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land")
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too

era5land_list <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame

new_column_names <- c("u10", "v10", "dew", "temp", "skt", "snowc", "sde", "sf", "pres", "prec", "date")
era5land_list <- lapply(era5land_list, function(df) setNames(df, new_column_names))

names(era5land_list) <- sub("\\.csv$", "", basename(file_list)) # Name each data frame based on the file name

era5land_list <- lapply(era5land_list, function(df) {
  df <- df %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric
  return(df)
})

# Convert Julian hours in the date column for each data frame
era5land_list <- lapply(era5land_list, function(df) {
  df %>%
    mutate(date = as_datetime(date * 3600, origin = as.POSIXct("1900-01-01 00:00:00", tz = "UTC"), tz = "UTC"))
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

    # Process ERA5 ----

setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5")
setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/era5")

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = FALSE) # recursive to read within subfolders too

era5 <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame

col_names <- c("u10", "v10", "dew", "temp", "pres", "prec", "gust", "snowfall", "date")

era5 <- map(era5, ~ setNames(.x, col_names)) # Rename columns in each data frame

names(era5) <- gsub("^datos|\\.csv$", "", basename(file_list)) # Name each data frame based on the file name

era5 <- lapply(era5, function(df) {
  df <- df %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric
  return(df)
})

# Convert Julian hours in the date column for each data frame
era5_date <- lapply(era5, function(df) {
  df %>%
    mutate(date = as_datetime(date, origin = as.POSIXct("1970-01-01 00:00:00", tz = "UTC"), tz = "UTC")) %>%
    select(date, everything())
})

# Convert columns 3, 4, and 5 from Kelvin to Celsius for each data frame
era5_celsius <- lapply(era5_date, function(df) {
  df %>%
    mutate(across(4:5, ~ . - 273.15))  # Convert columns 3, 4, and 5 from Kelvin to Celsius
})

# Transform u and v to wind speed and direction for each data frame
era5_wind <- lapply(era5_celsius, function(df) {
  df %>%
    mutate(
      vel10 = sqrt(u10^2 + v10^2),  # Calculate wind speed
      dir = (atan2(v10, u10) * (180 / pi)) %% 360  # Calculate wind direction in degrees
    )
})

# Calculate wind speed at 3m based on a logarithmic attenuation
era5_vel3 <- lapply(era5_wind, function(df) {
  
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
file_path= "/Volumes/Pengo2/Doctorado/datos_netcdf_rema/era5land/station_locations.csv"
df <- read.csv(file_path, sep = ",", header = TRUE)
df$elev_station <- c(8, 45, 10, 5, 12, 70, 15, 10, 25, 17, 10, 25, 7, 12, 5, 93, 4, 63, 30) # Elevation of weather stations
df$elev_diff <- df$elev - df$elev_station
df$temp_diff <- (df$elev_diff) / 1000 * 6.5
temp_diff <- df$temp_diff[c(6,3,14,2,9,1,18,7,12,16,5,19,8,11,4,10,17,15,13)]
elev_diff <- df$elev_diff[c(6,3,14,2,9,1,18,7,12,16,5,19,8,11,4,10,17,15,13)]
coords <- df

# Correct temperature in era5land data
era5_temp <- lapply(seq_along(era5_vel3), function(i) {
  era5_vel3[[i]] %>%
    rename(
      old_temp = temp,
      old_dew = dew) %>%
    mutate(
      temp = old_temp + temp_diff[i],
      dew = old_dew + temp_diff[i])
})

# Calculate relative humidity
era5_hr <- lapply(era5_temp, function(df) {
  df %>%
    mutate(hr = RH(old_temp, old_dew, isK = FALSE))
})

# Adjust pressure for each dataframe in the list
era5_list <- lapply(seq_along(era5_hr), function(i) {
  P0 <- 101325  # Pa
  g <- 9.81     # m/s²
  M <- 0.029    # kg/mol
  R <- 8.314    # J/(mol·K)
  T <- era5_hr[[i]]$old_temp + 273.15 # degrees Kelvin
  delta_h <- elev_diff[i]
  delta_P <- P0 * (g * M / (R * T)) * delta_h  # Calculate delta_P for each station's elevation difference 
  
  era5_hr[[i]] %>%
    rename(old_pres = pres) %>% 
    mutate(pres = as.numeric(old_pres) + delta_P) %>% 
    mutate(pres = pres / 100)  # Convert resulting pressure back to hPa
})

# Rename and save each dataframe in the list as CSV
  #names(era5) <- gsub("^datos|\\.csv$", "", basename(file_list))
output_directory <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/era5/processed"
for (name in names(era5_vel3)) {
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(era5_vel3[[name]], file = file_path, row.names = FALSE)
}

    # Extract study period from ERA5Land, extract obs. every 3 hours ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/processed_era5land")
file_list <- list.files(pattern = "\\_era5land.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

era5land <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame
names(era5land) <- gsub("\\_era5land.csv$", "", basename(file_list))

era5land <- lapply(era5land, function(df) {
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

    # Combine era5land and stations with matching names ----
# read era5land with modified wind dir and magnitude
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land")

file_list <- list.files(pattern = "\\mag.csv$", full.names = TRUE, recursive = TRUE)

era5land <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame

names(era5land) <- c("carlini", "esperanza", "jci", "ohiggins", "prat", "rothera", "sanmartin", "vernadsky")

# read processed station datasets
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/3hourly_stations")

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

stations <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame

names(stations) <- gsub("\\_station.csv$", "", basename(file_list))

# Combine station and era5land files
combined_list <- lapply(names(era5land), function(name) {
  df1 <- era5land[[name]]
  df2 <- stations[[name]]
  
  # Merge the data frames by 'date'   
  combined_df <- merge(df1, df2, by = "date", suffixes = c("_era5", "_station"))
  
  # Filter selected columns to only include those that exist in combined_df
  columns_to_select <- c("date", 
                         "temp_station", "vel_station", "dir_station", "pres_station", "hr_station", "wind_magnitude_station", "wind_direction_station", 
                         "temp_era5", "vel_era5", "dir_era5", "pres_era5", "hr_era5",
                         "temp_df1", "vel_df1", "dir_df1", "pres_df1", "hr_df1")
  
  columns_to_select <- columns_to_select[columns_to_select %in% names(combined_df)]
  
  # Select the specified columns from the combined data frame
  combined_df <- combined_df[, columns_to_select, drop = FALSE]
  
  return(combined_df)
})

# Assign names to the combined data frames and extract them to the global environment
names(combined_list) <- names(era5land)

combined_list <- lapply(combined_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC"))
})

# Calculate wind speed at 3m based on a logarithmic attenuation
combined_list <- lapply(combined_list, function(df) {
  
  # Vector to store the value at x = 3 for each row
  df$vel10 <- df$vel_era5
  
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
    df$vel_era5[i] <- log_function(3)
  }
  return(df)
})

list2env(setNames(combined_list, paste0("combined_", names(combined_list))), envir = .GlobalEnv)

names(combined_list) <- c("Carlini", "Esperanza", "Juan Carlos I", "O'Higgins", "Prat", "Rothera", "San Martin", "Vernadsky")

output_directory <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/combined_data"

for (name in names(combined_list)) {
  file_path <- file.path(output_directory, paste0(name, ".csv"))
  write.csv(combined_list[[name]], file = file_path, row.names = FALSE)
}

# Plots and analyses ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/combined_data")

file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)

combined_list <- lapply(file_list, read.csv) # Read each CSV file into a separate data frame

names(combined_list) <- gsub("\\.csv$", "", basename(file_list))

names(combined_list) <- c("carlini", "esperanza", "jci", "ohiggins", "prat", "rothera", "sanmartin", "vernadsky")

combined_list <- lapply(combined_list, function(df) {
  df %>%
    mutate(date = as.POSIXct(date, tz="UTC"))
})

list2env(setNames(combined_list, paste0("combined_", names(combined_list))), envir = .GlobalEnv)

names(combined_list) <- c("Carlini", "Esperanza", "Juan Carlos I", "O'Higgins", "Prat", "Rothera", "San Martin", "Vernadsky")

    # Time series plots ----
# Meteo analyses and plots ----

library(ggplot2)
library(gridExtra)
detach(stats, unload = TRUE)
library(tidyverse)


setwd("/Users/albert/Desktop/doctorado_definitivo/combined_data")
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

combined_list <- lapply(file_list, function(file) {
  df <- read.csv(file)
  df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")  # Adjust format as needed
  return(df)
})

names(combined_list) <- gsub("^\\./|\\.csv$", "", basename(file_list))

combined_list <- lapply(combined_list, function(df) {
  df %>%
    mutate(year = year(date)) %>% # Extract the year from the date column
    filter(year != 2014) # Keep entire years, drop obs from 2014
})

    # Calculate statistics ----
        # Mean and SD table ----

calculate_stats <- function(df, df_name) {
  
  cols_to_analyze <- list(
    c("temp_station", "temp_era5"),
    c("vel_station", "vel_era5"),
    c("pres_station", "pres_era5")
  )
  
  # Initialize a data frame to store results
  stats_df <- data.frame(df_name = df_name, stringsAsFactors = FALSE)
  
  # Loop through the columns and calculate stats
  for (cols in cols_to_analyze) {
    col1 <- cols[1]
    col2 <- cols[2]
    
    # Calculate mean and sd, excluding NAs
    stats <- df %>%
      select(all_of(c(col1, col2))) %>%
      na.omit() %>%
      summarise(
        !!paste0(col1, "_mean") := round(mean(get(col1)), 2),  # Round to 2 decimal places
        !!paste0(col1, "_sd") := round(sd(get(col1)), 2),      # Round to 2 decimal places
        !!paste0(col2, "_mean") := round(mean(get(col2)), 2),  # Round to 2 decimal places
        !!paste0(col2, "_sd") := round(sd(get(col2)), 2)       # Round to 2 decimal places
      )
    
    # Bind the stats to the results data frame
    stats_df <- bind_cols(stats_df, stats)
  }
  
  # Calculate stats for "hr_era5" and "hr_station" if they exist
  if ("hr_station" %in% names(df) && "hr_era5" %in% names(df)) {
    hr_stats <- df %>%
      select(hr_station, hr_era5) %>%
      na.omit() %>%
      summarise(
        hr_station_mean = round(mean(hr_station), 2),  # Round to 2 decimal places
        hr_station_sd = round(sd(hr_station), 2),      # Round to 2 decimal places
        hr_era5_mean = round(mean(hr_era5), 2),        # Round to 2 decimal places
        hr_era5_sd = round(sd(hr_era5), 2)              # Round to 2 decimal places
      )
    
    stats_df <- bind_cols(stats_df, hr_stats)
  } else {
    # If columns are missing, add NA values
    stats_df <- bind_cols(stats_df,
                          data.frame(hr_station_mean = NA, hr_station_sd = NA, hr_era5_mean = NA, hr_era5_sd = NA))
  }
  
  return(stats_df)
}

# Calculate statistics for each data frame in combined_list
results_list <- map(names(combined_list), ~ calculate_stats(combined_list[[.x]], .x))

# Combine all results into a single data frame
mean_sd_table <- bind_rows(results_list)

write.csv(mean_sd_table, "mean_sd.csv", row.names = FALSE)

        # Normalized Bias ----

# Define column pairs for calculation
cols_to_analyze <- list(
  c("temp_station", "temp_era5"),
  c("pres_station", "pres_era5"),
  c("vel_station", "vel_era5"),
  c("hr_station", "hr_era5")
)

# Function to calculate normalized bias without using summarise
nbias_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df) && "year" %in% names(df)) {
      # Calculate mean of col1 for each year and join with original df
      df <- df %>%
        group_by(year) %>%
        mutate(mean_col1_year = mean(!!sym(col1), na.rm = TRUE)) %>%
        ungroup()
      
      # Filter out rows with NA in either col1 or col2
      valid_rows <- df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2)))
      
      # Calculate normalized bias
      normalized_bias <- sum((valid_rows[[col2]] - valid_rows[[col1]]) / valid_rows$mean_col1_year, na.rm = TRUE) / nrow(valid_rows)
      
      # Return results as a data frame
      data.frame(normalized_bias = round(normalized_bias, 3), 
                 variable = paste(col1, "vs", col2))
    } else {
      data.frame(normalized_bias = NA, variable = paste(col1, "vs", col2))  # Return NA if columns are missing
    }
  })
}

# Apply the function to each data frame in combined_list
nbias_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  nbias_results <- nbias_function(df, cols_to_analyze)
  nbias_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format
nbias_results <- nbias_results_list %>%
  pivot_wider(
    names_from = variable,
    values_from = normalized_bias
  )

write.csv(nbias_results, "nbias_results.csv", row.names = FALSE)

        # Seasonal bias ----
# temperature bias
cols_to_analyze <- list(c("temp_station", "temp_era5"))
cols_to_analyze <- list(c("vel_station", "vel_era5"))
cols_to_analyze <- list(c("pres_station", "pres_era5"))
cols_to_analyze <- list(c("hr_station", "hr_era5"))


# Function to calculate bias grouped by season
bias_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df) && "season" %in% names(df)) {
      
      # Filter out rows with NA in either col1 or col2
      valid_rows <- df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2)))
      
      # Calculate bias grouped by season
      bias_results <- valid_rows %>%
        group_by(season) %>%
        summarise(
          bias = round(mean((!!sym(col2) - !!sym(col1)), na.rm = TRUE), 2) # round to 2 decimal places
        )
      return(bias_results)
      
    } else {
      # Return NA if columns are missing or no season column
      data.frame(season = NA, bias = NA, variable = paste(col1, "vs", col2))
    }
  })
}

# Apply the function to each data frame in combined_list
bias_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  bias_results <- bias_function(df, cols_to_analyze)
  bias_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format, with one row per station and columns for each season and variable
bias_results <- bias_results_list %>%
  pivot_wider(
    names_from = season,
    values_from = bias
  )

# Save the resulting bias_results DataFrame as a CSV file
write.csv(bias_results, "bias_results.csv", row.names = FALSE)

# plot heat map with ggplot

# Transform data into a long format for ggplot, reorder season levels, and station levels
long_bias_results <- bias_results %>%
  pivot_longer(
    cols = -station,  # Convert all columns except 'station'
    names_to = "season",
    values_to = "bias"
  ) %>%
  mutate(
    season = factor(season, levels = c("DJF", "MAM", "JJA", "SON")),  # Reorder seasons
    station = factor(station, levels = c("Prat", "Carlini", "Juan Carlos I", 
                                         "O'Higgins", "Esperanza", "Vernadsky", 
                                         "Rothera", "San Martin"))  # Reorder stations
  )

# Create the heatmap plot
heatmap_plot <- ggplot(long_bias_results, aes(x = season, y = station, fill = bias)) +
  geom_tile() +  # Use white borders for clarity
  geom_text(aes(label = sprintf("%.2f", bias)), color = "black", size = 4) +  # Display values with 2 decimal places
  scale_fill_gradient2(low = "darkblue", mid = "white", high = "tomato", 
                       midpoint = 0,  # Set fixed limits
                       name = "Bias (ºC)") +
  scale_x_discrete(position = "top") +  # Position season labels on top
  scale_y_discrete(limits = rev(levels(long_bias_results$station))) +  # Reverse the order of stations
  theme_minimal() +
  labs(title = "Temperature Bias by Season",
       x = "", y = "") +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 12),  # Adjust font size for x-axis
    axis.text.y = element_text(angle = 0, hjust = 1, size = 12),  # Adjust font size for y-axis
    plot.title = element_text(size = 14, hjust = 0.5),  # Center and enlarge the title
    legend.text = element_text(size = 10),  # Adjust legend font size
    legend.title = element_text(size = 10)  # Adjust legend title font size
  )

ggsave("temp_bias_heatmap.png", plot = heatmap_plot, width = 5, height = 4, dpi = 300)


          # Normalized Mean Absolute Error ----

# Function to calculate normalized Mean Absolute Error (MAE)
nmae_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df) && "year" %in% names(df)) {
      # Calculate mean of col1 for each year and join with original df
      df <- df %>%
        group_by(year) %>%
        mutate(mean_col1_year = mean(!!sym(col1), na.rm = TRUE)) %>%
        ungroup()
      
      # Filter out rows with NA in either col1 or col2
      valid_rows <- df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2)))
      
      # Calculate normalized MAE
      normalized_mae <- sum(abs((valid_rows[[col2]] - valid_rows[[col1]]) / valid_rows$mean_col1_year))/ nrow(valid_rows)
      
      # Return results as a data frame
      data.frame(normalized_mae = round(normalized_mae, 3),
                 variable = paste(col1, "vs", col2))
    } else {
      data.frame(normalized_mae = NA, variable = paste(col1, "vs", col2))  # Return NA if columns are missing
    }
  })
}

# Apply the function to each data frame in combined_list
nmae_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  nmae_results <- nmae_function(df, cols_to_analyze)
  nmae_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format
nmae_results <- nmae_results_list %>%
  pivot_wider(
    names_from = variable,
    values_from = normalized_mae
  )

write.csv(nmae_results, "nmae_results.csv", row.names = FALSE)

        # use NMAE function from package DTWBI
library(DTWBI)

# Function to calculate normalized mae every pair of variables
nmae_dtwbi_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df)) {
      df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2))) %>%
        summarise(normalized_mae = compute.nmae((!!sym(col2)), (!!sym(col1))) ) %>% # Calculate normalized bias
        mutate(variable = paste(col1, "vs", col2))  # Add a label for the variable pair
    } else {
      data.frame(normalized_mae = NA, variable = paste(col1, "vs", col2))  # Return NA if columns are missing
    }
  })
}

# Calculate normalized biases for each data frame in combined_list
nmae_dtwbi_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  nmae_results <- nmae_dtwbi_function(df, cols_to_analyze)
  nmae_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format
nmae_dtwbi_results <- nmae_dtwbi_results_list %>%
  pivot_wider(
    names_from = variable,
    values_from = normalized_mae
  )

        # Normalized RMSE ----

# Function to calculate normalized Root Mean Squared Error (RMSE)
nrmse_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df) && "year" %in% names(df)) {
      # Calculate mean of col1 for each year and join with original df
      df <- df %>%
        group_by(year) %>%
        mutate(mean_col1_year = mean(!!sym(col1), na.rm = TRUE)) %>%
        ungroup()
      
      # Filter out rows with NA in either col1 or col2
      valid_rows <- df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2)))
      
      # Calculate normalized RMSE
      normalized_rmse <- sqrt(sum(((valid_rows[[col2]] - valid_rows[[col1]]) / valid_rows$mean_col1_year)^2) / nrow(valid_rows))
      
      # Return results as a data frame
      data.frame(normalized_rmse = round(normalized_rmse, 3),
                 variable = paste(col1, "vs", col2))
    } else {
      data.frame(normalized_rmse = NA, variable = paste(col1, "vs", col2))  # Return NA if columns are missing
    }
  })
}

# Apply the function to each data frame in combined_list
nrmse_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  nrmse_results <- nrmse_function(df, cols_to_analyze)
  nrmse_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format
nrmse_results <- nrmse_results_list %>%
  pivot_wider(
    names_from = variable,
    values_from = normalized_rmse
  )

write.csv(nrmse_results, "nrmse_results.csv", row.names = FALSE)

        # Correlation

library(dplyr)
library(purrr)
library(tidyr)

# Function to calculate Pearson correlation and p-value
correlation_function <- function(df, cols) {
  map_dfr(cols, function(pair) {
    col1 <- pair[1]
    col2 <- pair[2]
    
    if (col1 %in% names(df) && col2 %in% names(df)) {
      # Filter out rows with NA in either col1 or col2
      valid_rows <- df %>%
        filter(!is.na(!!sym(col1)), !is.na(!!sym(col2)))
      
      # Calculate Pearson correlation and p-value
      correlation_test <- cor.test(valid_rows[[col1]], valid_rows[[col2]], method = "pearson", conf.level = 0.95)
      correlation_value <- correlation_test$estimate
      p_value <- correlation_test$p.value
      
      # Return results as a data frame
      data.frame(
        correlation = round(correlation_test$estimate, 2),
        p_value = p_value,
        variable = paste(col1, "vs", col2)
      )
    } else {
      data.frame(correlation = NA, p_value = NA, variable = paste(col1, "vs", col2))  # Return NA if columns are missing
    }
  })
}

# Apply the function to each data frame in combined_list
correlation_results_list <- map_dfr(names(combined_list), function(name) {
  df <- combined_list[[name]]
  correlation_results <- correlation_function(df, cols_to_analyze)
  correlation_results %>%
    mutate(station = name)  # Add station name to results
})

# Reshape the results into a wide format
correlation_results <- correlation_results_list %>%
  pivot_wider(
    names_from = variable,
    values_from = c(correlation, p_value)
  )

# Save the resulting wide_correlation_results DataFrame as a CSV file
write.csv(correlation_results, "correlation_results.csv", row.names = FALSE)

    # Time series plots ----

# Function to plot time series for two variables
plot_time_series_pair <- function(df, var1, var2, ylab) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = !!sym(var1), color = var1, group = 1), size = 0.3, alpha = 0.7) +
    geom_line(aes(y = !!sym(var2), color = var2, group = 2), size = 0.3, alpha = 0.7) +
    labs(x = NULL, y = ylab) +
    scale_color_manual(values = c("tomato", "black"), labels = c("Era5-Land","Station")) +
    theme_minimal() +
    #theme(legend.title = element_blank()) 
    theme(legend.position = "none")
}


for (name in names(combined_list)) {
  df <- combined_list[[name]]
  
  plots <- list() # Create a list to store the plots
  
  # Check if columns exist and create plots
  if ("temp_station" %in% names(df) && "temp_era5" %in% names(df)) {
    plots$temp <- plot_time_series_pair(df, "temp_station", "temp_era5", "2 m Temperature (°C)")
  }
  
  if ("vel_station" %in% names(df) && "vel_era5" %in% names(df)) {
    plots$vel <- plot_time_series_pair(df, "vel_station", "vel_era5", "Wind speed (m/s)")
  }
  
  if ("pres_station" %in% names(df) && "pres_era5" %in% names(df)) {
    plots$pres <- plot_time_series_pair(df, "pres_station", "pres_era5", "Pressure (hPa)")
  }
  
  if ("hr_station" %in% names(df) && "hr_era5" %in% names(df)) {
    plots$hr <- plot_time_series_pair(df, "hr_station", "hr_era5", "Relative humidity (%)")
  }
  
  plot <- grid.arrange(grobs = plots, ncol = 1)#, top = paste(name, "Base"))
  
#  folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
#  ggsave(filename = paste0(folder_path, "/", name, "_timeseries.png"), plot = plot, width = 7, height = 7, dpi = 300)
  ggsave(filename = paste0(name, "_timeseries.png"), plot = plot, width = 7, height = 7, dpi = 300, bg = "transparent")
}

    # Taylor diagrams ----
library(plotrix)
combined_list <- combined_list[c("Prat", "Carlini", "Juan Carlos I", "O'Higgins", "Esperanza", "Vernadsky", "Rothera", "San Martin")]

png("taylor_temperature.png", width = 1500, height = 1500, res = 300, bg = "transparent")

taylor.diagram(combined_list[[4]]$temp_station, combined_list[[4]]$temp_era5, col = "yellow", sd.arcs = TRUE,pch=19, pcex = 1.8, main = "")

# Define colors corresponding to each dataset
colors <- c("red", "blue", "pink", "yellow", "green", "purple", "orange", "lightblue")

# Loop through each dataset in combined_list
for (i in seq_along(combined_list)) {
  df <- combined_list[[i]]
  color <- colors[i]
  
  taylor.diagram(df$temp_station, df$temp_era5, add = TRUE, col = color, pch=19, pcex = 1.8)
  
  points(sd(df$temp_station, na.rm = TRUE), 0, col = color, pch = 19, cex = 1.5) # Reference sd for each station

}

# Add a legend for the datasets using their names
legend_labels <- names(combined_list)  # Use the names of the data frames directly
legend(x = 7.5, y = 9.5,
       legend = legend_labels,
       pch = 19, col = colors[seq_along(combined_list)], bty = "n", pt.cex = 1.5)

title(main = "2 m Temperature", cex.main=2)

dev.off()


# Pressure diagram
{ 
png("taylor_pressure.png", width = 1500, height = 1500, res = 300, bg = "transparent")

taylor.diagram(combined_list[[1]]$pres_station, combined_list[[1]]$pres_era5, col = "red",pch=1, sd.arcs = TRUE, pcex = 1.8, main = "")

# Define colors corresponding to each dataset
colors <- c("red", "blue", "pink", "yellow", "green", "purple", "orange", "lightblue")

# Loop through each dataset in combined_list
for (i in seq_along(combined_list)) {
  df <- combined_list[[i]]
  color <- colors[i]
  
  taylor.diagram(df$pres_station, df$pres_era5, add = TRUE, col = color, pch = 1, pcex = 1.8)
  
  points(sd(df$pres_station, na.rm = TRUE), 0, col = color, pch = 19, cex = 1.5) # Reference sd for each station
  
}

# Add a legend for the datasets using their names
legend_labels <- names(combined_list)  # Use the names of the data frames directly
legend(x = 17, y = 22,
       legend = legend_labels,
       pch = 19, col = colors[seq_along(combined_list)], bty = "n", pt.cex = 1.5)

title(main = "Surface Pressure", cex.main = 2)

dev.off()

}


# Wind speed diagram
{ 
  png("taylor_vel.png", width = 1500, height = 1500, res = 300, bg = "transparent")
  
  taylor.diagram(combined_list[[1]]$vel_station, combined_list[[1]]$vel_era5, col = "red",pch=19, sd.arcs = TRUE, pcex = 1.8, main = "")
  
  # Define colors corresponding to each dataset
  colors <- c("red", "blue", "pink", "yellow", "green", "purple", "orange", "lightblue")
  
  # Loop through each dataset in combined_list
  for (i in seq_along(combined_list)) {
    df <- combined_list[[i]]
    color <- colors[i]
    
    taylor.diagram(df$vel_station, df$vel_era5, add = TRUE, col = color, pch = 19, pcex = 1.8)
    
    points(sd(df$vel_station, na.rm = TRUE), 0, col = color, pch = 19, cex = 1.5) # Reference sd for each station
    
  }
  
  # Add a legend for the datasets using their names
  legend_labels <- names(combined_list)  # Use the names of the data frames directly
  legend(x = 7.5, y = 10,
         legend = legend_labels,
         pch = 19, col = colors[seq_along(combined_list)], bty = "n", pt.cex = 1.5)
  
  title(main = "Wind Speed", cex.main = 2)
  
  dev.off()
  
}

# Relative humidity diagram
{ 
  png("taylor_humidity.png", width = 1500, height = 1500, res = 300, bg = "transparent")
  
  taylor.diagram(combined_list[[1]]$hr_station, combined_list[[1]]$hr_era5, col = "red",pch=19, sd.arcs = TRUE, pcex = 1.8, main = "")
  
  # Define colors corresponding to each dataset
  colors <- c("red", "pink", "yellow", "purple", "orange")
  
  # Loop through each dataset in combined_list
  for (i in seq_along(combined_list)) {
    df <- combined_list[[i]]
    color <- colors[i]
    
    taylor.diagram(df$hr_station, df$hr_era5, add = TRUE, col = color, pch = 19, pcex = 1.8)
    
    points(sd(df$hr_station, na.rm = TRUE), 0, col = color, pch = 19, cex = 1.5) # Reference sd for each station
    
  }
  
  # Add a legend for the datasets using their names
  legend_labels <- names(combined_list)  # Use the names of the data frames directly
  legend(x = 7.5, y = 10,
         legend = legend_labels,
         pch = 19, col = colors[seq_along(combined_list)], bty = "n", pt.cex = 1.5)
  
  title(main = "Relative Humidity", cex.main = 2)
  
  dev.off()
  
}



# Define the specific datasets to include
selected_datasets <- c("Prat", "Juan Carlos I", "O'Higgins", "Vernadsky", "Rothera")

# Create a PNG for the Taylor Diagram
png("taylor_humidity.png", width = 1500, height = 1500, res = 300, bg = "transparent")

# Initialize the Taylor diagram with the first selected dataset
taylor.diagram(combined_list[[selected_datasets[1]]]$hr_station, 
               combined_list[[selected_datasets[1]]]$hr_era5, 
               col = "red", pch = 19, sd.arcs = TRUE, pcex = 1.8, main = "")

# Define colors corresponding to each dataset
colors <- c("red", "pink", "yellow", "purple", "orange")

# Loop through each selected dataset in combined_list
for (i in seq_along(selected_datasets)) {
  dataset_name <- selected_datasets[i]
  df <- combined_list[[dataset_name]]
  color <- colors[i]
  
  # Plot the data on the Taylor diagram
  taylor.diagram(df$hr_station, df$hr_era5, add = TRUE, col = color, pch = 19, pcex = 1.8)
  
  # Add a reference point for standard deviation for each station
  points(sd(df$hr_station, na.rm = TRUE), 0, col = color, pch = 19, cex = 1.5) # Reference sd for each station
}

# Add a legend for the datasets using their names
legend_labels <- gsub("combined_", "", selected_datasets)  # Remove prefix for clarity
legend(x = 13.5, y = 18,
       legend = legend_labels,
       pch = 19, col = colors[seq_along(selected_datasets)], bty = "n", pt.cex = 1.5)

# Add title to the diagram with enlarged font
title(main = "Relative Humidity", cex.main = 2)

# Close the PNG device
dev.off()




# Set the path for saving the plots
save_path <- "/Users/albert/Desktop/doctorado_definitivo/combined_data/"

# Loop through each data frame in combined_list to create a Taylor diagram for each station
for (i in 1:length(combined_list)) {
  # Get the current data frame
  df <- combined_list[[i]]
  
  # Extract the name of the current data frame
  data_frame_name <- names(combined_list)[i]  # Extract the name from combined_list
  
  # Create a new plot for the Taylor diagram
  png(paste0(save_path, data_frame_name, "_taylor.png"), width = 1500, height = 1500, res = 300, bg = "transparent") 
  
  # Initialize the Taylor diagram with pressure as the first variable pair
  oldpar <- taylor.diagram(df$pres_station, df$pres_era5, col = "blue", pcex = 1.5, main = data_frame_name, cex.main = 2)
  
  # Initialize a vector to store legend labels and colors
  legend_labels <- c("Pressure")  # Start with Pressure
  legend_colors <- c("blue")       # Corresponding color
  
  # Add temperature variable pair
  taylor.diagram(df$temp_station, df$temp_era5, col = "red", sd.arcs = TRUE, pcex = 1.5, add = TRUE)
  legend_labels <- c(legend_labels, "2m Temperature")
  legend_colors <- c(legend_colors, "red")
  
  # Add wind speed variable pair
  taylor.diagram(df$vel_station, df$vel_era5, add = TRUE, col = "green", pcex = 1.5)
  legend_labels <- c(legend_labels, "Wind Speed")
  legend_colors <- c(legend_colors, "green")
  
  # Check if 'hr_station' and 'hr_era5' are present before plotting
  if (all(c("hr_station", "hr_era5") %in% names(df))) {
    taylor.diagram(df$hr_station, df$hr_era5, add = TRUE, col = "yellow", pcex = 1.5)
    legend_labels <- c(legend_labels, "Relative Humidity")
    legend_colors <- c(legend_colors, "yellow")
  } 
  
  # Add reference points for each variable
  points(sd(df$pres_station, na.rm = TRUE), 0, pch = 21, col = "blue", bg = "blue", cex = 1.2)  # Reference for Pressure
  points(sd(df$temp_station, na.rm = TRUE), 0, pch = 21, col = "red", bg = "red", cex = 1.2)   # Reference for Temperature
  points(sd(df$vel_station, na.rm = TRUE), 0, pch = 21, col = "green", bg = "green", cex = 1.2) # Reference for Wind Speed
  
  # Check if 'hr_station' exists before adding reference point
  if (all(c("hr_station") %in% names(df))) {
    points(sd(df$hr_station, na.rm = TRUE), 0, pch = 21, col = "yellow", bg = "yellow", cex = 1.2) # Reference for Relative Humidity
  }
  
  # Add legend for clarity with dynamic labels
  legend(x = 15, y = 22, legend = legend_labels,
         pch = 19, col = legend_colors, bty = "n", pt.cex = 1.5)
  
  # Finish saving the plot
  dev.off()  # Close the device
}



 # first trial - taylor diagram for temperature

{
  taylor.diagram(combined_esperanza$temp_station, combined_esperanza$temp_era5, col = "red", sd.arcs = TRUE, pcex = 1.5)
  taylor.diagram(combined_jci$temp_station, combined_jci$temp_era5, add = TRUE, col = "blue", pcex = 1.5)
  taylor.diagram(combined_carlini$temp_station, combined_carlini$temp_era5, add = TRUE, col = "pink", pcex = 1.5)
  taylor.diagram(combined_ohiggins$temp_station, combined_ohiggins$temp_era5, add = TRUE, col = "yellow", pcex = 1.5)
  taylor.diagram(combined_prat$temp_station, combined_prat$temp_era5, add = TRUE, col = "green", pcex = 1.5)
  taylor.diagram(combined_rothera$temp_station, combined_rothera$temp_era5, add = TRUE, col = "purple", pcex = 1.5)
  taylor.diagram(combined_sanmartin$temp_station, combined_sanmartin$temp_era5, add = TRUE, col = "orange", pcex = 1.5)
  taylor.diagram(combined_vernadsky$temp_station, combined_vernadsky$temp_era5, add = TRUE, col = "lightblue", pcex = 1.5)
  
  legend(x = 16, y = 26, legend = c("Prat","Carlini", "Juan Carlos I","O'higgins", "Esperanza", "Vernadsky", "Rothera", "San Martin"),
         pch = 19, col = c("green", "pink", "blue", "yellow", "red", "lightblue", "purple", "orange"), bty = "n", pt.cex = 1.5)
  title(main = "2 m Temperature")
  print()
}

# Restore original plotting parameters
par(oldpar)

    # Taylor Diagram for pressure
{oldpar <- taylor.diagram(combined_esperanza$pres_station, combined_esperanza$pres_era5, col = "red", sd.arcs = TRUE, pcex = 1.5, main = "")
taylor.diagram(combined_jci$pres_station, combined_jci$pres_era5, col = "blue", add = TRUE, pcex = 1.5, sd.arcs = TRUE)
taylor.diagram(combined_carlini$pres_station, combined_carlini$pres_era5, add = TRUE, col = "pink", pcex = 1.5)
taylor.diagram(combined_ohiggins$pres_station, combined_ohiggins$pres_era5, add = TRUE, col = "yellow", pcex = 1.5)
taylor.diagram(combined_prat$pres_station, combined_prat$pres_era5, add = TRUE, col = "green", pcex = 1.5)
taylor.diagram(combined_rothera$pres_station, combined_rothera$pres_era5, add = TRUE, col = "purple", pcex = 1.5)
taylor.diagram(combined_sanmartin$pres_station, combined_sanmartin$pres_era5, add = TRUE, col = "orange", pcex = 1.5)
taylor.diagram(combined_vernadsky$pres_station, combined_vernadsky$pres_era5, add = TRUE, col = "lightblue", pcex = 1.5)

legend(x = 16, y = 26, legend = c("Carlini", "Esperanza", "Juan Carlos I", "O'higgins", "Prat", "Rothera", "San Martin", "Vernadsky"),
       pch = 19, col = c("pink", "red", "blue", "yellow", "green", "purple", "orange", "lightblue"), bty = "n", pt.cex = 1.5)
title(main = "Pressure comparison")
}
    # Density plots ----
library(ggplot2)
library(rlang)

plot_density_pair <- function(df, var1, var2, xlab) {
  ggplot(df) +
    geom_density(aes_string(x = var1), fill = "black", alpha = 0.5) +
    geom_density(aes_string(x = var2), fill = "tomato", alpha = 0.5) +
    labs(x = xlab, y = "Density") +
    geom_vline(xintercept = mean_era5, color = "tomato",  linetype = "dashed", alpha = 0.6) +
    geom_vline(xintercept = mean_station, color = "black", linetype = "dashed", alpha = 0.6) +
    theme_minimal()
}

# Loop through each dataframe in combined_list
for (name in names(combined_list)) {
  df <- combined_list[[name]]
  plots <- list() # Create a list to store the plots
  
  # Check if columns exist and create plots
  if ("temp_station" %in% names(df) && "temp_era5" %in% names(df)) {
    mean_station <- mean(df$temp_station, na.rm = TRUE)
    mean_era5 <- mean(df$temp_era5, na.rm = TRUE)
    plots$temp <- plot_density_pair(df, "temp_station", "temp_era5", "2 m Temperature (°C)")
  }
  
  if ("vel_station" %in% names(df) && "vel_era5" %in% names(df)) {
    mean_station <- mean(df$vel_station, na.rm = TRUE)
    mean_era5 <- mean(df$vel_era5, na.rm = TRUE)
    plots$vel <- plot_density_pair(df, "vel_station", "vel_era5", "Wind speed (m/s)")
  }
  
  if ("pres_station" %in% names(df) && "pres_era5" %in% names(df)) {
    mean_station <- mean(df$pres_station, na.rm = TRUE)
    mean_era5 <- mean(df$pres_era5, na.rm = TRUE)
    plots$pres <- plot_density_pair(df, "pres_station", "pres_era5", "Pressure (hPa)")
  }
  
  if ("hr_station" %in% names(df) && "hr_era5" %in% names(df)) {
    mean_station <- mean(df$hr_station, na.rm = TRUE)
    mean_era5 <- mean(df$hr_era5, na.rm = TRUE)
    plots$hr <- plot_density_pair(df, "hr_station", "hr_era5", "Relative humidity (%)")
  }
  
  plot <- grid.arrange(grobs = plots, ncol = 1, top = paste(name, "Base"))
  
  folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
  ggsave(filename = paste0(folder_path, "/", name, "_density.png"), plot = plot, width = 5, height = 10, dpi = 300)
}

    # Correlations ----
# Columns to compare
vars_to_compare <- c("temp", "vel", "dir", "hr", "pres")

# Calculate correlations for each matching DataFrame pair
correlations <- lapply(names(era5land), function(name) {
  # Merge the corresponding DataFrames on the date column
  merged_df <- merge(era5land[[name]], stations[[name]], by = "date", suffixes = c("_era5land", "_stations"))
  
  # Calculate correlations for specified columns
  cor_results <- sapply(vars_to_compare, function(col) {
    cor(merged_df[[paste0(col, "_era5land")]], merged_df[[paste0(col, "_stations")]], use = "complete.obs")
  })
  
  # Name the results with the corresponding DataFrame name
  setNames(cor_results, columns_to_compare)
})

# Convert the list of correlations into a named list
names(correlations) <- names(era5land)

# View the correlation results
print(correlations)

    # Scatterplots ----
library(ggplot2)
library(rlang)

scatterplot_pair <- function(df, var1, var2, title) {
  ggplot(df) +
    geom_point(aes_string(x = var1, y = var2), size = 1, alpha = 0.05) +
    geom_abline(slope = 1, colour = "tomato") +
    geom_smooth(aes_string(x = var1, y = var2), method = "lm", color = "black", se = FALSE) +
    labs(title = title, x = "Station", y = "Era5-Land") +
    theme_minimal()
}
for (name in names(combined_list)) {
  df <- combined_list[[name]]
  plots <- list() # Create a list to store the plots
  
  # Check if columns exist and create plots
  if ("temp_station" %in% names(df) && "temp_era5" %in% names(df)) {
    plots$temp <- scatterplot_pair(df, "temp_station", "temp_era5", "2 m Temperature (°C)")
  }
  
  if ("vel_station" %in% names(df) && "vel_era5" %in% names(df)) {
    plots$vel <- scatterplot_pair(df, "vel_station", "vel_era5", "Wind speed (m/s)")
  }
  
  if ("pres_station" %in% names(df) && "pres_era5" %in% names(df)) {
    plots$pres <- scatterplot_pair(df, "pres_station", "pres_era5", "Pressure (hPa)")
  }
  
  if ("hr_station" %in% names(df) && "hr_era5" %in% names(df)) {
    plots$hr <- scatterplot_pair(df, "hr_station", "hr_era5", "Relative humidity (%)")
  }
  
  plot <- grid.arrange(grobs = plots, ncol = 1, top = paste(name, "Base"))
  
  folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
  ggsave(filename = paste0(folder_path, "/", name, "_scatter.png"), plot = plot, width = 5, height = 10, dpi = 300)
}

    # Wind rose plots ----
library(openair)
library(climaemet)
library(cowplot)
# Adding season column to each data frame in combined_list
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

combined_list <- lapply(combined_list, function(df) {
    df$season <- sapply(df$date, assign_season)
  return(df)
})

windRose(combined_list[[1]], ws = "vel_station", wd = "dir_station", angle = 10, breaks = 6, paddle = FALSE, seg = 2, angle.scale = 45, main = "Carlini Base", key.position = "right")

speed = combined_list[[1]]$vel_station
direction = combined_list[[1]]$dir_station
season = combined_list[[1]]$season

ggwindrose(
  speed,
  direction,
  n_speeds = 7,
  calm_wind = 10,
  plot_title = "Carlini Base",
  facet = season,
  n_col = 2,
  col_pal = 'ag_Sunset',
)


library(clifro)
library(viridis)
library(RColorBrewer)

# Function to create wind rose plot
create_windrose <- function(df, var1, var2, var3, title) {
  windrose(
    speed = var1,
    direction = var2,
    facet = var3,
    n_directions = 12,
    speed_cuts = c(0, 5, 10, 20, 30, 40),  # Define consistent speed cuts
    col_pal =  ("YlOrRd"),
    ggtheme = "minimal",
    calm_wind = 0.5,
    n_col = 1
    #axis.text = element_blank()
  ) +
    ggtitle(title) +
    theme(
      plot.title = element_text(size = 10, hjust = 0.5),
      #legend.position = "none",          
      panel.background = element_rect(fill = "grey10", color = NA),
      panel.grid.major = element_line(color = "grey30"), 
      strip.text = element_blank(),
      axis.text.x = element_text(color = "white", size = 10, margin = margin(t = 5)) # Customize color and size of radial labels
      ) 
}

for (name in names(combined_list)) {
  df <- combined_list[[name]]

p1 <- create_windrose(df, df$vel_station, df$dir_station, df$season, "Station")  # First plot without legend
p2 <- create_windrose(df, df$vel_era5, df$dir_era5, df$season, "Era5-Land")        # Second plot with legend

plot <- grid.arrange(p1, p2, ncol = 2,  top = paste(name, "Base"))

#folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
#ggsave(filename = paste0(folder_path, "/", name, "_windrose.png"), plot = plot, width = 6, height = 6, dpi = 300)
ggsave(filename = paste0(name, "_windrose.png"), plot = plot, width = 6, height = 6, dpi = 300, bg = "transparent")
}


# Plot meteo stations and aws ----
meteo <- read.table("meteo.txt", sep = "\t",header = TRUE)
aws <- read.table("aws.txt", sep = "\t",header = TRUE)
stations_validation <- read.table("aws.txt", sep = "\t",header = TRUE)

m<- ggplot() +  
  geom_polygon(data=shp1, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-65.5, -61.8), xlim = c(-64, -55)) +
  geom_point(data=col10, aes(x=longitude_epsg_4326, y=latitude_epsg_4326), color="red", size=1.5, alpha= 0.5) +
  geom_point(data=meteo, aes(x=longitude, y=latitude), color="blue", size=1.5, alpha= 0.5)+
  geom_point(data=aws, aes(x=longitude, y=latitude), color="green", size=1.5, alpha= 0.5)+
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Colonies with at least 10 counts")
m
dev.copy2pdf(file="m.pdf", width = 6, height = 6)

# Gantt chart ----
library(timevis)
gantt <- read.delim("gantt_chart.txt")
timevis(gantt)


# Plotting penguin counts ----
pdf(file="ad.trends.pdf",width=6,height=6)
ggplot(data=Adelie10,aes(x=season_starting, y=penguin_count))+
  geom_point() +
  facet_grid(site_name ~ .) +
  facet_wrap(site_name ~., scales = "free_y", ncol=2) +
  labs(x="Season", y="Number of nests", title="Adélie penguin colonies")
dev.off()

pdf(file="ch.trends.pdf",width=7,height=7)
ggplot(data=Chinstrap10,aes(x=season_starting, y=penguin_count))+
  geom_point() +
  facet_grid(site_name ~ .) +
  facet_wrap(site_name ~., scales = "free_y", ncol=2) +
  labs(x="Season", y="Number of nests", title="Chinstrap penguin colonies")
dev.off()

pdf(file="ge.trends.pdf",width=12,height=7)
ggplot(data=Gentoo10,aes(x=season_starting, y=penguin_count))+
  geom_point() +
  facet_grid(site_name ~. ) +
  facet_wrap(site_name ~., scales = "free_y", ncol=3) +
  labs(x="Season", y="Number of nests", title="Gentoo penguin colonies")
dev.off()



library("RCurl")
library("httr")
set_config( config( ssl_verifypeer = 0L ) )
# grab the data
raw_data <- getURL("https://opendata.aemet.es/opendata/api/antartida/datos/fechaini/2010-01-01T00%3A00%3A00UTC/fechafin/2011-01-01T00%3A00%3A00UTC/estacion/89070
")
raw_data <- getURL("http://opendata.aemet.es/opendata/sh/78236911_202211251452_json")
# Then covert from JSON into a list in R
                data <- fromJSON(file="78236911_202211241911_json")
                length(data)
              
                # We can coerce this to a data.frame
                final_data <- do.call(rbind, data)
                # Then write it to a flat csv file
                write.csv(final_data, "final_data.csv")
               
raw_data <- getURL('https://opendata.aemet.es/opendata/api/antartida/datos/fechaini/2010-01-01T00%3A00%3A00UTC-3/fechafin/2010-02-01T00%3A00%3A00UTC-3/estacion/89070', ssl.verifypeer=FALSE)
connection <- textConnection(raw_data)
dataset <- read.csv(connection, header=FALSE)

# penguin counts from github ----
install.packages('devtools')
devtools::install_github('CCheCastaldo/mapppdr', build_vignettes = TRUE)
library(tidyverse)
library(RefManageR)
library(sf)
library(htmlwidgets)
data(penguin_obs)
head(penguin_obs)


# meteorology Spain - json to dataframe ----
setwd("/Users/albert.pg/Documents/Doctorado/data exploration/meteo españa/gabriel de castilla")
library(jsonlite)
gdc17 <- fromJSON('4b45d68c_202212020315_json.txt', flatten=TRUE)
gdc17 <- as.data.frame(gdc17)
gdc21 <- stream_in(url("https://opendata.aemet.es/opendata/sh/cee1a1aa_202305240851_json"))


library(rjson)

# Walk through a directory to find JSON files
dir <- "/Users/albert.pg/Documents/Doctorado/data exploration/meteo españa/gabriel de castilla"
files <- list.files(dir, pattern = "*.json.txt", recursive = TRUE)
filepaths <- file.path(dir, files)

# Initialise output data frame
df <- data.frame()
# Iterate over all JSON files
for (filepath in filepaths) {
  # Import files
  json_data <- jsonlite::fromJSON(txt= filepath, flatten=TRUE)
  # Construct new row for output data frame
  new_row <- data.frame(
    identificacion = json_data[["identificacion"]],
    nombre = json_data[[ "nombre"]],
    latitud = json_data[[ "latitud" ]],
    longitud = json_data[["longitud"]],
    altitud = json_data[["altitud"]],
    srs = json_data[[ "srs"]],
    alt_nieve = json_data[["alt_nieve"]],
    ddd = json_data[["ddd"]],
    dddstd = json_data[["dddstd"]],
    dddx = json_data[["dddx"]],
    fhora = json_data[["fhora"]],
    hr = json_data[["hr"]],
    ins= json_data[["ins"]],
    lluv= json_data[["lluv"]],
    pres= json_data[["pres"]],
    rad_kj_m2= json_data[["rad_kj_m2"]],
    rad_w_m2= json_data[["rad_w_m2"]],
    rec= json_data[["rec"]],
    temp= json_data[["temp"]],
    tmn= json_data[["tmn"]],
    tmx= json_data[["tmx"]],
    ts= json_data[["ts"]],
    tbs= json_data[["tsb"]],
    tsmn= json_data[["tsmn"]],
    tsmx= json_data[["tsmx"]],
    vel= json_data[["vel"]],
    velx= json_data[["velx"]],
    albedo= json_data[["albedo"]],
    difusa= json_data[["difusa"]],
    directa= json_data[["directa"]],
    global= json_data[["global"]],
    ir_solar= json_data[["ir_solar"]],
    neta= json_data[["neta"]],
    par= json_data[["par"]],
    tcielo= json_data[["tcielo"]],
    ttierra= json_data[["ttierra"]],
    uvab= json_data[["uvab"]],
    uvb= json_data[["uvb"]],
    uvi= json_data[["uvi"]],
    qdato= json_data[["qdato"]]
  )
  # Add new row to output data frame
  df <- rbind(df, new_row)
}
# con todas las variables
df$fhora <- as.POSIXct(df$fhora, tz="UTC",format="%Y-%m-%dT%H:%M:%S")
df <- df[order(df$fhora),]
# Export
utils::write.table(df, "gdc2014.txt", row.names = FALSE)


# AxyTrek data ----
setwd("/Volumes/Pengo2/Antártida 2022-23/Tracking/AxyTrek/Barbijos/tracking_txt")

# packages to use: diveMOVE, crawl, mclust
library(diveMove)

cp01 <- read.csv('CP-01_S1.csv')
cp01txt <- read.table('CP-01_S1.txt')

cp01depth <- subset(cp01, !is.na(Depth))
cp01loc <- subset(cp01, !is.na(location.lat))
# cp01dry <- subset(cp01loc, Activity == 'Active/Dry')

ggplot(cp01loc, aes(x = location.lon, y = location.lat, color = Activity)) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-62.6, -62.2), xlim = c(-59.5, -58.5)) +
  geom_point()

## read in data and create a TDR object
zz <- system.file(file.path("data", "CP-01_S1.csv"),
                  package="diveMove", mustWork=TRUE)

df_export <- cp01loc[,c(1:3,7,10,11)]

df <- read.csv('KI-20_R2_6bV_S1.csv')
df$datetime <- paste(df$Date, ",", df$Time)
df$datetime <- as.POSIXct(df$datetime, format = "%d/%m/%Y , %H:%M:%OS")
head(df$datetime)
new.df <- df[,c(20,10:16)] 
new.df <- subset(new.df, !is.na(location.lat))
write.table(new.df, "KI-20_R2_6bV_S1.txt", row.names = FALSE, col.names=FALSE)

# Modelling breeding success ----
setwd("/Volumes/Pengo2/Doctorado/data exploration/decepcion")

library("ggplot2")
library("gridExtra")
library("grid")
library ("tidyverse")
library("tidyr")
library("mgcv")

d<- read.delim("zonalstatistics.txt", sep="\t", dec =",")
dwind<- read.delim("zonalstatistics_wind.txt", sep="\t", dec =",")

d1 = merge(d, dwind[,c("windeffect_mean","fid")], by="fid")
d1 <- select(d1, "fid", "CLUSTER_ID", "new_nests", "new_productivty", "buffer_area", "perim_area",
             "aspect_majority", "elevation_mean","slope_mean","flow_mean","wetness_mean","windeffect_mean")
names(d1) <- c("id","cluster","nests","productivity","area","perim_area","aspect","elevation","slope","flow","wetness","wind")

p1 <- ggplot(x, aes(productivity)) + geom_density(fill= "tomato", alpha=.6) + labs(x="Breeding success (chicks per nest)", y="Density") + theme_linedraw()
p1
ggsave("p1.png", width = 3, height = 2.5)

p2 <- ggplot(d1, aes(area)) + geom_density() + labs(x= expression ("Area"~(m^2)))
p3 <- ggplot(d1, aes(perim_area)) + geom_density() + labs(x="Perim - Area ratio")
p4 <- ggplot(d1, aes(aspect)) + geom_density() + labs(x="Aspect (º)")
p5 <- ggplot(d1, aes(elevation)) + geom_density() + labs(x="Elevation (m)")
p6 <- ggplot(d1, aes(slope)) + geom_density() + labs(x="Slope (º)", y=NULL)
p7 <- ggplot(d1, aes(flow)) + geom_density() + labs(x="Flow accumulation (log)")
#p8 <- ggplot(d1, aes(wetness)) + geom_density() + labs(x="Wetness index")
p9 <- ggplot(d1, aes(wind)) + geom_density() + labs(x="Wind exposure")

yleft = textGrob("Density", rot = 90)
p = list(p2,p3,p4,p5,p6,p7,p9) %>% map(~.x + labs(y=NULL) + geom_density(fill= "tomato", alpha=.6) + theme_linedraw())
p10 <- grid.arrange(grobs = p ,nrow = 3, left = yleft)
ggsave("p10.png", p10, width = 7, height = 4)

d1[5:12] <- as.data.frame(scale(d1[5:12]))

library(PerformanceAnalytics)
library(corrplot)
d2 <- select(d1,"area","perim_area","aspect","elevation","slope","flow","wind")
cor <- cor(d2, y=NULL, method="spearman",)
corrplot(cor, method = 'color', order = 'AOE', diag = FALSE, type="lower", addCoefasPercent=TRUE, tl.srt=40, tl.col="black") #remove area, high correlation with perim_area

m <- gam(productivity ~ s(perim_area) + s(aspect, bs = "cc") + s(elevation) +s(slope) +s(flow) + s(wind), data=d1, select=TRUE)
m1 <- gam(productivity ~ s(perim_area) + s(aspect, bs = "cc") + s(elevation) +s(slope) +s(flow) + s(wind), data=d1)
m2 <- gam(productivity ~ s(perim_area) + s(aspect, bs = "cc") + s(elevation) +s(slope) +s(flow) + s(wind), data=d1, select=TRUE, method="REML")
m3 <- gam(productivity ~ s(perim_area) + s(aspect, bs = "cc") + s(elevation) +s(slope) +s(flow) + s(wind), data=d1, method="REML")
m4 <- gam(productivity ~ s(perim_area) + s(elevation), data=d1, select=TRUE, method="REML")
summary(m3)

AIC(m, m1, m2, m3, m4) 
par(mfrow = c(2, 3))
gam.check(m4) 
plot(m3, residuals=TRUE)

pred <- data.frame(
  br_success = seq(min(d1$productivity),max(d1$productivity),length.out = 61),
  perim_area = d1$perim_area,
  aspect = d1$aspect,
  elevation = d1$elevation,
  slope = d1$slope,
  flow = d1$flow,
  wind = d1$wind
)


preds <- predict(m4, se.fit = TRUE)
my_data <- data.frame(mu=preds$fit, low =(preds$fit - 1.96 * preds$se.fit), high = (preds$fit + 1.96 * preds$se.fit))
ggplot()+
  geom_line(data = my_data, aes(x=d1$perim_area, y=mu), size=1, col="blue") +
  geom_smooth(data=my_data,aes(ymin = low, ymax = high, x=d1$perim_area, y = mu), stat = "identity", col="green")

ggplot(pred, aes(x = perim_area)) +
  geom_point(aes(y = productivity), size = 1, alpha = 0.5) +
  geom_line(aes(y = predicted_values), colour = "red")

ggplot(pred, aes(x = elevation)) +
  geom_point(aes(y = productivity), size = 1, alpha = 0.5) +
  geom_line(aes(y = predicted_values), colour = "red")

