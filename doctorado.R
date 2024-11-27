# Dataset with all penguin counts available on penguinmap.com
setwd("/Volumes/Pengo2/Doctorado/data exploration")

library ("tidyverse")
library ("fastDummies")
library("rjson")
library("dplyr")
library("tidyr")
library("lubridate")
library("gridExtra")

# Processing penguin colony database ----
d<- read.csv("CountQuery_V_4_1.csv")
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

#setwd("/Volumes/Pengo2/Doctorado/data exploration")
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_palmer")

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
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/palmer_other_2019")

# Define the path to folder containing the .txt files
folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/palmer_other_2019"

# List all .txt files in the folder
file_list <- list.files(path = folder_path, pattern = "_BASE.txt$", full.names = FALSE, recursive = TRUE) # recursive to read within subfolders too

# Initialize an empty list to store data frames
df_list <- list()

process_df <- function(df) {
  df <- df %>%
    select(c(1:15)) %>% 
    mutate(V1 = as.POSIXct(paste(V1, V2, sep = " "), format = "%Y/%m/%d %H:%M:%S", tz = "UTC")) %>%
    #select(-V2) %>% 
    mutate(across(4:15, as.numeric)) %>% 
    mutate(across(3, as.character))
  return(df)
}

# Read files
for (file in file_list) {
  df <- fread(file, fill = TRUE, na.strings = "NA", skip=1, header = FALSE)
  df <- process_df(df)   # Standardize column types
  df_list[[length(df_list) + 1]] <- df   # Add the dataframe to the list
}

#Rename data frames as the txt files
names(df_list) <- sub("\\.txt$", "", basename(file_list))

# Combine all dataframes into one
df <- bind_rows(df_list)

# Check NAs and shifted columns 
check <- df %>% filter(V4 == 360)
na_check <- sapply(df_list, function(df) any(is.na(df$V1)))
na_dfs <- df_list[na_check] # Keep only the data frames that contain NA in V1


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
    # Convert columns to character type if needed
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
    
    # Convert columns to character type if needed
    #mutate(across(everything(), as.character)) %>%
    
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

# Load necessary libraries
library(dplyr) # For data manipulation
library(lubridate) # For date-time functions
library(stringr)

# Define the path to folder containing the .txt files
folder_path <- "/Volumes/Pengo2/Doctorado/data exploration/meteo/meteo_argentina"

# List all .txt files in the folder
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
  # Extract the base name of the file (without the path and extension)
  file_name <- tools::file_path_sans_ext(basename(file))

  # Read the current file
  df <- read.delim(file, sep = "", header = TRUE, skip = 2, dec = ",") # Adjust the sep, header, and skip parameters as needed
  
  # Apply the function process_df
  df <- process_df(df)
  
  # Create the CSV file name
  csv_file_name <- paste0(file_name, ".csv")
  
  # Save the processed dataframe to a .csv file
  write.csv(df, file = csv_file_name, row.names = FALSE)
  
  # Print a message to confirm the file was saved
  cat("Exported", csv_file_name, "\n")
}

    # Rothera ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/meteo_rothera")

process_df <- function(df) {
  df <- df %>%
    # Convert time to a formatted string
    mutate(date = as.POSIXct(paste(V1, V2, V3, V4, V5, sep = "-"), format = "%Y-%m-%d-%H-%M", tz = "UTC")) %>%
    # Drop the original time columns and unnecessary columns
    select(-c(1:6,9,14,15,17:20,22:34)) %>%
    # Reorder columns, date first
    select(date, everything())  %>% 
    # Change ckass of numeric variables
    mutate(across(2:9, as.numeric))
  return(df)
}

# Read and process data from Rothera
df <- read.delim("rothera.txt", sep = "", header = FALSE, skip = 2, dec = ".")
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
write.csv(df, "rothera.csv", row.names = FALSE)

# Stations and nearest grid point in ERA5-Land ----
setwd("/media/donoso/Pengo2/Doctorado/datos_netcdf_rema/era5land")
library(mapdata)
library(rgdal)
library(ggmap)
library(raster)
library(mapproj)
library(sf)
library(ggplot2)

#shp_file <- "/Volumes/Pengo2/Doctorado/data exploration/add_coastline_high_res_polygon_v7_4/add_coastline_high_res_polygon_v7_4.shp"
shp_file <- "/Volumes/Pengo2/Doctorado/ATA_adm0/ATA_adm0.shp"
shp <- st_read(shp_file)
shp.df <- as_Spatial(shp)

# Read weather station and ERA5-Land coordinates
stations <- read.csv("station_locations.csv", sep = ",", header = TRUE)

# Plot
m1<- ggplot() +  
  geom_polygon(data=shp.df, aes(x=long, y=lat, group=group), alpha= 0.6) +
  coord_map(project="stereographic", orientation=c(-90,-60,0),
            ylim = c(-72, -62), xlim = c(-70, -55)) +
  geom_point(data=stations, aes(x=target_long, y=target_lat), color="black",size=0.5) +
  geom_point(data = stations, aes(x = nearest_lon, y = nearest_lat), color = "red", size = 0.5, alpha = 0.5) +
  geom_text(data = stations, aes(x = target_long, y = target_lat, label = location), 
            vjust = -1, color = "black", size = 4) + 
  #theme(panel.background = element_rect(fill = "black"), panel.grid.major = element_line(colour = "white", linetype = "dotted")) +
  labs(x = "", y = "", title = "Weather stations (black) and ERA5-Land grid points (red)")
m1

ggsave("ws_peninsula.png", plot = m1, width = 8, height = 8, dpi = 300)
ggsave("ws_fossilbluff.png", plot = m1, width = 8, height = 8, dpi = 300)

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


    # Process weather station data and identify a study periods with less than 15 days of NAs per year ----
setwd("/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/flags_3h_tolerance15")
#setwd("/Volumes/Pengo2/Doctorado/data exploration/meteo/flags_3h_tolerance30")

# List all .csv files in the folder
file_list <- list.files(pattern = "\\.csv$", full.names = TRUE, recursive = TRUE) # recursive to read within subfolders too

# Read each CSV file into a separate data frame
data_frames <- lapply(file_list, read.csv)

# Name each data frame based on the file name
names(data_frames) <- sub("\\.csv$", "", basename(file_list))

lapply(names(data_frames), function(x) {
  data_frames[[x]]$date <- as.POSIXct(data_frames[[x]]$date, tz="UTC") 
  assign(x, data_frames[[x]], envir = .GlobalEnv)
})

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

# Create a list of data frames
df_list15 <- list(ohiggins15, carlini15, esperanza15, jci15, prat15, rothera15, sanmartin15, vernadsky15)
#df_list30 <- list(ohiggins30, carlini30, esperanza30, jci30, prat30, rothera30, sanmartin30, vernadsky30)

# Combine all data frames by date and filter
df <- df_list15 %>%
  Reduce(function(x, y) full_join(x, y, by = "date"), .) %>%
  filter(if_all(-date, ~ . == 1))

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
  filter(all(!date %in% missing_dates)) %>%  # Keep groups without missing dates
  summarise(start = min(date), end = max(date), .groups = 'drop')

    # Extract study period from weather stations ----
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
era5land_list <- map(era5land_list, ~ setNames(.x, new_column_names)) # Rename columns in each data frame

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

    # Extract study period from ERA5Land ----
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
library(ggplot2)
library(gridExtra)

# Function to create a time series plot for two variables
plot_time_series_pair <- function(df, var1, var2, ylab) {
  ggplot(df, aes(x = date)) +
    geom_line(aes(y = !!sym(var1), color = var1), size = 0.6, alpha = 0.7) +
    geom_line(aes(y = !!sym(var2), color = var2), size = 0.6, alpha = 0.7) +
    labs(x = NULL, y = ylab) +
    scale_color_manual(values = c("tomato", "black"),
                       labels = c("Era5-Land","Station")) +
    theme_minimal() +
    theme(legend.title = element_blank())
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
  
  plot <- grid.arrange(grobs = plots, ncol = 1, top = paste(name, "Base"))
  
  folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
  ggsave(filename = paste0(folder_path, "/", name, "_timeseries.png"), plot = plot, width = 10, height = 10, dpi = 300)
}

    # Taylor diagrams ----
library(plotrix)
    # Taylor Diagram for temperature
{oldpar <- taylor.diagram(combined_esperanza$temp_station, combined_esperanza$temp_era5, col = "red", sd.arcs = TRUE, pcex = 1.5)
taylor.diagram(combined_jci$temp_station, combined_jci$temp_era5, add = TRUE, col = "blue", pcex = 1.5)
taylor.diagram(combined_carlini$temp_station, combined_carlini$temp_era5, add = TRUE, col = "pink", pcex = 1.5)
taylor.diagram(combined_ohiggins$temp_station, combined_ohiggins$temp_era5, add = TRUE, col = "yellow", pcex = 1.5)
taylor.diagram(combined_prat$temp_station, combined_prat$temp_era5, add = TRUE, col = "green", pcex = 1.5)
taylor.diagram(combined_rothera$temp_station, combined_rothera$temp_era5, add = TRUE, col = "purple", pcex = 1.5)
taylor.diagram(combined_sanmartin$temp_station, combined_sanmartin$temp_era5, add = TRUE, col = "orange", pcex = 1.5)
taylor.diagram(combined_vernadsky$temp_station, combined_vernadsky$temp_era5, add = TRUE, col = "lightblue", pcex = 1.5)

legend(x = 16, y = 26, legend = c("Carlini", "Esperanza", "Juan Carlos I", "O'higgins", "Prat", "Rothera", "San Martin", "Vernadsky"),
       pch = 19, col = c("pink", "red", "blue", "yellow", "green", "purple", "orange", "lightblue"), bty = "n", pt.cex = 1.5)
title(main = "2 m Temperature")
}
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
speed = combined_list[[1]]$vel_station
direction = combined_list[[1]]$dir_station
season = combined_list[[1]]$season

windrose(
  speed,
  direction,
  facet = season,
  n_directions = 12,
  n_speeds = 7,
  col_pal = "YlOrRd",
  ggtheme = "minimal",
  legend_title = "Wind Speed (m/s)",
  calm_wind = 0.5,
  max = 35,
  n_col = 2,
  axis.text = element_blank(),
  axis.ticks = element_blank()
)

# Function to create wind rose plot
create_windrose <- function(df, var1, var2, var3, title) {
  windrose(
    speed = var1,
    direction = var2,
    facet = var3,
    n_directions = 12,
    speed_cuts = c(0, 5, 10, 20, 30, 40, 50),  # Define consistent speed cuts
    col_pal = "YlOrRd",
    ggtheme = "minimal",
    calm_wind = 0.5,
    n_col = 1,
    axis.text = element_blank()
  ) +
    ggtitle(title) +
    theme(plot.title = element_text(size = 10, hjust = 0.5),
          legend.title = element_text(size = 9)
    )
}

for (name in names(combined_list)) {
  df <- combined_list[[name]]

p1 <- create_windrose(df, df$vel_station, df$dir_station, df$season, "Station")  # First plot without legend
p2 <- create_windrose(df, df$vel_era5, df$dir_era5, df$season, "Era5-Land")        # Second plot with legend

plot <- grid.arrange(p1, p2, ncol = 2, widths = c(1, 1.1),  top = paste(name, "Base"))

folder_path <- "/media/ddonoso/Pengo2/Doctorado/data exploration/meteo/era5land/figures"
ggsave(filename = paste0(folder_path, "/", name, "_windrose.png"), plot = plot, width = 6, height = 6, dpi = 300)
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
