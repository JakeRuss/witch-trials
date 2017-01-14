# Title: 5-create-temperature.R
# Description: Clean and tidy temperature data from Luterbacher et al. (2004)
# Authors: Peter T. Leeson and Jacob W. Russ

library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(arrayhelpers)
library(maptools)
library(sp)
library(raster)
library(Grid2Polygons)
library(ggplot2)
library(stringr)

# Import data -----------------------------------------------------------------
luterbacher <- "data/raw/Luterbacher_et_al_2004/TT_Europe_1500_2002_NewCG.txt" %>%
  scan(na.strings = -999) 

euro_spdf <- "data/raw/Eurostat/resolution_60_mil/NUTS_RG_60M_2010.shp" %>%
  readShapePoly(proj4string = CRS("+init=epsg:4326"))

nutsid_crosswalk <- read_csv("data/raw/Eurostat/nutsid_to_country_crosswalk.csv")

# Reformat Luterbacher data from array to data frame --------------------------
df <- 1500:2002 %>%
  expand.grid(year = ., season = c("Winter", "Spring", "Summer", "Fall")) %>%
  arrange(year) %>%
  mutate(season      = as.character(season),
         year_season = paste0(year, "_", season))

# If you want the data to be arranged as a 3 dimensional array you could 
# continue with: arrange vector to 3-dim-array [ lat, lon, time]
europe_1500_2002 <- luterbacher %>%
  array(dim      = c(70, 130, 4*503), 
        dimnames = list(lat  = seq(69.75, 35.25, -.5),
                        lon  = seq(-24.75, 39.75, .5),
                        time = df$year_season)) %>%
  array2df(label.x = "temperature") %>%
  as_data_frame() %>%
  separate(col = time, into = c("year", "season"), sep = "_") %>%
  dplyr::select(year, season, lat, lon, temperature) %>%
  mutate(lat         = lat %>% as.character() %>% as.numeric(),
         lon         = lon %>% as.character() %>% as.numeric(),
         temperature = if_else(temperature %in% -999, NA_real_, temperature))

# Look at temperature data seasonally as a check ----------------------------------

seasons_year <- europe_1500_2002 %>%
  group_by(year, season) %>%
  summarise(temp = mean(temperature, na.rm = TRUE))

# Match Luterbacher grids to Eurostat NUTS regions (countries) ----------------

grid_points  <- europe_1500_2002 %>% distinct(lat, lon)
grid_points2 <- grid_points

# Convert lat/lon to SpatialPointsDataFrame
coordinates(grid_points) <- ~lon+lat
proj4string(grid_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
grid_points_spdf <- SpatialPointsDataFrame(grid_points, grid_points2)

# Transform europe map projection to long/lat
euro_spdf %<>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Intersect grids with euro regions
grids_in_countries <- spatialEco::point.in.poly(grid_points_spdf, euro_spdf)

combined <- grids_in_countries@data %>%
  mutate(NUTS_ID      = as.character(NUTS_ID),
         nuts_id_2010 = str_extract(NUTS_ID, "\\D+")) %>%
  left_join(y = nutsid_crosswalk, by = "nuts_id_2010")

euro_df            <- fortify(euro_spdf)
grids_countries_df <- data.frame(grids_in_countries)

# Match country to temperatures ---------------------------------------------

europe_1500_2002 <- europe_1500_2002 %>%
  left_join(y  = dplyr::select(combined, lon, lat, country), 
            by = c("lat", "lon"))

# How many grids get matched to each country? ---------------------------------

n_grids_by_country <- europe_1500_2002 %>%
  distinct(lat, lon, .keep_all = TRUE) %>%
  group_by(country) %>%
  summarise(n = n())

# Check via map ---------------------------------------------------------------

map <- ggplot() + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), 
               fill    = "white", 
               colour  = "black", data = euro_df) +
  coord_cartesian(xlim = c(-20, 50), ylim = c(25, 75)) +
  geom_point(mapping = aes(x = lon, y = lat), 
               fill    = NA, 
               colour  = "black", data = grids_countries_df)

map

# Collapse temperature into country-year observations -----------------------------

temperature_by_decade <- europe_1500_2002 %>%
  filter(!is.na(country)) %>%
  filter(year < 1700) %>%
  mutate(year   = as.numeric(year),
         decade = (year %/% 10) * 10) %>%
  group_by(decade, country) %>%
  summarise(temperature = mean(temperature, na.rm = TRUE) %>% round(digits = 3)) %>%
  arrange(country, decade)

# Calculate temperature standardized values (z-scores) grouped by location.
std_temperature <- temperature_by_decade %>%
  group_by(country) %>%
  mutate(avg.temp    = mean(temperature),
         sd.temp     = sd(temperature),
         zscore.temp = (temperature - avg.temp) / sd.temp) %>%
  ungroup()

# Export temperature series to CSV ------------------------------------------------
write_csv(std_temperature, "data/clean/temperature.csv")
