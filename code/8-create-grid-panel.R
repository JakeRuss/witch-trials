# Title: 8-create-grid-cells.R
# Description: Create a panel dataset by grid cell. 
# Authors: Peter T. Leeson and Jacob W. Russ 

library(readr)
library(raster)
library(dplyr)
library(magrittr)
library(tidyr)
library(maptools)
library(sp)
library(spatialEco) 
library(Grid2Polygons)
library(rgdal)
library(ggplot2)

# Import datasets -------------------------------------------------------------

trials      <- read_csv("data/trials.csv")
battles     <- read_csv("data/battles.csv")

euro_spdf   <- readShapePoly("data/raw/Eurostat/resolution_60_mil/NUTS_RG_60M_2010.shp", 
                           proj4string = CRS("+init=epsg:4326"))
# GADM 2012
europe_adm2 <- read_csv("data/raw/GADM/europe_gadm_adm2.csv")

# Calculate the centroid of the counties
centroids_adm2 <- europe_adm2 %>%
  select(gadm.adm0 = NAME_0, gadm.adm1 = NAME_1, gadm.adm2 = id, 
         lon = long, lat) %>%
  group_by(gadm.adm0, gadm.adm1, gadm.adm2) %>%
  summarise(c.lon = mean(lon),
            c.lat = mean(lat)) %>% 
  ungroup()

# Recode United Kingdom countries to GADM 1 Regions ---------------------------
trials <- trials %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0))

battles <- battles %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0))

centroids_adm2 <- centroids_adm2 %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0))


# Merge centroids onto trials
trials_w_centroids <- centroids_adm2 %>%
  left_join(x = trials, y = ., by = c("gadm.adm2", "gadm.adm1", "gadm.adm0")) %>%
  mutate(lon = if_else(is.na(lon) & !is.na(c.lon), c.lon, lon),
         lat = if_else(is.na(lat) & !is.na(c.lat), c.lat, lat))

# Make events into point shapefiles
trials_coords  <- trials_w_centroids %>% filter(!is.na(lat)) %>% select(lon, lat)
battles_coords <- battles %>% filter(!is.na(lat)) %>% select(lon, lat)

trials_spdf <- trials_w_centroids %>%
  filter(!is.na(lat)) %>%
  SpatialPointsDataFrame(coords = trials_coords, 
                         data   = .,
                         proj4string = CRS("+init=epsg:4326"))

battles_spdf <- battles %>%
  filter(!is.na(lat)) %>%
  SpatialPointsDataFrame(coords = battles_coords, 
                         data   = .,
                         proj4string = CRS("+init=epsg:4326"))

# Convert to Euro equal area projection in meters
euro_spdf    %<>% spTransform(CRS("+init=epsg:3035"))
trials_spdf  %<>% spTransform(CRS("+init=epsg:3035"))
battles_spdf %<>% spTransform(CRS("+init=epsg:3035"))

# Define SpatialGrid object in the same projection
bb <- bbox(euro_spdf)
cs <- c(1, 1) * 250000  # cell size 100km x 100km (for illustration)
# Units for epsg:3035 = meters
cc <- bb[ , 1] + (cs / 2)  # cell offset
cd <- ceiling(diff(t(bb))/cs)  # number of cells per direction
grd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
grd

sp_grd <- SpatialGridDataFrame(grid        = grd,
                               data        = data.frame(id = 1:prod(cd)),
                               proj4string = CRS("+init=epsg:3035"))

poly_grid <- Grid2Polygons(sp_grd)

# Keep only the grids that are over land --------------------------------------
poly_grid_subset <- raster::intersect(x = poly_grid, y = euro_spdf)

euro_df    <- fortify(euro_spdf)
grid_df    <- fortify(poly_grid_subset)
trials_df  <- data.frame(trials_spdf)
battles_df <- data.frame(battles_spdf)

trials_pts_poly  <- point.in.poly(trials_spdf, poly_grid_subset)
battles_pts_poly <- point.in.poly(battles_spdf, poly_grid_subset)

trials_by_grid <- trials_pts_poly@data %>%
  as_data_frame() %>%
  rename(grid.id = z) %>%
  group_by(grid.id, decade) %>%
  summarise(trials = sum(tried))

battles_by_grid <- battles_pts_poly@data %>%
  as_data_frame() %>%
  rename(grid.id = z) %>%
  group_by(grid.id, decade) %>%
  summarise(battles = n())

combined <- poly_grid_subset@data %>%
  as_data_frame() %>%
  rename(grid.id = z) %>%
  expand(grid.id, decade = seq(1300, 1850, 10)) %>%
  left_join(y = trials_by_grid,  by = c("grid.id", "decade")) %>%
  left_join(y = battles_by_grid, by = c("grid.id", "decade")) %>%
  # Change "missing" battles to zeroes
  replace_na(list(battles = 0, trials = 0)) %>%
  # Add three "future" battles columns for the placebo test. For leads or lags 
  # to work correctly, we need to sort the data frame and use only one grouping 
  # variable. In this case use country name.
  arrange(grid.id, decade) %>%
  group_by(grid.id) %>%
  mutate(ln.trials   = if_else(trials %in% 0, NA_real_, log(trials)),
         ln1p.trials = log1p(trials),
         battles.tp1 = lead(battles, 1),
         battles.tp2 = lead(battles, 2),
         battles.tp3 = lead(battles, 3)) %>%
  ungroup() %>%
  mutate(grid.id = factor(grid.id) %>% as.numeric)

# Plot map of Europe as a check
map <- ggplot() + 
  geom_polygon(mapping = aes(x = long, y = lat, group = group), 
                 fill    = "white", 
                 colour  = "black", data = euro_df) +
  coord_cartesian(xlim = c(2000000, 8000000), ylim = c(1000000, 5500000)) +
  geom_point(mapping = aes(x = lon.1, y = lat.1),
               size   = 2,
               data   = trials_df) +
  geom_point(mapping = aes(x = lon.1, y = lat.1),
             size   = 2, colour = "red",
             data   = battles_df) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group), 
               fill    = NA, 
               colour  = "black", data = grid_df)
  
map

# Export grids to csv
write_csv(combined, "data/clean/panel_dataset_grids.csv")
