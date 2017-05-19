# Title: 6-create-figure-data.R
# Description: The following code creates the data for figures 1 and 2 from 
#              "Witch Trials."
# Authors: Peter T. Leeson and Jacob W. Russ 

# Load R packages -------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)
library(maptools)
library(ggplot2)
library(stringr)
library(rgeos)
       
# Import datasets -------------------------------------------------------------

trials      <- read_csv("data/trials.csv")
battles     <- read_csv("data/battles.csv")
population  <- read_csv("data/clean/population.csv")
europe_adm2 <- read_csv("data/raw/GADM/europe_gadm_adm2.csv")


# Create Figure 1 data --------------------------------------------------------

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
trials_w_centroids <- trials %>%
  filter(decade >= 1500, decade < 1700) %>%
  left_join(y = centroids_adm2, by = c("gadm.adm2", "gadm.adm1", "gadm.adm0")) %>%
  mutate(lon = if_else(is.na(lon) & !is.na(c.lon), c.lon, lon),
         lat = if_else(is.na(lat) & !is.na(c.lat), c.lat, lat)) %>%
  filter(!is.na(lat))

trials_and_battles <- 
  bind_rows("Confessional battle" = battles %>% filter(!is.na(lat)) %>% select(lon, lat),
            "Witchcraft trial"    = trials_w_centroids %>% 
              select(lon, lat), .id = "event")

# Output a file for publication
write_csv(trials_and_battles, "data/clean/figure1.csv")

# Create Figure 2 data --------------------------------------------------------

# Count the number of persons tried for witchcraft by country and decade
witches_decade <- trials %>%
  # Remove records with no date
  filter(!is.na(decade)) %>%
  # Group by decade
  group_by(decade) %>%
  # Aggregate the trial records
  summarise(trials = sum(tried, na.rm = TRUE), 
            deaths = sum(deaths, na.rm = TRUE))

# Count confessional-battles by decade
battles_by_decade <- battles %>%
  filter(!is.na(decade)) %>%
  group_by(decade) %>%
  summarise(battles = n())

# Combine to make a Figure 2 file
fig2_data <- witches_decade %>%
  left_join(x  = ., 
            y  = battles_by_decade,
            by = "decade") %>%
  select(decade, trials, deaths, battles) %>%
  # Change NA values to zeroes
  replace_na(list(battles = 0))

# Output a file for publication
write_csv(fig2_data, "data/clean/figure2.csv")
