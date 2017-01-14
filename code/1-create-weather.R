# Title: 0-create-weather.R
# Description: Use raw data from Oster (2004) to create weather series
# Authors: Peter T. Leeson and Jacob W. Russ (2017)

library(readxl)
library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

# Import Oster's datasets
weather  <- read_excel("data/raw/Oster_2004/weatherweb.xls")
trials   <- read_excel("data/raw/Oster_2004/trialsweb.xls")

# Clean Oster's data ----------------------------------------------------------

# Tidy Oster's weather data
clean_weather <- weather %>%
  rename(decade         = Year) %>%
  gather(weather_region, temperature, -decade) %>%
  mutate(weather_region = recode(weather_region, 
                          `England Winter`          = "England",
                          `Russia Winter`           = "Russia",
                          `Germany Winter`          = "Germany",
                          `Hungary Temperature`     = "Hungary",
                          `Switzerland Temperature` = "Switzerland"))

# Rename trials variables for merging with weather
trials <- trials %>% 
  rename(decade   = Date,
         location = Location,
         trials   = Trials,
         deaths   = Deaths)

# Calculate weather standardized values (z-scores) grouped by weather region
std_weather <- clean_weather %>%
  group_by(weather_region) %>%
  mutate(avg.temp    = mean(temperature),
         sd.temp     = sd(temperature),
         zscore.temp = (temperature - avg.temp) / sd.temp) %>%
  ungroup()

# Calculate trials standardized values (z-scores) grouped by location
std_trials <- trials %>%
  group_by(location) %>%
  mutate(avg.trials    = mean(trials),
         sd.trials     = sd(trials), 
         zscore.trials = (trials - avg.trials) / sd.trials) %>%
  ungroup()

# Use assignment rules from Oster (2004) to assign weather regions to countries. 
oster_seven   <- c("Switzerland", "England", "Estonia", "Finland", 
                   "France", "Scotland", "Hungary", "Germany")

weather_seven <- std_weather %>%
  expand(country = oster_seven, decade) %>%
  mutate(weather_region = NA_character_,
         weather_region = if_else(country %in% "England", "England", weather_region),
         weather_region = if_else(country %in% "Switzerland", "Switzerland", weather_region),
         weather_region = if_else(country %in% "Estonia", "Russia", weather_region),
         weather_region = if_else(country %in% "Finland", "Russia", weather_region),
         weather_region = if_else(country %in% "France", "Switzerland", weather_region),
         weather_region = if_else(country %in% "Scotland", "England", weather_region),
         weather_region = if_else(country %in% "Hungary", "Hungary", weather_region),
         weather_region = if_else(country %in% "Germany", "Germany", weather_region)) %>%
  left_join(y = std_weather, by = c("decade", "weather_region")) %>%
  select(country, decade, weather = zscore.temp) %>%
  arrange(country, decade)

# Export final datasets to csv file
write_csv(weather_seven, "data/clean/weather.csv")
