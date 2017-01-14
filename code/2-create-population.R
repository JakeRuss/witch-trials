# Title: 0-create-population.R
# Description: This file takes raw population statistics from McEvedy and 
#              Jones (1978) and linearly interpolates the values for decade
#              by decade population estimates.
# Authors: Peter T. Leeson and Jacob W. Russ 

library(readr)
library(dplyr)
library(magrittr)
library(tidyr)
library(zoo)

# Import McEvedy and Jones (1978) data plus Palli (1980) for Estonia in 1630
mcevedy <- "data/raw/McEvedy_Jones_1978/mcevedy-jones-1978.csv" %>%
  read_csv() %>%
  rename(decade = year)

# Apply a land area ratio rule to McEvedy and Jones populations to split records
# into modern countries
# Belgium (11,787 square miles), Luxembourg (999), Czech Republic (30,450),
# Slovakia (18,933), England (50,346), Wales (8,016), Republicof Ireland (27,133)
mcevedy <- mcevedy %>%
  mutate(land.ratio = 1,
         land.ratio = if_else(country %in% "Belgium", true = .922, false = land.ratio),
         land.ratio = if_else(country %in% "Luxembourg", true = .078, false = land.ratio),
         land.ratio = if_else(country %in% "Czech Republic", true = .617, false = land.ratio),
         land.ratio = if_else(country %in% "Slovakia", true = .383, false = land.ratio),
         land.ratio = if_else(country %in% "Wales", true = .137, false = land.ratio),
         land.ratio = if_else(country %in% "England", true = .863, false = land.ratio),
         land.ratio = if_else(country %in% "Ireland", true = .835, false = land.ratio),
         land.ratio = if_else(country %in% "Northern Ireland", true = .165, false = land.ratio),
         # Adjust population by land area ratio
         adj.pop = population * land.ratio)

# Create a data frame that contains adjusted population data at the 
# country decade level
pop_country_decade <- mcevedy %>%
  expand(country, decade = seq(1300, 1900, 10)) %>%
  left_join(y  = select(mcevedy, country, decade, adj.pop), 
            by = c("country", "decade")) %>%
  rename(population = adj.pop)

# Use Linearly interpolate the decade values
pop_country_decade$population <- na.approx(pop_country_decade$population)

# Export to data frame to clean directory
write_csv(pop_country_decade, "data/clean/population.csv")
