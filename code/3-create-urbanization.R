# Title: 3-create-urbanization.R
# Description: Use raw Bairoch data to create urbanization series
# Authors: Peter T. Leeson and Jacob W. Russ 

library(readxl)
library(readr)
library(magrittr)
library(tidyr)
library(dplyr)

# Import raw data -------------------------------------------------------------
bairoch <- "data/raw/Bairoch_1988/bairoch-modified.csv" %>%
  read_csv(col_types = list("800"  = col_integer(),
                            "900"  = col_integer()))

population <- read_csv("data/clean/population.csv")


# Tidy Bairoch ----------------------------------------------------------------
bairoch_tidy <- bairoch %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0)) %>%
  gather(year, population, -country, -city, -gadm.adm0, -gadm.adm1, 
         -gadm.adm2, convert = TRUE) %>%
  select(-country) %>%
  rename(country = gadm.adm0)

# Summarise Bairoch population by country
urban_pop_by_country <- bairoch_tidy %>%
  filter(!is.na(country)) %>%
  # (1) Scale population variable (i.e. 5 = 5,000)
  # (2) Set a minimum of 1,000 persons for all named places in all years
  mutate(population     = population * 1000,
         min.population = if_else(is.na(population), 1000, population)) %>%
  group_by(year, country) %>%
  summarise(total.bairoch  = sum(population, na.rm = TRUE),
            total.with.min = sum(min.population),
            urban.pop      = sum(population[population > 5000], na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(y = population, by = c("year" = "decade", "country")) %>%
  mutate(urbanization = ((urban.pop / population) * 100) %>% round(digits = 3))

# Subset for 1300-1850
urbanization_final <- urban_pop_by_country %>%
  select(century = year, country, urbanization) %>%
  filter(century >= 1300, century < 1860) %>%
  arrange(country, century)

# Export urbanization rates to csv
write_csv(urbanization_final, "data/clean/urbanization.csv")

