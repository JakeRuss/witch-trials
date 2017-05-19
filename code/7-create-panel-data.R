# Title: 7-create-panel-data.R
# Description: Replication file "Witch Trials."
# Authors: Peter T. Leeson and Jacob W. Russ

# Load R packages -------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(readxl)
library(magrittr)

# Import datasets -------------------------------------------------------------

trials      <- read_csv("data/trials.csv")
battles     <- read_csv("data/battles.csv") 
population  <- read_csv("data/clean/population.csv")
weather     <- read_csv("data/clean/weather.csv")
real_wage   <- read_csv("data/clean/real_wages.csv")
urban       <- read_csv("data/clean/urbanization.csv")
taxes       <- read_csv("data/clean/taxes-manually-modified.csv")
temperature <- read_csv("data/clean/temperature.csv") %>%
  select(country, decade, temperature = zscore.temp)

# Recode United Kingdom countries to GADM 1 Regions ---------------------------
trials <- trials %>%
  mutate(country = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                           true      = gadm.adm1,
                           false     = gadm.adm0))

battles <- battles %>%
  mutate(country = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                           true      = gadm.adm1,
                           false     = gadm.adm0))

# Expand the trials data to make a balanced panel -----------------------------

df <- data_frame(decade = c(1800, 1810, 1840), tried = 0, country = "Austria")

trials_by_country_decade <- trials %>%
  # Add decades that do not appear in the data before using complete
  bind_rows(data_frame(decade  = c(1800, 1810, 1840), 
                       tried   = 0, 
                       country = "Austria")) %>%
  # Fill in "missing" observations with zero trials
  complete(country, decade, fill = list(tried = 0)) %>%
  group_by(country, decade) %>%
  summarise(trials = sum(tried, na.rm = TRUE)) %>%
  # Ad century via decade
  mutate(century = (decade %/% 100) * 100) %>%
  select(country, decade, century, trials)

battles_by_country_decade <- battles %>%
  group_by(country, decade) %>%
  summarise(battles = n())

# Merge datasets to create panel ----------------------------------------------

combined <- trials_by_country_decade %>%
  left_join(y = battles_by_country_decade, by = c("country", "decade")) %>%
  replace_na(list(battles = 0)) %>%
  left_join(y = population, by = c("country", "decade")) %>%
  mutate(trials.mil        = ((trials / population) * 1000000) %>% round(digits = 3),
         battles.mil       = ((battles / population) * 1000000) %>% round(digits = 3),
         ln.trials         = if_else(trials %in% 0, NA_real_, log(trials)), 
         ln.trials.mil     = if_else(trials.mil %in% 0, NA_real_, log(trials.mil)),
         ln1p.trials       = log1p(trials),
         ln1p.trials.mil   = log1p(trials.mil)) %>%
  # Add three "future" battles columns for the placebo test. For leads or lags 
  # to work correctly, we need to sort the data frame and use only one grouping 
  # variable. In this case use country name.
  arrange(country, decade) %>%
  group_by(country) %>%
  mutate(battles.tp1        = lead(battles, 1),
         battles.tp2        = lead(battles, 2),
         battles.tp3        = lead(battles, 3),
         battles.mil.tp1    = lead(battles.mil, 1),
         battles.mil.tp2    = lead(battles.mil, 2),
         battles.mil.tp3    = lead(battles.mil, 3))

# Add external sources

combined <- combined %>%
  left_join(y = weather,   by = c("country", "decade")) %>%
  left_join(y = urban,     by = c("country", "century")) %>%
  left_join(y = real_wage, by = c("country", "decade")) %>%
  left_join(y = select(taxes, -revenues), by = c("country", "decade")) %>%
  left_join(y = temperature, by = c("country", "decade"))

# Export temperature series to CSV --------------------------------------------

write_csv(combined, "data/clean/panel_dataset.csv")

  