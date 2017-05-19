# Title: 9-create-table-data-excel.R
# Description: Takes panel datasets and outputs an Excel sheet with data for
#              each table of the paper.
# Authors: Peter T. Leeson and Jacob W. Russ

# Load R packages -------------------------------------------------------------

library(readr)
library(dplyr)
library(readxl)
library(magrittr)
library(openxlsx)

# Import datasets -------------------------------------------------------------

trials        <- read_csv("data/trials.csv")
battles       <- read_csv("data/battles.csv")
population    <- read_csv("data/clean/population.csv")
country_panel <- read_csv("data/clean/panel_dataset.csv")
gridded_panel <- read_csv("data/clean/panel_dataset_grids.csv")
figure1       <- read_csv("data/clean/figure1.csv") 
figure2       <- read_csv("data/clean/figure2.csv") 

# Recode United Kingdom countries to GADM 1 Regions ---------------------------
trials <- trials %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0))

battles <- battles %>%
  mutate(gadm.adm0 = if_else(condition = gadm.adm0 %in% "United Kingdom", 
                             true      = gadm.adm1,
                             false     = gadm.adm0))

# Select data by table --------------------------------------------------------

trials_export <- trials %>%
  select(year, decade, tried, deaths, city, county = gadm.adm2, 
         country = gadm.adm0, lon, lat, record.source)

battles_export <- battles %>%
  select(battle, year, decade, war, war.theater, city, county = gadm.adm2,
         country = gadm.adm0, lon, lat, source, source.page)

table_1 <- trials %>%
  group_by(gadm.adm0) %>%
  summarise(trials   = sum(tried, na.rm = TRUE),
            deaths   = sum(deaths, na.rm = TRUE)) %>%
  left_join(y = population %>% filter(decade %in% 1600), by = c("gadm.adm0" = "country")) %>%
  mutate(trials.mil    = ((trials/population) * 1000000) %>% round(),
         deaths.mil    = ((deaths/population) * 1000000) %>% round(), 
         pct.tried     = (trials / sum(trials) * 100) %>% round(digits = 1),
         pct.deaths    = (deaths / sum(deaths) * 100) %>% round(digits = 1)) %>%
  select(country = gadm.adm0, population, persons.tried = trials, pct.tried, 
         trials.mil, deaths, pct.deaths, deaths.mil, -decade) %>%
  arrange(desc(persons.tried))

table_2 <- battles %>%
  group_by(gadm.adm0) %>%
  summarise(total.battles = length(battle)) %>%
  ungroup() %>%
  left_join(y = population %>% filter(decade %in% 1600), by = c("gadm.adm0" = "country")) %>%
  mutate(pct.battles = (total.battles / sum(total.battles) * 100) %>% round(digits = 1),
         battles.mil = ((total.battles / population) * 1000000) %>% round(digits = 1))  %>%
  arrange(desc(total.battles)) %>%
  select(-decade, -population)

table_4 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(trials > 0) %>%
  select(decade, country, ln.trials, ln.trials.mil, battles, 
         battles.tp1, battles.tp2, battles.mil, battles.mil.tp1, 
         battles.mil.tp2)

table_5 <- gridded_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(trials > 0) %>%
  select(decade, grid.id, ln.trials, ln.trials, battles, battles.tp1, 
         battles.tp2)

table_6 <- country_panel %>%
  filter(!is.na(weather)) %>%
  filter(decade >= 1520, decade < 1770) %>%
  filter(trials > 0) %>%
  select(decade, country, ln.trials, ln.trials.mil, 
         battles, battles.mil, weather)

table_7 <- country_panel %>%
  filter(!is.na(urbanization) | !is.na(real.wage)) %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(trials > 0) %>%
  select(decade, country, ln.trials, ln.trials.mil, battles, 
         battles.mil, urbanization, real.wage)

table_8 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(!is.na(taxes.percap)) %>%
  filter(trials > 0) %>%
  select(decade, country, ln.trials, ln.trials.mil, 
         taxes.percap, battles, battles.mil)

table_9 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(trials > 0) %>%
  select(decade, country, ln.trials, ln.trials.mil, battles, 
         urbanization, battles.mil, weather, real.wage, taxes.percap) %>% 
  na.omit()

table_c1 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  select(decade, country, ln1p.trials, ln1p.trials.mil, battles, 
         battles.tp1, battles.tp2, battles.mil, battles.mil.tp1, 
         battles.mil.tp2)

table_c2 <- gridded_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  select(decade, grid.id, ln1p.trials, ln1p.trials, battles, battles.tp1, 
         battles.tp2)

table_c3 <- country_panel %>%
  filter(!is.na(weather)) %>%
  filter(decade >= 1520, decade < 1770) %>%
  select(decade, country, ln1p.trials, ln1p.trials.mil, 
         battles, battles.mil, weather)

table_c4 <- country_panel %>%
  filter(!is.na(urbanization) | !is.na(real.wage)) %>%
  filter(decade >= 1500, decade < 1700) %>%
  select(decade, country, ln1p.trials, ln1p.trials.mil, battles, 
         battles.mil, urbanization, real.wage)

table_c5 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  filter(!is.na(taxes.percap)) %>%
  select(decade, country, ln1p.trials, ln1p.trials.mil, 
         taxes.percap, battles, battles.mil)

table_c6 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  select(decade, country, ln1p.trials, ln1p.trials.mil, battles, 
         urbanization, battles.mil, weather, real.wage, taxes.percap) %>% 
  na.omit()

table_d1 <- country_panel %>%
  filter(decade >= 1500, decade < 1700) %>%
  select(decade, country, ln.trials, ln.trials.mil, battles, 
         battles.mil, temperature)

# Write table data frames to an Excel workbook --------------------------------

# Create a new workbook
wb <- createWorkbook() 

# Initalize the worksheets
addWorksheet(wb, sheetName = "Trials")
addWorksheet(wb, sheetName = "Battles")
addWorksheet(wb, sheetName = "Table1")
addWorksheet(wb, sheetName = "Table2")
addWorksheet(wb, sheetName = "Table4")
addWorksheet(wb, sheetName = "Table5")
addWorksheet(wb, sheetName = "Table6")
addWorksheet(wb, sheetName = "Table7")
addWorksheet(wb, sheetName = "Table8")
addWorksheet(wb, sheetName = "Table9")
addWorksheet(wb, sheetName = "Figure1")
addWorksheet(wb, sheetName = "Figure2")
addWorksheet(wb, sheetName = "TableC1")
addWorksheet(wb, sheetName = "TableC2")
addWorksheet(wb, sheetName = "TableC3")
addWorksheet(wb, sheetName = "TableC4")
addWorksheet(wb, sheetName = "TableC5")
addWorksheet(wb, sheetName = "TableC6")
addWorksheet(wb, sheetName = "TableD1")

# Add data to the worksheet by name

writeData(wb, "Trials",  trials_export,  keepNA = TRUE)
writeData(wb, "Battles", battles_export, keepNA = TRUE)

writeData(wb, "Table1", table_1,   keepNA = TRUE)
writeData(wb, "Table2", table_2,   keepNA = TRUE)
writeData(wb, "Table4", table_4,   keepNA = TRUE)
writeData(wb, "Table5", table_5,   keepNA = TRUE)
writeData(wb, "Table6", table_6,   keepNA = TRUE)
writeData(wb, "Table7", table_7,   keepNA = TRUE)
writeData(wb, "Table8", table_8,   keepNA = TRUE)
writeData(wb, "Table9", table_9,   keepNA = TRUE)

writeData(wb, "Figure1", figure1,  keepNA = TRUE)
writeData(wb, "Figure2", figure2,  keepNA = TRUE)

writeData(wb, "TableC1", table_c1, keepNA = TRUE)
writeData(wb, "TableC2", table_c2, keepNA = TRUE)
writeData(wb, "TableC3", table_c3, keepNA = TRUE)
writeData(wb, "TableC4", table_c4, keepNA = TRUE)
writeData(wb, "TableC5", table_c5, keepNA = TRUE)
writeData(wb, "TableC6", table_c6, keepNA = TRUE)
writeData(wb, "TableD1", table_d1, keepNA = TRUE)

# Save the workbook to directory ----------------------------------------------
saveWorkbook(wb        = wb, 
             file      = "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx", 
             overwrite = TRUE)
