# Title: 5-create-taxes-real-wages.R
# Description: Clean real wage data from Allen (2001) and tax revenue per 
#              capita data from Karaman and Pamuk (2013) 
# Authors: Peter T. Leeson and Jacob W. Russ 

library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(tidyr)

# Import raw wage and tax data
taxes     <- read_excel("data/raw/Karaman_Pamuk_2013/tax_revenues.xlsx")
real_wage <- read_csv("data/raw/Allen_2001/real_wages.csv")

# Clean and tidy the tax revenues ---------------------------------------------

clean_taxes <- taxes %>%
  select(decade, England, Netherlands = `Dutch R`, France, Spain,
         Italy = Venice, Austria, Poland = `Poland Lithuania `) %>%
  gather(country, revenues, -decade) %>%
  separate(decade, into = "decade", sep = "-", extra = "drop", convert = TRUE)

clean_taxes_panel <- clean_taxes %>%
  expand(country, decade = seq(1500, 1790, 10)) %>%
  left_join(y = clean_taxes, by = c("decade", "country"))

# Clean and tidy the real wages -----------------------------------------------
clean_real_wage <- real_wage %>%
  gather(country, real.wage, -year) %>%
  mutate(decade  = (year %/% 10) * 10,
         country = recode(country,
                          Antwerp    = "Belgium",
                          Amsterdam  = "Netherlands",
                          London     = "England",
                          Oxford     = "England",
                          Paris      = "France",
                          Strasbourg = "France",
                          Florence   = "Italy",
                          Milan      = "Italy",
                          Naples     = "Italy",
                          Valencia   = "Spain",
                          Madrid     = "Spain",
                          Augsburg   = "Germany",
                          Leipzig    = "Germany",
                          Munich     = "Germany",
                          Vienna     = "Austria",
                          Gdansk     = "Poland",
                          Krakow     = "Poland",
                          Warsaw     = "Poland",
                          Lwow       = "Ukraine",
                          Hamburg    = "Germany")) %>%
  group_by(country, decade) %>%
  summarise(real.wage = mean(real.wage, na.rm = TRUE),
            real.wage = if_else(condition = is.nan(real.wage), 
                                true      = NA_real_, 
                                false     = real.wage))

# Export taxes and wages data to csv
write_csv(clean_taxes_panel, "data/clean/taxes.csv")
write_csv(clean_real_wage, "data/clean/real_wages.csv")
