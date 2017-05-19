# Title: ej-regressions.R
# Description: Replication file for tables 4 - 10 from "Witch Trials."
# Authors: Peter T. Leeson and Jacob W. Russ

# Load R packages -------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
library(stargazer)
library(sandwich)
library(broom)

# Import datasets -------------------------------------------------------------

table_4 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Table4", na = "NA")

table_5 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>% 
  read_excel(sheet = "Table5", na = "NA")

table_6 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Table6", na = "NA")

table_7 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Table7", na = "NA")

table_8 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Table8", na = "NA")

table_9 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "Table9", na = "NA")

# Table 4: Religious-Market Contestation and Witch Trials (Panel A) -----------

# OLS (1500-1699)
t4_1a <- "ln.trials ~ battles"
# OLS (1500-1699) + period FE
t4_2a <- "ln.trials ~ battles + factor(decade)"
# OLS (1500-1549)
t4_3a <- "ln.trials ~ battles"
# OLS (1500-1549) + period FE
t4_4a <- "ln.trials ~ battles + factor(decade)"
# OLS + country-specific time trends
t4_5a <- "ln.trials ~ battles + factor(country) + factor(decade)"
# Placebo test regression (future battles) 
t4_6a <- "ln.trials ~ battles + battles.tp1 + battles.tp2 + factor(country) + factor(decade)"

# Run each model
results_t4_1a <- lm(formula = t4_1a, data = table_4)
results_t4_2a <- lm(formula = t4_2a, data = table_4)
results_t4_3a <- lm(formula = t4_3a, data = filter(table_4, decade < 1550))
results_t4_4a <- lm(formula = t4_4a, data = filter(table_4, decade < 1550))
results_t4_5a <- lm(formula = t4_5a, data = table_4)
results_t4_6a <- lm(formula = t4_6a, data = table_4)

# Use cluster-robust standard errors 

clrse_t4_1a <- results_t4_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_2a <- results_t4_2a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_3a <- results_t4_3a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_4a <- results_t4_4a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_5a <- results_t4_5a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_6a <- results_t4_6a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

t4_labs_a <- c("Confessional battles", 
               "Confessional battles (t + 1)",
               "Confessional battles (t + 2)")

stargazer(results_t4_1a, results_t4_2a, results_t4_3a, 
          results_t4_4a, results_t4_5a, results_t4_6a,
          se                = list(clrse_t4_1a, clrse_t4_2a, 
                                   clrse_t4_3a, clrse_t4_4a,
                                   clrse_t4_5a, clrse_t4_6a),
          omit              = c("factor", "time.id", "Constant"),
          header            = FALSE,
          model.names       = FALSE,
          covariate.labels  = t4_labs_a,
          dep.var.caption   = "Panel A: Ln persons tried",
          table.layout      = "=!l#-t-as-!", # Custom table formatting
          float             = FALSE,
          add.lines         = list(c("Sample", 
                                     "1500-1699", "1500-1699", "1500-1549",
                                     "1500-1549", "1500-1699", "1500-1699"),
                                   c("Period/Country Fixed Effects",
                                     "No/No", "Yes/No", "No/No",
                                     "Yes/No", "Yes/Yes", "Yes/Yes")),
          omit.stat         = c("adj.rsq", "ser", "f"),
          type              = "text")

# Table 4: Religious-Market Contestation and Witch Trials (Panel B) -----------

# OLS (1500-1699)
t4_1b <- "ln.trials.mil ~ battles.mil"
# OLS (1500-1699) + period FE
t4_2b <- "ln.trials.mil ~ battles.mil + factor(decade)"
# OLS (1500-1549)
t4_3b <- "ln.trials.mil ~ battles.mil"
# OLS (1500-1549) + period FE
t4_4b <- "ln.trials.mil ~ battles.mil + factor(decade)"
# OLS + country-specific time trends
t4_5b <- "ln.trials.mil ~ battles.mil + factor(country) + factor(decade)"
# Placebo test regression (future battles) 
t4_6b <- "ln.trials.mil ~ battles.mil + battles.mil.tp1 + battles.mil.tp2 + factor(country) + factor(decade)"

# Run each model

results_t4_1b <- lm(formula = t4_1b, data = table_4)
results_t4_2b <- lm(formula = t4_2b, data = table_4)
results_t4_3b <- lm(formula = t4_3b, data = filter(table_4, decade < 1550))
results_t4_4b <- lm(formula = t4_4b, data = filter(table_4, decade < 1550))
results_t4_5b <- lm(formula = t4_5b, data = table_4)
results_t4_6b <- lm(formula = t4_6b, data = table_4)

# Use cluster-robust standard errors 

clrse_t4_1b <- results_t4_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_2b <- results_t4_2b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_3b <- results_t4_3b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_4b <- results_t4_4b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_5b <- results_t4_5b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t4_6b <- results_t4_6b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

t4_labs_b <- c("Confessional battles per million", 
               "Confessional battles per million (t + 1)",
               "Confessional battles per million (t + 2)")

stargazer(results_t4_1b, results_t4_2b, results_t4_3b, 
          results_t4_4b, results_t4_5b, results_t4_6b,
          se                = list(clrse_t4_1b, clrse_t4_2b, 
                                   clrse_t4_3b, clrse_t4_4b,
                                   clrse_t4_5b, clrse_t4_6b),
          omit              = c("factor", "time.id", "Constant"),
          header            = FALSE,
          model.names       = FALSE,
          covariate.labels  = t4_labs_b,
          dep.var.caption   = "Panel B: Ln persons tried per million",
          table.layout      = "=!l#-t-as-!", # Custom table formatting
          float             = FALSE,
          add.lines         = list(c("Sample", 
                                     "1500-1699", "1500-1699", "1500-1549",
                                     "1500-1549", "1500-1699", "1500-1699"),
                                   c("Period/Country Fixed Effects",
                                     "No/No", "Yes/No", "No/No",
                                     "Yes/No", "Yes/Yes", "Yes/Yes")),
          omit.stat         = c("adj.rsq", "ser", "f"),
          type              = "text")

# Table 5: Using Grid Cells ---------------------------------------------------

# OLS (1500-1699)
t5_1  <- "ln.trials ~ battles"
# OLS (1500-1699) + period FE
t5_2  <- "ln.trials ~ battles + factor(decade)"
# OLS, 1500 - 1549
t5_3  <- "ln.trials ~ battles"
# OLS, add period FE, 1500 - 1549
t5_4  <- "ln.trials ~ battles + factor(decade)"
# Add country-specific time trends (keep FEs)
t5_5  <- "ln.trials ~ battles + factor(grid.id) + factor(decade)"
# Placebo test regression (add future battles) 
t5_6  <- "ln.trials ~ battles + battles.tp1 + battles.tp2 + factor(grid.id) + factor(decade)"

# Run each model

results_t5_1 <- lm(formula = t5_1, data = table_5)
results_t5_2 <- lm(formula = t5_2, data = table_5)
results_t5_3 <- lm(formula = t5_3, data = filter(table_5, decade < 1550))
results_t5_4 <- lm(formula = t5_4, data = filter(table_5, decade < 1550))
results_t5_5 <- lm(formula = t5_5, data = table_5)
results_t5_6 <- lm(formula = t5_6, data = table_5)

# Use cluster-robust standard errors

clrse_t5_1 <- results_t5_1  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t5_2 <- results_t5_2 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t5_3 <- results_t5_3 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t5_4 <- results_t5_4 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t5_5 <- results_t5_5 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t5_6 <- results_t5_6 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t5_labs <- c("Confessional battles", 
             "Confessional battles (t + 1)",
             "Confessional battles (t + 2)")

stargazer(results_t5_1, results_t5_2, results_t5_3, 
          results_t5_4, results_t5_5, results_t5_6,
          se               = list(clrse_t5_1, clrse_t5_2, 
                                  clrse_t5_3, clrse_t5_4, 
                                  clrse_t5_5, clrse_t5_6),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t5_labs,
          dep.var.caption  = "Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          add.lines        = list(c("Sample", 
                                    "1500-1699", "1500-1699", "1500-1549", 
                                    "1500-1549", "1500-1699", "1500-1699"),
                                  c("Period/Grid-Cell Fixed Effects",
                                    "No/No", "Yes/No", "No/No",
                                    "Yes/No", "Yes/Yes", "Yes/Yes")),
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table 6: Weather and Witch Trials (Panel A) ---------------------------------

# Models with Oster Seven sample
t6_1a <- "ln.trials ~ weather + factor(decade)"
t6_2a <- "ln.trials ~ battles + factor(decade)"
t6_3a <- "ln.trials ~ weather + battles + factor(decade)"

# Models with Oster + Germany
t6_4a <- "ln.trials ~ weather + factor(decade)"
t6_5a <- "ln.trials ~ battles + factor(decade)"
t6_6a <- "ln.trials ~ weather + battles + factor(decade)"

# Subset Oster countries
oster_seven <- table_6 %>% filter(!(country %in% "Germany"))

# Run each model

results_t6_1a <- lm(formula = t6_1a, data = oster_seven)
results_t6_2a <- lm(formula = t6_2a, data = oster_seven)
results_t6_3a <- lm(formula = t6_3a, data = oster_seven)
results_t6_4a <- lm(formula = t6_4a, data = table_6)
results_t6_5a <- lm(formula = t6_5a, data = table_6)
results_t6_6a <- lm(formula = t6_6a, data = table_6)

# Use cluster-robust standard errors

clrse_t6_1a <- results_t6_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_2a <- results_t6_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_3a <- results_t6_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_4a <- results_t6_4a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_5a <- results_t6_5a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_6a <- results_t6_6a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t6_labs_a <- c("Weather", "Confessional battles")

stargazer(results_t6_1a, results_t6_2a, results_t6_3a, 
          results_t6_4a, results_t6_5a, results_t6_6a,
          se               = list(clrse_t6_1a, clrse_t6_2a, clrse_t6_3a, 
                                  clrse_t6_4a, clrse_t6_5a, clrse_t6_6a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t6_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          add.lines        = list(c("Sample", "Oster", "Oster", "Oster",
                                    "Oster + Germany", "Oster + Germany", 
                                    "Oster + Germany")),
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table 6: Weather and Witch Trials (Panel B) ---------------------------------

# Models with Oster Seven sample
t6_1b <- "ln.trials.mil ~ weather + factor(decade)"
t6_2b <- "ln.trials.mil ~ battles.mil + factor(decade)"
t6_3b <- "ln.trials.mil ~ weather + battles.mil + factor(decade)"
# Models with Oster + Germany
t6_4b <- "ln.trials.mil ~ weather + factor(decade)"
t6_5b <- "ln.trials.mil ~ battles.mil + factor(decade)"
t6_6b <- "ln.trials.mil ~ weather + battles.mil + factor(decade)"

# Run each model

results_t6_1b <- lm(formula = t6_1b, data = oster_seven)
results_t6_2b <- lm(formula = t6_2b, data = oster_seven)
results_t6_3b <- lm(formula = t6_3b, data = oster_seven)
results_t6_4b <- lm(formula = t6_4b, data = table_6)
results_t6_5b <- lm(formula = t6_5b, data = table_6)
results_t6_6b <- lm(formula = t6_6b, data = table_6)

# Use cluster-robust standard errors

clrse_t6_1b <- results_t6_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_2b <- results_t6_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_3b <- results_t6_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_4b <- results_t6_4b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_5b <- results_t6_5b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t6_6b <- results_t6_6b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t6_labs_b <- c("Weather", "Confessional battles per million")

stargazer(results_t6_1b, results_t6_2b, results_t6_3b, 
          results_t6_4b, results_t6_5b, results_t6_6b,
          se               = list(clrse_t6_1b, clrse_t6_2b, clrse_t6_3b, 
                                  clrse_t6_4b, clrse_t6_5b, clrse_t6_6b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t6_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          add.lines        = list(c("Sample", "Oster", "Oster", "Oster",
                                    "Oster + Germany", "Oster + Germany", 
                                    "Oster + Germany")),
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table 7: Income and Witch Trials (Panel A) ----------------------------------

t7_1a <- "ln.trials ~ urbanization + factor(decade)"
t7_2a <- "ln.trials ~ battles + factor(decade)"
t7_3a <- "ln.trials ~ urbanization + battles + factor(decade)"
t7_4a <- "ln.trials ~ real.wage + factor(decade)"
t7_5a <- "ln.trials ~ battles + factor(decade)"
t7_6a <- "ln.trials ~ real.wage + battles + factor(decade)"

# Subset for real wage observations
real_wage_obs <- table_7 %>% filter(!is.na(real.wage))

# Run each model

results_t7_1a <- lm(formula = t7_1a, data = table_7)
results_t7_2a <- lm(formula = t7_2a, data = table_7)
results_t7_3a <- lm(formula = t7_3a, data = table_7)
results_t7_4a <- lm(formula = t7_4a, data = real_wage_obs)
results_t7_5a <- lm(formula = t7_5a, data = real_wage_obs)
results_t7_6a <- lm(formula = t7_6a, data = real_wage_obs)

# Use cluster-robust standard errors

clrse_t7_1a <- results_t7_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_2a <- results_t7_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_3a <- results_t7_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_4a <- results_t7_4a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_5a <- results_t7_5a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_6a <- results_t7_6a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t7_labs_a <- c("Urbanization", "Confessional battles", "Real wage")

stargazer(results_t7_1a, results_t7_2a, results_t7_3a, 
          results_t7_4a, results_t7_5a, results_t7_6a,
          se               = list(clrse_t7_1a, clrse_t7_2a, 
                                  clrse_t7_3a, clrse_t7_4a,
                                  clrse_t7_5a, clrse_t7_6a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t7_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"), 
          type             = "text")

# Table 7: Income and Witch Trials (Panel B) ----------------------------------

t7_1b <- "ln.trials.mil ~ urbanization + factor(decade)"
t7_2b <- "ln.trials.mil ~ battles.mil  + factor(decade)"
t7_3b <- "ln.trials.mil ~ urbanization + battles.mil + factor(decade)"
t7_4b <- "ln.trials.mil ~ real.wage + factor(decade)"
t7_5b <- "ln.trials.mil ~ battles.mil  + factor(decade)"
t7_6b <- "ln.trials.mil ~ real.wage + battles.mil + factor(decade)"

# Run each model

results_t7_1b <- lm(formula = t7_1b, data = table_7)
results_t7_2b <- lm(formula = t7_2b, data = table_7)
results_t7_3b <- lm(formula = t7_3b, data = table_7)
results_t7_4b <- lm(formula = t7_4b, data = real_wage_obs)
results_t7_5b <- lm(formula = t7_5b, data = real_wage_obs)
results_t7_6b <- lm(formula = t7_6b, data = real_wage_obs)

# Use cluster-robust standard errors

clrse_t7_1b <- results_t7_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_2b <- results_t7_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_3b <- results_t7_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_4b <- results_t7_4b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_5b <- results_t7_5b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t7_6b <- results_t7_6b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t7_labs_b <- c("Urbanization", "Confessional battles per million", "Real wage")

stargazer(results_t7_1b, results_t7_2b, results_t7_3b, 
          results_t7_4b, results_t7_5b, results_t7_6b,
          se               = list(clrse_t7_1b, clrse_t7_2b, 
                                  clrse_t7_3b, clrse_t7_4b,
                                  clrse_t7_5b, clrse_t7_6b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t7_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"), 
          type             = "text")

# Table 8: State Capacity and Witch Trials (Panel A) --------------------------

t8_1a <- "ln.trials ~ taxes.percap + factor(decade)"
t8_2a <- "ln.trials ~ battles + factor(decade)"
t8_3a <- "ln.trials ~ taxes.percap + battles + factor(decade)"

# Run each model

results_t8_1a <- lm(formula = t8_1a, data = table_8)
results_t8_2a <- lm(formula = t8_2a, data = table_8)
results_t8_3a <- lm(formula = t8_3a, data = table_8)

# Use cluster-robust standard errors

clrse_t8_1a <- results_t8_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t8_2a <- results_t8_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t8_3a <- results_t8_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t8_labs_a <- c("Tax revenue per capita", "Confessional battles")

stargazer(results_t8_1a, results_t8_2a, results_t8_3a,
          se               = list(clrse_t8_1a, clrse_t8_2a, clrse_t8_3a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t8_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table 8: State Capacity and Witch Trials (Panel B) --------------------------

t8_1b <- "ln.trials.mil ~ taxes.percap + factor(decade)"
t8_2b <- "ln.trials.mil ~ battles.mil  + factor(decade)"
t8_3b <- "ln.trials.mil ~ taxes.percap + battles.mil + factor(decade)"

# Run the models -------------------------------------------------------------

results_t8_1b <- lm(formula = t8_1b, data = table_8)
results_t8_2b <- lm(formula = t8_2b, data = table_8)
results_t8_3b <- lm(formula = t8_3b, data = table_8)

# Use cluster-robust standard errors

clrse_t8_1b <- results_t8_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t8_2b <- results_t8_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t8_3b <- results_t8_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

t8_labs_b <- c("Tax revenue per capita", "Confessional battles per million")

stargazer(results_t8_1b, results_t8_2b, results_t8_3b,
          se               = list(clrse_t8_1b, clrse_t8_2b, clrse_t8_3b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = t8_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table 9: Horse Race (Panel A) -----------------------------------------------

t9_1a <- "ln.trials ~ battles + weather + real.wage + taxes.percap + factor(decade)"
t9_2a <- "ln.trials ~ battles + weather + taxes.percap + urbanization + factor(decade)"

# Run each model

results_t9_1a <- lm(formula = t9_1a, data = table_9)
results_t9_2a <- lm(formula = t9_2a, data = table_9)

# Use cluster-robust standard errors

clrse_t9_1a <- results_t9_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t9_2a <- results_t9_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

t9_labs_a <- c("Confessional battles",
              "Weather",
              "Real wage",
              "Tax revenue per capita", 
              "Urbanization")

stargazer(results_t9_1a, results_t9_2a,
          se                     = list(clrse_t9_1a, clrse_t9_2a),
          omit                   = c("factor", "time.id", "Constant"),
          header                 = FALSE,
          model.names            = FALSE,
          covariate.labels       = t9_labs_a,
          dep.var.caption        = "Panel A: Ln persons tried",
          table.layout           = "=!l#-t-as-!", # Custom table formatting
          float                  = FALSE,
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table 9: Horse Race (Panel B) -----------------------------------------------

t9_1b <- "ln.trials.mil ~ battles.mil + weather + real.wage + taxes.percap + factor(decade)"
t9_2b <- "ln.trials.mil ~ battles.mil + weather + taxes.percap + urbanization + factor(decade)"

# Run each model

results_t9_1b <- lm(formula = t9_1b, data = table_9)
results_t9_2b <- lm(formula = t9_2b, data = table_9)

# Use cluster-robust standard errors

clrse_t9_1b <- results_t9_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_t9_2b <- results_t9_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

t9_labs_b <- c("Confessional battles per million",
               "Weather",
               "Real wage",
               "Tax revenue per capita", 
               "Urbanization")

stargazer(results_t9_1b, results_t9_2b, 
          se                     = list(clrse_t9_1b, clrse_t9_2b),
          omit                   = c("factor", "time.id", "Constant"),
          header                 = FALSE,
          model.names            = FALSE,
          covariate.labels       = t9_labs_a,
          dep.var.caption        = "Panel B: Ln persons tried per million",
          table.layout           = "=!l#-t-as-!", # Custom table formatting
          float                  = FALSE,
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")


# Table 10: Altonji-Elder-Taber Results ---------------------------------------

# Row 1: OLS vs OLS + weather (Oster)

aet_ols_bat_ost <- "ln.trials ~ battles" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_wth_ost <- "ln.trials ~ battles + weather" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_bat_ost_pm <- "ln.trials.mil ~ battles.mil" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_ols_wth_ost_pm <- "ln.trials.mil ~ battles.mil + weather" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row1_ratio_bat <- aet_ols_wth_ost$estimate[1] / (aet_ols_bat_ost$estimate[1] - aet_ols_wth_ost$estimate[1])

row1_ratio_bat_pm <- aet_ols_wth_ost_pm$estimate[1] / (aet_ols_bat_ost_pm$estimate[1] - aet_ols_wth_ost_pm$estimate[1])

# Row 2: Period FE vs PFE + weather (Oster)

aet_pfe_bat_ost <- "ln.trials ~ battles + factor(decade)" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_wth_ost <- "ln.trials ~ battles + weather + factor(decade)" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_bat_ost_pm <- "ln.trials.mil ~ battles.mil + factor(decade)" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_pfe_wth_ost_pm <- "ln.trials.mil ~ battles.mil + weather + factor(decade)" %>%
  lm(data = oster_seven) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row2_ratio_bat <- aet_pfe_wth_ost$estimate[1] / (aet_pfe_bat_ost$estimate[1] - aet_pfe_wth_ost$estimate[1])

row2_ratio_bat_pm <- aet_pfe_wth_ost_pm$estimate[1] / (aet_pfe_bat_ost_pm$estimate[1] - aet_pfe_wth_ost_pm$estimate[1])

# Row 3: OLS vs OLS + weather (Oster + Germany)

aet_ols_bat_ger <- "ln.trials ~ battles" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_wth_ger <- "ln.trials ~ battles + weather" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_bat_ger_pm <- "ln.trials.mil ~ battles.mil" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_ols_wth_ger_pm <- "ln.trials.mil ~ battles.mil + weather" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row3_ratio_bat <- aet_ols_wth_ger$estimate[1] / (aet_ols_bat_ger$estimate[1] - aet_ols_wth_ger$estimate[1])

row3_ratio_bat_pm <- aet_ols_wth_ger_pm$estimate[1] / (aet_ols_bat_ger_pm$estimate[1] - aet_ols_wth_ger_pm$estimate[1])

# PFE vs PFE + weather (Oster + Germany)

aet_pfe_bat_ger <- "ln.trials ~ battles + factor(decade)" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_wth_ger <- "ln.trials ~ battles + weather + factor(decade)" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_bat_ger_pm <- "ln.trials.mil ~ battles.mil + factor(decade)" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_pfe_wth_ger_pm <- "ln.trials.mil ~ battles.mil + weather + factor(decade)" %>%
  lm(data = table_6) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row4_ratio_bat <- aet_pfe_wth_ger$estimate[1] / (aet_pfe_bat_ger$estimate[1] - aet_pfe_wth_ger$estimate[1])

row4_ratio_bat_pm <- aet_pfe_wth_ger_pm$estimate[1] / (aet_pfe_bat_ger_pm$estimate[1] - aet_pfe_wth_ger_pm$estimate[1])

# Row 5: OLS vs OLS + urbanization

aet_ols_bat_urb <- "ln.trials ~ battles" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_inc_urb <- "ln.trials ~ battles + urbanization" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_bat_urb_pm <- "ln.trials.mil ~ battles.mil" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_ols_inc_urb_pm <- "ln.trials.mil ~ battles.mil + urbanization" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row5_ratio_bat <- aet_ols_inc_urb$estimate[1] / (aet_ols_bat_urb$estimate[1] - aet_ols_inc_urb$estimate[1])

row5_ratio_bat_pm <- aet_ols_inc_urb_pm$estimate[1] / (aet_ols_bat_urb_pm$estimate[1] - aet_ols_inc_urb_pm$estimate[1])

# Row 6: PFE vs PFE + urbanization

aet_pfe_bat_urb <- "ln.trials ~ battles + factor(decade)" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_inc_urb <- "ln.trials ~ battles + urbanization + factor(decade)" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_bat_urb_pm <- "ln.trials.mil ~ battles.mil + factor(decade)" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_pfe_inc_urb_pm <- "ln.trials.mil ~ battles.mil + urbanization + factor(decade)" %>%
  lm(data = table_7) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row6_ratio_bat <- aet_pfe_inc_urb$estimate[1] / (aet_pfe_bat_urb$estimate[1] - aet_pfe_inc_urb$estimate[1])

row6_ratio_bat_pm <- aet_pfe_inc_urb_pm$estimate[1] / (aet_pfe_bat_urb_pm$estimate[1] - aet_pfe_inc_urb_pm$estimate[1])

# Row 7: OLS vs OLS + real wage

aet_ols_bat_wage <- "ln.trials ~ battles" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_inc_wage <- "ln.trials ~ battles + real.wage" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_bat_wage_pm <- "ln.trials.mil ~ battles.mil" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_ols_inc_wage_pm <- "ln.trials.mil ~ battles.mil + real.wage" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row7_ratio_bat <- aet_ols_inc_wage$estimate[1] / (aet_ols_bat_wage$estimate[1] - aet_ols_inc_wage$estimate[1])

row7_ratio_bat_pm <- aet_ols_inc_wage_pm$estimate[1] / (aet_ols_bat_wage_pm$estimate[1] - aet_ols_inc_wage_pm$estimate[1])

# Row 8: PFE vs PFE + real wage

aet_pfe_bat_wage <- "ln.trials ~ battles + factor(decade)" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_inc_wage <- "ln.trials ~ battles + real.wage + factor(decade)" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_bat_wage_pm <- "ln.trials.mil ~ battles.mil + factor(decade)" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_pfe_inc_wage_pm <- "ln.trials.mil ~ battles.mil + real.wage + factor(decade)" %>%
  lm(data = real_wage_obs) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row8_ratio_bat <- aet_pfe_inc_wage$estimate[1] / (aet_pfe_bat_wage$estimate[1] - aet_pfe_inc_wage$estimate[1])

row8_ratio_bat_pm <- aet_pfe_inc_wage_pm$estimate[1] / (aet_pfe_bat_wage_pm$estimate[1] - aet_pfe_inc_wage_pm$estimate[1])

# Row 9: OLS vs OLS + tax revenues

aet_ols_bat_tax <- "ln.trials ~ battles" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_tax <- "ln.trials ~ battles + taxes.percap" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_ols_bat_tax_pm <- "ln.trials.mil ~ battles.mil" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_ols_tax_pm <- "ln.trials.mil ~ battles.mil + taxes.percap" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row9_ratio_bat <- aet_ols_tax$estimate[1] / (aet_ols_bat_tax$estimate[1] - aet_ols_tax$estimate[1])

row9_ratio_bat_pm <- aet_ols_tax_pm$estimate[1] / (aet_ols_bat_tax_pm$estimate[1] - aet_ols_tax_pm$estimate[1])

# Row 10: PFE vs PFE + taxes

aet_pfe_bat_tax <- "ln.trials ~ battles + factor(decade)" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_tax <- "ln.trials ~ battles + taxes.percap + factor(decade)" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles")

aet_pfe_bat_tax_pm <- "ln.trials.mil ~ battles.mil + factor(decade)" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

aet_pfe_tax_pm <- "ln.trials.mil ~ battles.mil + taxes.percap + factor(decade)" %>%
  lm(data = table_8) %>% 
  tidy() %>% 
  filter(term %in% "battles.mil")

row10_ratio_bat <- aet_pfe_tax$estimate[1] / (aet_pfe_bat_tax$estimate[1] - aet_pfe_tax$estimate[1])

row10_ratio_bat_pm <- aet_pfe_tax_pm$estimate[1] / (aet_pfe_bat_tax_pm$estimate[1] - aet_pfe_tax_pm$estimate[1])

# Assemble Table 10
altonji_tbl <- 
  data_frame(restricted = c("None", "Period fixed effects", 
                            "None", "Period fixed effects",
                            "None", "Period fixed effects",
                            "None", "Period fixed effects",
                            "None", "Period fixed effects"),
             
             full       = c("Weather (Oster)",
                            "Period fixed effects + weather (Oster)",
                            "Weather (Oster + Germany)",
                            "Period fixed effects + weather (Oster + Germany)",
                            "Urbanization",
                            "Period fixed effects + urbanization",
                            "Real wage",
                            "Period fixed effects + real wage",
                            "Tax revenue per capita",
                            "Period fixed effects + tax revenue per capita"),
             
             bat.ratio     = c(row1_ratio_bat, row2_ratio_bat,
                               row3_ratio_bat, row4_ratio_bat, 
                               row5_ratio_bat, row6_ratio_bat, 
                               row7_ratio_bat, row8_ratio_bat, 
                               row9_ratio_bat, row10_ratio_bat),
             
             bat.mil.ratio = c(row1_ratio_bat_pm, row2_ratio_bat_pm,
                               row3_ratio_bat_pm, row4_ratio_bat_pm, 
                               row5_ratio_bat_pm, row6_ratio_bat_pm, 
                               row7_ratio_bat_pm, row8_ratio_bat_pm, 
                               row9_ratio_bat_pm, row10_ratio_bat_pm)) %>% 
  data.frame()

# Labels for table
altonji_labs <- c("Controls in restricted set", "Controls in full set", 
                  "Battles", "Battles per million", "Ratio")

stargazer(altonji_tbl,
          float            = FALSE, 
          rownames         = FALSE,
          summary          = FALSE,
          covariate.labels = altonji_labs,
          digits           = 1,
          type             = "text")
