# Title: ej-appendix-regressions.R
# Description: Replication file for tables C1 - D1 from "Witch Trials."
# Authors: Peter T. Leeson and Jacob W. Russ

# Load R packages -------------------------------------------------------------

library(dplyr)
library(readxl)
library(magrittr)
library(stargazer)
library(sandwich)
library(broom)

# Import datasets -------------------------------------------------------------

tableC1 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC1", na = "NA")

tableC2 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC2", na = "NA")

tableC3 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC3", na = "NA")

tableC4 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC4", na = "NA")

tableC5 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC5", na = "NA")

tableC6 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableC6", na = "NA")

tableD1 <- "data/publication/Leeson-Russ.WitchTrials.Dataset.xlsx" %>%
  read_excel(sheet = "TableD1", na = "NA")

# Table C1: Religious-Market Contestation and Witch Trials (Panel A) ----------

# OLS (1500-1699)
C1_1a <- "ln1p.trials ~ battles"
# OLS (1500-1699) + period FE
C1_2a <- "ln1p.trials ~ battles + factor(decade)"
# OLS (1500-1549)
C1_3a <- "ln1p.trials ~ battles"
# OLS (1500-1549) + period FE
C1_4a <- "ln1p.trials ~ battles + factor(decade)"
# OLS + country-specific time trends
C1_5a <- "ln1p.trials ~ battles + factor(country) + factor(decade)"
# Placebo test regression (future battles) 
C1_6a <- "ln1p.trials ~ battles + battles.tp1 + battles.tp2 + factor(country) + factor(decade)"

# Run each model
results_C1_1a <- lm(formula = C1_1a, data = tableC1)
results_C1_2a <- lm(formula = C1_2a, data = tableC1)
results_C1_3a <- lm(formula = C1_3a, data = filter(tableC1, decade < 1550))
results_C1_4a <- lm(formula = C1_4a, data = filter(tableC1, decade < 1550))
results_C1_5a <- lm(formula = C1_5a, data = tableC1)
results_C1_6a <- lm(formula = C1_6a, data = tableC1)

# Use cluster-robust standard errors 

clrse_C1_1a <- results_C1_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_2a <- results_C1_2a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_3a <- results_C1_3a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_4a <- results_C1_4a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_5a <- results_C1_5a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_6a <- results_C1_6a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

C1_labs_a <- c("Confessional battles", 
               "Confessional battles (t + 1)",
               "Confessional battles (t + 2)")

stargazer(results_C1_1a, results_C1_2a, results_C1_3a, 
          results_C1_4a, results_C1_5a, results_C1_6a,
          se                = list(clrse_C1_1a, clrse_C1_2a, 
                                   clrse_C1_3a, clrse_C1_4a,
                                   clrse_C1_5a, clrse_C1_6a),
          omit              = c("factor", "time.id", "Constant"),
          header            = FALSE,
          model.names       = FALSE,
          covariate.labels  = C1_labs_a,
          dep.var.caption   = "Panel A: Ln persons tried",
          table.layout      = "=!l#-t-as-!", # Custom table formatting
          float             = FALSE,
          add.lines         = list(c("Sample", 
                                          "1500-1699", "1500-1699", "1500-1549",
                                          "1500-1549", "1500-1699", "1500-1699"),
                                        c("Period/Country Fixed Effects",
                                          "No/No", "Yes/No", "No/No",
                                          "Yes/No", "Yes/Yes", "Yes/Yes")),
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table C1: Religious-Market Contestation and Witch Trials (Panel B) ----------

# OLS (1500-1699)
C1_1b <- "ln1p.trials.mil ~ battles.mil"
# OLS (1500-1699) + period FE
C1_2b <- "ln1p.trials.mil ~ battles.mil + factor(decade)"
# OLS (1500-1549)
C1_3b <- "ln1p.trials.mil ~ battles.mil"
# OLS (1500-1549) + period FE
C1_4b <- "ln1p.trials.mil ~ battles.mil + factor(decade)"
# OLS + country-specific time trends
C1_5b <- "ln1p.trials.mil ~ battles.mil + factor(country) + factor(decade)"
# Placebo test regression (future battles) 
C1_6b <- "ln1p.trials.mil ~ battles.mil + battles.mil.tp1 + battles.mil.tp2 + factor(country) + factor(decade)"

# Run each model

results_C1_1b <- lm(formula = C1_1b, data = tableC1)
results_C1_2b <- lm(formula = C1_2b, data = tableC1)
results_C1_3b <- lm(formula = C1_3b, data = filter(tableC1, decade < 1550))
results_C1_4b <- lm(formula = C1_4b, data = filter(tableC1, decade < 1550))
results_C1_5b <- lm(formula = C1_5b, data = tableC1)
results_C1_6b <- lm(formula = C1_6b, data = tableC1)

# Use cluster-robust standard errors 

clrse_C1_1b <- results_C1_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_2b <- results_C1_2b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_3b <- results_C1_3b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_4b <- results_C1_4b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_5b <- results_C1_5b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C1_6b <- results_C1_6b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

C1_labs_b <- c("Confessional battles per million", 
               "Confessional battles per million (t + 1)",
               "Confessional battles per million (t + 2)")

stargazer(results_C1_1b, results_C1_2b, results_C1_3b, 
          results_C1_4b, results_C1_5b, results_C1_6b,
          se                = list(clrse_C1_1b, clrse_C1_2b, 
                                   clrse_C1_3b, clrse_C1_4b,
                                   clrse_C1_5b, clrse_C1_6b),
          omit              = c("factor", "time.id", "Constant"),
          header            = FALSE,
          model.names       = FALSE,
          covariate.labels  = C1_labs_b,
          dep.var.caption   = "Panel B: Ln persons tried per million",
          table.layout      = "=!l#-t-as-!", # Custom table formatting
          float             = FALSE,
          add.lines         = list(c("Sample", 
                                     "1500-1699", "1500-1699", "1500-1549",
                                     "1500-1549", "1500-1699", "1500-1699"),
                                   c("Period/Country Fixed Effects",
                                     "No/No", "Yes/No", "No/No",
                                     "Yes/No", "Yes/Yes", "Yes/Yes")),
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table C2: Using Grid Cells --------------------------------------------------

# OLS (1500-1699)
C2_1  <- "ln1p.trials ~ battles"
# OLS (1500-1699) + period FE
C2_2  <- "ln1p.trials ~ battles + factor(decade)"
# OLS, 1500 - 1549
C2_3  <- "ln1p.trials ~ battles"
# OLS, add period FE, 1500 - 1549
C2_4  <- "ln1p.trials ~ battles + factor(decade)"
# Add country-specific time trends (keep FEs)
C2_5  <- "ln1p.trials ~ battles + factor(grid.id) + factor(decade)"
# Placebo test regression (add future battles) 
C2_6  <- "ln1p.trials ~ battles + battles.tp1 + battles.tp2 + factor(grid.id) + factor(decade)"

# Run each model

results_C2_1 <- lm(formula = C2_1, data = tableC2)
results_C2_2 <- lm(formula = C2_2, data = tableC2)
results_C2_3 <- lm(formula = C2_3, data = filter(tableC2, decade < 1550))
results_C2_4 <- lm(formula = C2_4, data = filter(tableC2, decade < 1550))
results_C2_5 <- lm(formula = C2_5, data = tableC2)
results_C2_6 <- lm(formula = C2_6, data = tableC2)

# Use cluster-robust standard errors

clrse_C2_1 <- results_C2_1  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C2_2 <- results_C2_2 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C2_3 <- results_C2_3 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C2_4 <- results_C2_4 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C2_5 <- results_C2_5 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C2_6 <- results_C2_6 %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C2_labs <- c("Confessional battles", 
             "Confessional battles (t + 1)",
             "Confessional battles (t + 2)")

stargazer(results_C2_1, results_C2_2, results_C2_3, 
          results_C2_4, results_C2_5, results_C2_6,
          se               = list(clrse_C2_1, clrse_C2_2, 
                                  clrse_C2_3, clrse_C2_4, 
                                  clrse_C2_5, clrse_C2_6),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C2_labs,
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

# Table C3: Weather and Witch Trials (Panel A) --------------------------------

# Models with Oster Seven sample
C3_1a <- "ln1p.trials ~ weather + factor(decade)"
C3_2a <- "ln1p.trials ~ battles + factor(decade)"
C3_3a <- "ln1p.trials ~ weather + battles + factor(decade)"

# Models with Oster + Germany
C3_4a <- "ln1p.trials ~ weather + factor(decade)"
C3_5a <- "ln1p.trials ~ battles + factor(decade)"
C3_6a <- "ln1p.trials ~ weather + battles + factor(decade)"

# Subset Oster countries
oster_seven <- tableC3 %>% filter(!(country %in% "Germany"))

# Run each model

results_C3_1a <- lm(formula = C3_1a, data = oster_seven)
results_C3_2a <- lm(formula = C3_2a, data = oster_seven)
results_C3_3a <- lm(formula = C3_3a, data = oster_seven)
results_C3_4a <- lm(formula = C3_4a, data = tableC3)
results_C3_5a <- lm(formula = C3_5a, data = tableC3)
results_C3_6a <- lm(formula = C3_6a, data = tableC3)

# Use cluster-robust standard errors

clrse_C3_1a <- results_C3_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_2a <- results_C3_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_3a <- results_C3_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_4a <- results_C3_4a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_5a <- results_C3_5a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_6a <- results_C3_6a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C3_labs_a <- c("Weather", "Confessional battles")

stargazer(results_C3_1a, results_C3_2a, results_C3_3a, 
          results_C3_4a, results_C3_5a, results_C3_6a,
          se               = list(clrse_C3_1a, clrse_C3_2a, clrse_C3_3a, 
                                  clrse_C3_4a, clrse_C3_5a, clrse_C3_6a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C3_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          add.lines        = list(c("Sample", "Oster", "Oster", "Oster",
                                    "Oster + Germany", "Oster + Germany", 
                                    "Oster + Germany")),
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table C3: Weather and Witch Trials (Panel B) --------------------------------

# Models with Oster Seven sample
C3_1b <- "ln1p.trials.mil ~ weather + factor(decade)"
C3_2b <- "ln1p.trials.mil ~ battles.mil + factor(decade)"
C3_3b <- "ln1p.trials.mil ~ weather + battles.mil + factor(decade)"
# Models with Oster + Germany
C3_4b <- "ln1p.trials.mil ~ weather + factor(decade)"
C3_5b <- "ln1p.trials.mil ~ battles.mil + factor(decade)"
C3_6b <- "ln1p.trials.mil ~ weather + battles.mil + factor(decade)"

# Run each model

results_C3_1b <- lm(formula = C3_1b, data = oster_seven)
results_C3_2b <- lm(formula = C3_2b, data = oster_seven)
results_C3_3b <- lm(formula = C3_3b, data = oster_seven)
results_C3_4b <- lm(formula = C3_4b, data = tableC3)
results_C3_5b <- lm(formula = C3_5b, data = tableC3)
results_C3_6b <- lm(formula = C3_6b, data = tableC3)

# Use cluster-robust standard errors

clrse_C3_1b <- results_C3_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_2b <- results_C3_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_3b <- results_C3_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_4b <- results_C3_4b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_5b <- results_C3_5b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C3_6b <- results_C3_6b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C3_labs_b <- c("Weather", "Confessional battles per million")

stargazer(results_C3_1b, results_C3_2b, results_C3_3b, 
          results_C3_4b, results_C3_5b, results_C3_6b,
          se               = list(clrse_C3_1b, clrse_C3_2b, clrse_C3_3b, 
                                  clrse_C3_4b, clrse_C3_5b, clrse_C3_6b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C3_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          add.lines        = list(c("Sample", "Oster", "Oster", "Oster",
                                    "Oster + Germany", "Oster + Germany", 
                                    "Oster + Germany")),
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table C4: Income and Witch Trials (Panel A) ---------------------------------

C4_1a <- "ln1p.trials ~ urbanization + factor(decade)"
C4_2a <- "ln1p.trials ~ battles + factor(decade)"
C4_3a <- "ln1p.trials ~ urbanization + battles + factor(decade)"
C4_4a <- "ln1p.trials ~ real.wage + factor(decade)"
C4_5a <- "ln1p.trials ~ battles + factor(decade)"
C4_6a <- "ln1p.trials ~ real.wage + battles + factor(decade)"

# Subset for real wage observations
real_wage_obs <- tableC4 %>% filter(!is.na(real.wage))

# Run each model

results_C4_1a <- lm(formula = C4_1a, data = tableC4)
results_C4_2a <- lm(formula = C4_2a, data = tableC4)
results_C4_3a <- lm(formula = C4_3a, data = tableC4)
results_C4_4a <- lm(formula = C4_4a, data = real_wage_obs)
results_C4_5a <- lm(formula = C4_5a, data = real_wage_obs)
results_C4_6a <- lm(formula = C4_6a, data = real_wage_obs)

# Use cluster-robust standard errors

clrse_C4_1a <- results_C4_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_2a <- results_C4_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_3a <- results_C4_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_4a <- results_C4_4a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_5a <- results_C4_5a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_6a <- results_C4_6a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C4_labs_a <- c("Urbanization", "Confessional battles", "Real wage")

stargazer(results_C4_1a, results_C4_2a, results_C4_3a, 
          results_C4_4a, results_C4_5a, results_C4_6a,
          se               = list(clrse_C4_1a, clrse_C4_2a, 
                                  clrse_C4_3a, clrse_C4_4a,
                                  clrse_C4_5a, clrse_C4_6a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C4_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"), 
          type             = "text")

# Table C4: Income and Witch Trials (Panel B) ---------------------------------

C4_1b <- "ln1p.trials.mil ~ urbanization + factor(decade)"
C4_2b <- "ln1p.trials.mil ~ battles.mil  + factor(decade)"
C4_3b <- "ln1p.trials.mil ~ urbanization + battles.mil + factor(decade)"
C4_4b <- "ln1p.trials.mil ~ real.wage + factor(decade)"
C4_5b <- "ln1p.trials.mil ~ battles.mil  + factor(decade)"
C4_6b <- "ln1p.trials.mil ~ real.wage + battles.mil + factor(decade)"

# Run each model

results_C4_1b <- lm(formula = C4_1b, data = tableC4)
results_C4_2b <- lm(formula = C4_2b, data = tableC4)
results_C4_3b <- lm(formula = C4_3b, data = tableC4)
results_C4_4b <- lm(formula = C4_4b, data = real_wage_obs)
results_C4_5b <- lm(formula = C4_5b, data = real_wage_obs)
results_C4_6b <- lm(formula = C4_6b, data = real_wage_obs)

# Use cluster-robust standard errors

clrse_C4_1b <- results_C4_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_2b <- results_C4_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_3b <- results_C4_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_4b <- results_C4_4b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_5b <- results_C4_5b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C4_6b <- results_C4_6b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C4_labs_b <- c("Urbanization", "Confessional battles per million", "Real wage")

stargazer(results_C4_1b, results_C4_2b, results_C4_3b, 
          results_C4_4b, results_C4_5b, results_C4_6b,
          se               = list(clrse_C4_1b, clrse_C4_2b, 
                                  clrse_C4_3b, clrse_C4_4b,
                                  clrse_C4_5b, clrse_C4_6b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C4_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"), 
          type             = "text")

# Table C5: State Capacity and Witch Trials (Panel A) -------------------------

C5_1a <- "ln1p.trials ~ taxes.percap + factor(decade)"
C5_2a <- "ln1p.trials ~ battles + factor(decade)"
C5_3a <- "ln1p.trials ~ taxes.percap + battles + factor(decade)"

# Run each model

results_C5_1a <- lm(formula = C5_1a, data = tableC5)
results_C5_2a <- lm(formula = C5_2a, data = tableC5)
results_C5_3a <- lm(formula = C5_3a, data = tableC5)

# Use cluster-robust standard errors

clrse_C5_1a <- results_C5_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C5_2a <- results_C5_2a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C5_3a <- results_C5_3a %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C5_labs_a <- c("Tax revenue per capita", "Confessional battles")

stargazer(results_C5_1a, results_C5_2a, results_C5_3a,
          se               = list(clrse_C5_1a, clrse_C5_2a, clrse_C5_3a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C5_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table C5: State Capacity and Witch Trials (Panel B) -------------------------

C5_1b <- "ln1p.trials.mil ~ taxes.percap + factor(decade)"
C5_2b <- "ln1p.trials.mil ~ battles.mil  + factor(decade)"
C5_3b <- "ln1p.trials.mil ~ taxes.percap + battles.mil + factor(decade)"

# Run each model

results_C5_1b <- lm(formula = C5_1b, data = tableC5)
results_C5_2b <- lm(formula = C5_2b, data = tableC5)
results_C5_3b <- lm(formula = C5_3b, data = tableC5)

# Use cluster-robust standard errors

clrse_C5_1b <- results_C5_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C5_2b <- results_C5_2b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C5_3b <- results_C5_3b %>% 
  vcovHC(type = "HC1", cluster = "group", adjust  = TRUE) %>% 
  diag() %>% 
  sqrt()

C5_labs_b <- c("Tax revenue per capita", "Confessional battles per million")

stargazer(results_C5_1b, results_C5_2b, results_C5_3b,
          se               = list(clrse_C5_1b, clrse_C5_2b, clrse_C5_3b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = C5_labs_b,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table C6: Horse Race (Panel A) ----------------------------------------------

C6_1a <- "ln1p.trials ~ battles + weather + real.wage + taxes.percap + factor(decade)"
C6_2a <- "ln1p.trials ~ battles + weather + taxes.percap + urbanization + factor(decade)"

# Run each model

results_C6_1a <- lm(formula = C6_1a, data = tableC6)
results_C6_2a <- lm(formula = C6_2a, data = tableC6)

# Use cluster-robust standard errors

clrse_C6_1a <- results_C6_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C6_2a <- results_C6_2a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

C6_labs_a <- c("Confessional battles",
              "Weather",
              "Real wage",
              "Tax revenue per capita", 
              "Urbanization")

stargazer(results_C6_1a, results_C6_2a,
          se                     = list(clrse_C6_1a, clrse_C6_2a),
          omit                   = c("factor", "time.id", "Constant"),
          header                 = FALSE,
          model.names            = FALSE,
          covariate.labels       = C6_labs_a,
          dep.var.caption        = "Panel A: Ln persons tried",
          table.layout           = "=!l#-t-as-!", # Custom table formatting
          float                  = FALSE,
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table C6: Horse Race (Panel B) ----------------------------------------------

C6_1b <- "ln1p.trials.mil ~ battles.mil + weather + real.wage + taxes.percap + factor(decade)"
C6_2b <- "ln1p.trials.mil ~ battles.mil + weather + taxes.percap + urbanization + factor(decade)"

# Run each model

results_C6_1b <- lm(formula = C6_1b, data = tableC6)
results_C6_2b <- lm(formula = C6_2b, data = tableC6)

# Use cluster-robust standard errors

clrse_C6_1b <- results_C6_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_C6_2b <- results_C6_2b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

C6_labs_b <- c("Confessional battles per million",
               "Weather",
               "Real wage",
               "Tax revenue per capita", 
               "Urbanization")

stargazer(results_C6_1b, results_C6_2b, 
          se                     = list(clrse_C6_1b, clrse_C6_2b),
          omit                   = c("factor", "time.id", "Constant"),
          header                 = FALSE,
          model.names            = FALSE,
          covariate.labels       = C6_labs_b,
          dep.var.caption        = "Panel B: Ln persons tried per million",
          table.layout           = "=!l#-t-as-!", # Custom table formatting
          float                  = FALSE,
          omit.stat              = c("adj.rsq", "ser", "f"),
          type                   = "text")

# Table D1: Using Luterbacher et al. Weather Data (Panel A) -------------------

D1_1a  <- "ln.trials ~ temperature + factor(decade)"
D1_2a  <- "ln.trials ~ temperature + battles + factor(decade)"

# Run each model

results_D1_1a <- lm(formula = D1_1a, data = tableD1)
results_D1_2a <- lm(formula = D1_2a, data = tableD1)

# Use cluster-robust standard errors

clrse_D1_1a <- results_D1_1a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_D1_2a <- results_D1_2a  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

D1_labs_a <- c("Temperature", "Confessional battles")

stargazer(results_D1_1a, results_D1_2a,
          se               = list(clrse_D1_1a, clrse_D1_2a),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = D1_labs_a,
          dep.var.caption  = "Panel A: Ln persons tried",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")

# Table D1: Using Luterbacher et al. Weather Data (Panel B) -------------------

D1_1b  <- "ln.trials.mil ~ temperature + factor(decade)"
D1_2b  <- "ln.trials.mil ~ temperature + battles.mil + factor(decade)"

# Run each model

results_D1_1b <- lm(formula = D1_1b, data = tableD1)
results_D1_2b <- lm(formula = D1_2b, data = tableD1)

# Use cluster-robust standard errors

clrse_D1_1b <- results_D1_1b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

clrse_D1_2b <- results_D1_2b  %>% 
  vcovHC(type = "HC1", cluster = "group", adjust = TRUE) %>% 
  diag() %>% 
  sqrt()

D1_labs_b <- c("Temperature", "Confessional battles per million")

stargazer(results_D1_1b, results_D1_2b,
          se               = list(clrse_D1_1b, clrse_D1_2b),
          omit             = c("factor", "time.id", "Constant"),
          header           = FALSE,
          model.names      = FALSE,
          covariate.labels = D1_labs_a,
          dep.var.caption  = "Panel B: Ln persons tried per million",
          table.layout     = "=!l#-t-as-!", # Custom table formatting
          float            = FALSE,
          omit.stat        = c("adj.rsq", "ser", "f"),
          type             = "text")
