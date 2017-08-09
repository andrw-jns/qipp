rm(list = ls())


library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

trendCV <- qnorm((1 - 0.95)/2, lower.tail = FALSE)

# Inpatients --------------------------------------------------------------
setwd("H:/QIPP/Data")

englandData <- read_csv(
  file = "Output_PbR_IP_England.csv"
  , col_names = read_csv("Output_PbR_IP_England.csv", col_names = TRUE, n_max = 0) %>% colnames
  , col_types = paste0("ddddidi", paste0(rep("i", 32), collapse = ""))
  , skip = 2
  , na = "NULL"
  )

DSRbyStrategy <- . %>%
  select(-DSCostsVar, -Costs) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -DSCosts, -Spells, -FYear, convert = TRUE) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  group_by(Strategy, FYear) %>%
  summarise_each(funs(sum)) %>%
  mutate(
    SpellsCIUpper = (Spells + 1 ) * (1 - 1 / (9 * (Spells + 1)) + trendCV / (3 * sqrt(Spells + 1))) ^ 3
    , SpellsCILower = Spells * (1 - 1 / (9 * Spells) - trendCV / (3 * sqrt(Spells))) ^ 3
    , DSRateCIUpper = DSRate + sqrt(DSRateVar / Spells) * (SpellsCIUpper - Spells)
    , DSRateCILower = DSRate + sqrt(DSRateVar / Spells) * (SpellsCILower - Spells)
    , DSCostsPerHead = DSCosts / 100000
  )

ipDSRbyStrategy <- englandData %>% 
  DSRbyStrategy %>%
  filter(FYear == 201415) %>%
  select(-FYear) %>%
  mutate(Type = "IP")

# Outpatients -------------------------------------------------------------
opEnglandData <- read_csv(
  file = "Output_HES_OP_England.csv"
  , col_names = read_csv("Output_HES_OP_England.csv", col_names = TRUE, n_max = 0) %>% colnames
  , skip = 2
  )

opDSRbyStrategy <- opEnglandData %>%
  rename(Spells = Attendances) %>%
  select(-starts_with("FUF_")) %>%
  DSRbyStrategy %>%
  filter(FYear == 201415) %>%
  select(-FYear) %>%
  mutate(Type = "OP")
  
  


# A&E ---------------------------------------------------------------------
aeEnglandData <- read_csv(
  file = "Output_HES_AE_England.csv"
  , col_names = read_csv("Output_HES_AE_England.csv", col_names = TRUE, n_max = 0) %>% colnames
  , skip = 2
  )

aeDSRbyStrategy <- aeEnglandData %>%
  rename(Spells = Attendances) %>%
  DSRbyStrategy %>%
  filter(FYear == 201415) %>%
  select(-FYear) %>%
  mutate(Type = "AE")


# Export data -------------------------------------------------------------

out <- bind_rows(ipDSRbyStrategy, opDSRbyStrategy, aeDSRbyStrategy)
write_csv(out, "R_England_DSR.csv")

