############################################################################
" MASTER REDUX"
" CREATE QIPP PACK"
###########################################################################

# setwd("C:/2017_projects/qipp")
# https://github.com/RevolutionAnalytics/checkpoint/wiki
# ***** --------------------------------------------------------------
"Use package check system: checkpoint / packrat. See details:"
# https://github.com/RevolutionAnalytics/checkpoint/wiki
library(checkpoint)
checkpoint(snapshotDate = "2017-10-01", checkpointLocation = "C:/R_default_wdir")

# Packages ----------------------------------------------------------------


library(here)
library(readxl)
library(stringr)
library(testthat)
library(extrafont) # for theme_strategy.
library(ReporteRs)
library(scales, warn.conflicts = FALSE)
suppressPackageStartupMessages(library(tidyverse))
library(ggrepel)


# Parameters 1--------------------------------------------------------

baseDir  <- "C:/2017_projects/qipp/" # using here, now

active_ccg <- "05Q"
f_year     <- 201617
first_year <- 201213

qipp_ccgs  <- c(# Alphabetical:
               "13P", # BXC
               "04X", # BSC
               "04Y", # CAN
               "05C", # DUD
               "05D", # EST
               "05F", # HER
               "05G", # NST
               "05J", # RED
               "05L", # SWB
               "05N", # SHR
               "05P", # SOL
               "05Q", # SES
               # "05R", # SWK -- OUTSIDE CSU
               "05T", # SWC
               "05V", # SAS
               "05W", # STO
               "05X", # TEL
               "05Y", # WAL
               # "05H", # WKN -- OUTSIDE CSU
               "06A", # WOL#
               "06D"  # WYR#
               )

# Parameters 2 -------------------------------------------------------------

ip_colour <- "#EC6555" # SU red
ae_colour <- "#91F5AD" # teal deer, alternative #75BBA7 ; "#91F5AD"
op_colour <- "#5881c1" # SU blue

# Funnel
funnelParameters <- tibble(
  Years = 1
  , RatePerPeople = 100000
  , Smoothness = 200 #Number of points making up the funnel curve
  )
personYears <- funnelParameters$RatePerPeople * funnelParameters$Years

# Rate of change
rocParameters <- tibble(
  From = first_year
  , To = f_year
)

# Trend
trendParameters <- tibble(
  Significance = 0.95
  )
trendCV <- qnorm((1 - trendParameters$Significance)/2, lower.tail = FALSE)


# Functions ---------------------------------------------------------------
"Careful with summary functions <- many old parameters exist here"
"some hard coded. Also may be affected by updates to packages"


source_here <- function(name){
 source(here::here("r", name))
  }

source_here("roundingAndChartLimitFunctions.R")
source_here("plot_functions_2017.R")
source_here("funnelPlotFunctions.R")
source_here("trendPlotFunctions.R")
source_here("costPlotFunctions.R")
source_here("summaryFunctions.R") 
source_here("theme_strategy.R")


source("C:/2017_projects/funnel/funnel/funlplotr20171024.R")
# to hopefully get the newest version of the funnel plot code.

# This - believe it or not - works like a function : pound()
pound <- dollar_format(prefix = "£")


convert_dsr_100k <- function(df) { # For DSR funnel
  if("target" %in% colnames(df)){
    mutate(df, target  = target*100000
           , fnlLow  = fnlLow*100000
           , fnlHigh = fnlHigh*100000)
  } else {
    mutate(df, DSRate = DSRate*100000)
  }
} 


label_ccg <- function(df){
  
  df %>%
    mutate(ccg_label =case_when(
  .$CCGCode == "13P" ~ "Bcc",
  .$CCGCode == "04X" ~ "Bsc",
  .$CCGCode == "04Y" ~ "Can",
  .$CCGCode == "05C" ~ "Dud",
  .$CCGCode == "05D" ~ "Est",
  .$CCGCode == "05F" ~ "Her",
  .$CCGCode == "05G" ~ "Nst",
  .$CCGCode == "05J" ~ "Red",
  .$CCGCode == "05L" ~ "Swb",
  .$CCGCode == "05N" ~ "Shr",
  .$CCGCode == "05P" ~ "Sol",
  .$CCGCode == "05Q" ~ "Ses",
  .$CCGCode == "05T" ~ "Swo",
  .$CCGCode == "05V" ~ "Sas",
  .$CCGCode == "05W" ~ "Sto",
  .$CCGCode == "05X" ~ "Tel",
  .$CCGCode == "05Y" ~ "Wal",
  .$CCGCode == "06A" ~ "Wol",
  .$CCGCode == "06D" ~ "Wyr"
  )
  )
}

# ***** --------------------------------------------------------------


# Load data ---------------------------------------------------------------
setwd(paste0(baseDir, "data"))
"Take care to note whether data was handled by PowerShell"

source_here("active_strategies_mess.R")

# How many strategies for each type of data
numberOfStrategies <- activeStrategies %>% 
  count(TableType)


# Load sus data:

sus_regex <- tibble(ip = "IP[0-9]{4}.csv", op = "OP[0-9]{4}.csv", ae = "AE[0-9]{4}.csv")
sus_csvs  <- map(sus_regex, function(x) list.files(pattern = x))


read_sus <- function(filename, col_headers){
  read_csv(filename, col_headers, na = "NULL", skip = 2)
  }

load_sus <- function(filenames_vector){ # eg. sus_csvs$ip
  cols <- map(filenames_vector, read_csv, n_max = 0) %>% map(colnames)
  map2_df(filenames_vector, cols, read_sus)
  }

ipData <- read_rds("ipData.RDS") # Comes from new method. Old: load_sus(sus_csvs$ip)
aeData <- load_sus(sus_csvs$ae)
opData <- load_sus(sus_csvs$op)


# List of CCGs (Now in data folder. Previously:)
# setwd(paste0(ifelse(inOffice, "S:/Commissioning Intelligence And Strategy/Strategic Analytics/", "change file path"), "Jonathan Spencer/FrequentFiles/Classification/Organisations/CCG"))

allCCGs <- read_excel("CCG Index.xlsx", sheet = "England") %>%
  filter(CCGActiveDate <= "2014-04-01") %>% 
  # Note : we're still using the old 3x Newcastle CCGs 
  select(CCGCode, CCGDescription, ShortName) %>%
  mutate(CCGDescription  = stringr::str_c(CCGDescription, " CCG"))
  

# CCG populations for cost charts from ONS projections
ccgPopulation <- read_csv("CCGPopulation.csv", skip = 1)


# *****chckpnt**** -----------------------------------------------------------------

activeCCGInfo <- allCCGs %>% 
  filter(CCGCode == active_ccg) %>% 
  mutate(CCGNameMinusCCG = stringr::str_replace(CCGDescription, " CCG", "")) # %>% 
  # unlist()


# Munge data ---------------------------------------------------------------

comparatorCCGs2 <- allCCGs %>% 
  filter(CCGCode %in% qipp_ccgs)


# Remove rows where the CCGCode is not valid.
removeInvalidCCGs <- . %>%
  inner_join(comparatorCCGs2, by = "CCGCode")


# ipSmall <- read_rds(paste0(baseDir, "ipSmall.RDS"))
ipSmall <- ipData %>% removeInvalidCCGs
rm(ipData) # RAM saver!
gc() # call after a large object has been removed


aeSmall <- aeData %>% removeInvalidCCGs
opSmall <- opData %>% removeInvalidCCGs %>%
  select(-starts_with("FUF"))


# JS: I'm going to call attendances Spells so that I can reuse functions
colnames(aeSmall)    <- gsub("Attendances", "Spells", colnames(aeSmall))
colnames(opSmall)    <- gsub("Attendances", "Spells", colnames(opSmall))


# How many rows should there be? 
ipBase <- expand.grid(
  CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
  , Strategy = activeStrategies %>% filter(TableType == "IP") %>% select(Strategy) %>% unlist %>% unname
  , FYear = unique(ipSmall$FYear)
  , stringsAsFactors = FALSE
  ) %>% 
  filter(!(Strategy %in% c("Readmissions_v1", "Canc_Op_v1")))

opBase <- expand.grid(
  CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
  , Strategy = activeStrategies %>% filter(TableType == "OP" & !grepl("^FUF.*", Strategy)) %>% select(Strategy) %>% unlist %>% unname
  , FYear = unique(opData$FYear)
  , stringsAsFactors = FALSE
)

aeBase <- expand.grid(
  CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
  , Strategy = activeStrategies %>% filter(TableType == "AE") %>% select(Strategy) %>% unlist %>% unname
  , FYear = unique(aeData$FYear)
  , stringsAsFactors = FALSE
)


# To deal with new AAFs: 
process_ip <- function(df){
  data <- df %>% 
  select(-CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -DSRate
         , -FYear, -Costs, -DSCosts, -DSRateVar, -DSCostsVar
         #, -alc_wholly, -alc_chronic, -alc_acute
         , convert = T) %>%
  filter(Highlighted == 1 | Highlighted > 0) %>% 
  mutate(aaf_reduction_factor = NA)
  
  # where Strategy is alcohol, then replace "Spells" with "Highlighted" number:
  # IN FACT NEED to reduce several cols by the ratio Highlighted/Spells
  data2 <- data %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, Costs, DSCosts, DSRateVar, DSCostsVar),
            funs(. * aaf_reduction_factor))
  
  data <- data %>% 
    filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
    bind_rows(data2) %>% 
    select(-Highlighted, aaf_reduction_factor) %>%
    group_by(CCGCode, Strategy, FYear) %>%
    summarise(
      Spells = sum(Spells, na.rm = TRUE)
      , DSRate = sum(DSRate, na.rm = TRUE)
      , Costs = sum(Costs, na.rm = TRUE)
      , DSCosts = sum(DSCosts, na.rm = TRUE)
      , DSRateVar = sum(DSRateVar, na.rm = TRUE)
      , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      DerivedPopulation = (Spells / DSRate) * personYears
      , IsActiveCCG = ifelse(CCGCode == active_ccg, TRUE, FALSE)  
    )
}

process <- . %>% 
  select(-CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -DSRate
         , -FYear, -Costs, -DSCosts, -DSRateVar, -DSCostsVar, convert = T) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  group_by(CCGCode, Strategy, FYear) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , DSRate = sum(DSRate, na.rm = TRUE)
    , Costs = sum(Costs, na.rm = TRUE)
    , DSCosts = sum(DSCosts, na.rm = TRUE)
    , DSRateVar = sum(DSRateVar, na.rm = TRUE)
    , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    DerivedPopulation = (Spells / DSRate) * personYears
    , IsActiveCCG = ifelse(CCGCode == active_ccg, TRUE, FALSE)  
  )

# Process the small data
ip <- ipSmall %>% process_ip %>% right_join(ipBase, by = c("CCGCode", "Strategy", "FYear"))
op <- opSmall %>% process %>% right_join(opBase, by = c("CCGCode", "Strategy", "FYear"))
ae <- aeSmall %>% process %>% right_join(aeBase, by = c("CCGCode", "Strategy", "FYear"))


# ***** --------------------------------------------------------------


# Funnel ------------------------------------------------------------------
ipFunnelPoints    <- ip    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
opFunnelPoints    <- op    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
aeFunnelPoints    <- ae    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()

# Roc ----------------------------------------------------------
setwd(paste0(baseDir, "r"))

 source("rateOfChangePlotFunctions.R")

ipRoCAll <- ip %>% roc_all 
ipRoCAll <- ipRoCAll %>% left_join(
  ipRoCAll %>% 
    select(CCGCode, Strategy, FYear, Spells) %>%
    rename(SpellsInBaseYear = Spells, From = FYear) 
  , by = c("CCGCode", "Strategy", "From")) %>% 
  unique.data.frame() # to remove duplicates from incl. base yr in roc_all


ipRoCActive <- ipRoCAll %>% roc_active

ipRoC <- ipRoCAll %>%
  inner_join(ipRoCActive, by = c("Strategy", "FYear", "From"))

ipRoCCheck <- ipRoC %>% filter(!IsValid)
ipRoC <- ipRoC %>% filter(IsValid) #N Kirklees and Wakefield

ipRoCSummary <- ipRoC %>% roc_summary

ipRoCFunnels <- roc_funnels(ipRoCSummary, funnelParameters$Smoothness, personYears)

####
aeRoCAll <- ae %>% roc_all 
aeRoCAll <- aeRoCAll %>% left_join(
  aeRoCAll %>% 
    select(CCGCode, Strategy, FYear, Spells) %>%
    rename(SpellsInBaseYear = Spells, From = FYear)
  , by = c("CCGCode", "Strategy", "From"))%>% 
  unique.data.frame() # to remove duplicates from incl. base yr in roc_all

# Remove wolves ambulance: (12/13 is problem)
aeRoCAll <- aeRoCAll %>% filter(!(Strategy == "AmbNoInvNoTreat_v1" & CCGCode == "06A")) # & FYear == 201213


aeRoCActive <- aeRoCAll %>% roc_active

aeRoC <- aeRoCAll %>%
  inner_join(aeRoCActive, by = c("Strategy", "FYear", "From"))

aeRoCCheck <- aeRoC %>% filter(!IsValid)
aeRoC <- aeRoC %>% filter(IsValid) 


aeRoCSummary <- aeRoC %>% roc_summary

aeRoCFunnels <- roc_funnels(aeRoCSummary, funnelParameters$Smoothness, personYears)
####
opRoCAll <- op %>% roc_all 
opRoCAll <- opRoCAll %>% left_join(
    opRoCAll %>% 
      select(CCGCode, Strategy, FYear, Spells) %>%
      rename(SpellsInBaseYear = Spells, From = FYear)
    , by = c("CCGCode", "Strategy", "From")) %>% 
    unique.data.frame() # to remove duplicates f
  
opRoCActive <- opRoCAll %>% roc_active

opRoC <- opRoCAll %>%
  inner_join(opRoCActive, by = c("Strategy", "FYear", "From"))

opRoCCheck <- opRoC %>% filter(!IsValid)
opRoC <- opRoC %>% filter(IsValid) 

opRoCSummary <- opRoC %>% roc_summary

opRoCFunnels <- roc_funnels(opRoCSummary, funnelParameters$Smoothness, personYears)

  

# Trend  ---------------------------------------------------------
# ipTrendActive <- ipSmall %>% trend_active 
aeTrendActive <- aeSmall %>% trend_active 
opTrendActive <- opSmall %>% trend_active 

# ipTrendComparators <- ipSmall %>% trend_comparators 

# aeTrendComparators with Wolves 1213 ambulance removed
aeTrendComparators <- aeSmall %>%
  select(-DSCosts, -DSCostsVar, -Costs, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -Spells, -CCGCode, -FYear, convert = T) %>%
  filter(!(CCGCode == "06A" & Strategy == "AmbNoInvNoTreat_v1" & FYear == 201213)) %>% 
  group_by(Strategy, FYear, CCGCode, Highlighted) %>%
  summarise(
    DSRate = sum(DSRate, na.rm = TRUE)
    , DSRateVar = sum(DSRateVar, na.rm = TRUE)
    , Spells = sum(Spells, na.rm = TRUE)
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  mutate(
    DerivedPopulation = (Spells / DSRate) * personYears
    , SpellsCIUpper = (Spells + 1 ) * (1 - 1 / (9 * (Spells + 1)) + trendCV / (3 * sqrt(Spells + 1))) ^ 3
    , SpellsCILower = Spells * (1 - 1 / (9 * Spells) - trendCV / (3 * sqrt(Spells))) ^ 3
    , DSRateCIUpper = DSRate + sqrt(DSRateVar / Spells) * (SpellsCIUpper - Spells)
    , DSRateCILower = DSRate + sqrt(DSRateVar / Spells) * (SpellsCILower - Spells)
  ) %>%
  group_by(Strategy, FYear, add = FALSE) %>%
  summarise(
    Average = sum(Spells, na.rm = TRUE) / sum(DerivedPopulation, na.rm = TRUE) * personYears
    , TopQuartile = quantile(DSRate, 0.25, na.rm = TRUE)
    , TopDecile = quantile(DSRate, 0.1, na.rm = TRUE)
    , MaxDSRate = max(DSRate, na.rm = TRUE)
    , MinDSRate = min(DSRate, na.rm = TRUE)
  ) %>%
  gather(Type, DSRate, -Strategy, -FYear, convert = TRUE) %>%
  mutate(TypeNumber = ifelse(Type == "MinDSRate", 1,
                             ifelse(Type == "TopDecile", 2, 
                                    ifelse(Type =="TopQuartile", 3,
                                           ifelse(Type == "Average", 4, 5))))) %>%
  arrange(Strategy, FYear, TypeNumber) %>%
  group_by(Strategy, FYear) %>%
  mutate(Low = DSRate, High = lead(DSRate, 1)) %>%
  filter(TypeNumber != 5)



opTrendComparators <- opSmall %>% trend_comparators 



# Active CCG:
# ipTrend adjustments for alcohol: (because ipSmall should really be changed):
ipTrend_adjust1 <- ipSmall %>% 
  filter(CCGCode == active_ccg)  %>%
  select(-DSCosts, -DSCostsVar, -Costs, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -Spells, -CCGCode, -FYear, convert = T) %>% # filter(Highlighted == 1 | Highlighted > 0) %>% 
  mutate(aaf_reduction_factor = NA)

ipTrend_adjust2 <- ipTrend_adjust1 %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, DSRateVar),
            funs(. * aaf_reduction_factor)) %>% 
  mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 

ipTrendActive <- ipTrend_adjust1 %>%
  filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  bind_rows(ipTrend_adjust2) %>% 
  group_by(Strategy, FYear, Highlighted) %>%
  summarise(
    DSRate = sum(DSRate, na.rm = TRUE)
    , DSRateVar = sum(DSRateVar, na.rm = TRUE) 
    , Spells = sum(Spells, na.rm = TRUE)
  )  %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  mutate(SpellsCIUpper = (Spells + 1 ) * (1 - 1 / (9 * (Spells + 1)) + trendCV / (3 * sqrt(Spells + 1))) ^ 3
         , SpellsCILower = Spells * (1 - 1 / (9 * Spells) - trendCV / (3 * sqrt(Spells))) ^ 3
         , DSRateCIUpper = DSRate + sqrt(DSRateVar / Spells) * (SpellsCIUpper - Spells)
         , DSRateCILower = DSRate + sqrt(DSRateVar / Spells) * (SpellsCILower - Spells)
         , Group = activeCCGInfo$CCGDescription)

# Comparators

ipTrend_adjust__comp1 <- ipSmall %>% 
  select(-DSCosts, -DSCostsVar, -Costs, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -Spells, -CCGCode, -FYear, convert = T) %>% # filter(Highlighted == 1 | Highlighted > 0) %>% 
  mutate(aaf_reduction_factor = NA)


ipTrend_adjust__comp2 <- ipTrend_adjust__comp1 %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, DSRateVar),
            funs(. * aaf_reduction_factor)) %>% 
  mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 


ipTrendComparators <- ipTrend_adjust__comp1 %>%
  filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  bind_rows(ipTrend_adjust__comp2) %>%
  select(-aaf_reduction_factor) %>% 
  group_by(Strategy, FYear, CCGCode, Highlighted) %>%
  summarise(
    DSRate = sum(DSRate, na.rm = TRUE)
    , DSRateVar = sum(DSRateVar, na.rm = TRUE) 
    , Spells = sum(Spells, na.rm = TRUE)
  )  %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted)%>%
  mutate(DerivedPopulation = (Spells / DSRate) * personYears) %>%
  group_by(Strategy, FYear, add = FALSE) %>%
  summarise(
    Average = sum(Spells, na.rm = TRUE) / sum(DerivedPopulation, na.rm = TRUE) * personYears
    , TopQuartile = quantile(DSRate, 0.25, na.rm = TRUE)
    , TopDecile = quantile(DSRate, 0.1, na.rm = TRUE)
    , MaxDSRate = max(DSRate, na.rm = TRUE)
    , MinDSRate = min(DSRate, na.rm = TRUE)
  ) %>%
  gather(Type, DSRate, -Strategy, -FYear, convert = TRUE) %>%
  mutate(TypeNumber = ifelse(Type == "MinDSRate", 1,
                             ifelse(Type == "TopDecile", 2, 
                                    ifelse(Type =="TopQuartile", 3,
                                           ifelse(Type == "Average", 4, 5))))) %>%
  arrange(Strategy, FYear, TypeNumber) %>%
  group_by(Strategy, FYear) %>%
  mutate(Low = DSRate, High = lead(DSRate, 1)) %>%
  filter(TypeNumber != 5)


# Cost [KEEP]-----------------------------------

ipCost <- ipSmall %>% 
  filter(FYear == f_year) %>%
  select(-DSRate, -DSRateVar, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -Costs, -DSCosts, -DSCostsVar, convert = T) %>%
  mutate(Highlighted = as.numeric(Highlighted)) %>% 
  mutate(aaf_reduction_factor = NA)

ipCost2 <- ipCost %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  # mutate(Highlighted = as.numeric(Highlighted)) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, Costs, DSCosts, DSCostsVar),
            funs(. * aaf_reduction_factor)) %>% 
  mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 

ipCost <- ipCost %>% 
  filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  bind_rows(ipCost2) %>% 
  select(-aaf_reduction_factor) %>% 
  group_by(CCGCode, Strategy, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , Costs = sum(Costs, na.rm = TRUE)
    , DSCosts = sum(DSCosts, na.rm = TRUE)
    , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  left_join(ccgPopulation, by = "CCGCode") %>%
  mutate(
    CostPerHead = Costs / Population
    , SpellsHigh = (Spells + 1) * (1 - 1/(9 * (Spells + 1)) + trendCV / (3 * sqrt(Spells + 1))) ^ 3
    , SpellsLow = Spells * (1 - 1/(9 * Spells) - trendCV / (3 * sqrt(Spells))) ^ 3
    , CostsHigh = (Costs + 1) * (1 - 1/(9 * (Costs + 1)) + trendCV / (3 * sqrt(Costs + 1))) ^ 3
    , CostsLow = Costs * (1 - 1/(9 * Costs) - trendCV / (3 * sqrt(Costs))) ^ 3
    , DSCostsCIUpper = DSCosts + sqrt(DSCostsVar / Costs) * (CostsHigh - Costs)
    , DSCostsCILower = DSCosts + sqrt(DSCostsVar / Costs) * (CostsLow - Costs)    
    , DSCostsPerHead = DSCosts / 100000
    , DSCostsPerHeadUpper = DSCostsCIUpper / 100000
    , DSCostsPerHeadLower = DSCostsCILower / 100000
    , IsActiveCCG = CCGCode == active_ccg) %>%
  left_join(allCCGs, by = "CCGCode")

aeCost <- aeSmall %>% cost_ds 
opCost <- opSmall %>% cost_ds 

# ***** --------------------------------------------------------------


# Inpatient plots ---------------------------------------------------------
setwd(baseDir)

ipPlottableStrategies <- activeStrategies %>%
  filter(TableType == "IP") %>%
  filter(Strategy != "Canc_Op_v1") %>%
  filter(Strategy != "Readmissions_v1")


"FASTER IF YOU PRE-ALLOCATE LENGTH of list with vector('list', length)"
plot_ip_fun   <- list() 
plot_ip_roc   <- list()
plot_ip_trend <- list()


for(i in seq(ipPlottableStrategies$Strategy)){
# Draw funnel plot --------------------------------------------------------
 
  funnel <- funl_Data(ipFunnelPoints %>% filter(Strategy == ipPlottableStrategies$Strategy[i]) 
                         , col.unit = "CCGCode"
                         , col.group = "Strategy"
                         , col.O = "Spells" # Should be costs (?) but doesn't work in poisson
                         , col.n = "DerivedPopulation"
                         , col.rt = "DSRate"
                         , target = NULL
                         , smoothness = 100
                         , fnlMinEvents = NULL
                         , fnlMaxEvents = NULL
                         ) 
  
  # What happens when you have a rate which does not come directly from (spells/population)?
  # This situation (qipp) works only because the population has been derived (from Spells/DSRate)
 
  plotFunnels <- funnel[[1]] %>% convert_dsr_100k() %>% filter(fnlLimit == "threeSigma") # only 3 sigma
  plotUnits   <- funnel[[2]] %>% 
    left_join(ipFunnelPoints %>% filter(Strategy == ipPlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k() %>% label_ccg()
  " SUGGEST THIS JOIN IS INCORPORATED IN FUNCTION funl_data"
  " SUGGEST NEW NAME funl_data instead of funl_Data"
  
  plot_ip_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = ip_colour[1])
  

# Draw roc plot ------------------------------------------------------

  plotRocPoints <- ipRoC %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- ipRoCFunnels %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  plotRocSummary <- ipRoCSummary %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
 
  plot_ip_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = ip_colour[1])
  
  
# Draw trend plots --------------------------------------------------------

  plotTrendActive <- ipTrendActive %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  plotTrendComparators <- ipTrendComparators %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])

  
  plot_ip_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate,
                                   colour_block = ip_colour[1])
   
# ***** -----------------------------------------------------
}

rm(
   # plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
  funnel, plotFunnels, plotUnits
   , plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)


# A&E plots ---------------------------------------------------------------
aePlottableStrategies <- activeStrategies %>%
  filter(TableType == "AE") 

plot_ae_fun   <- list()
plot_ae_roc   <- list()
plot_ae_trend <- list()

for(i in seq(aePlottableStrategies$Strategy)){
# Draw funnel plot --------------------------------------------------------
 
  
  funnel <- funl_Data(aeFunnelPoints %>% filter(Strategy == aePlottableStrategies$Strategy[i]) 
                      , col.unit = "CCGCode"
                      , col.group = "Strategy"
                      , col.O = "Spells" # Should be costs (?) but doesn't work in poisson
                      , col.n = "DerivedPopulation"
                      , col.rt = "DSRate"
                      , target = NULL
                      , smoothness = 100
                      , fnlMinEvents = NULL
                      , fnlMaxEvents = NULL
  ) 

  plotFunnels <- funnel[[1]] %>% convert_dsr_100k() %>% filter(fnlLimit == "threeSigma")
  plotUnits   <- funnel[[2]] %>% 
    left_join(aeFunnelPoints %>% filter(Strategy == aePlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k()
  
  plot_ae_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = ae_colour[1])
 
  
  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- aeRoC %>%
    filter(Strategy == aePlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- aeRoCFunnels %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotRocSummary <- aeRoCSummary %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  
  plot_ae_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = ae_colour)
  
  
  
# Draw trend plots --------------------------------------------------------
  plotTrendActive <- aeTrendActive %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotTrendComparators <- aeTrendComparators %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  
  plot_ae_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate,
                                   colour_block = ae_colour[1])
  
  
# ***** ------------------------------------------------------------
}
rm(
  #plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
    funnel, plotFunnels, plotUnits
   , plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)

# Outpatient plots --------------------------------------------------------
opPlottableStrategies <- activeStrategies %>%
  filter(TableType == "OP") %>%
  filter(!(grepl("^FUF*", Strategy)))

plot_op_fun   <- list()
plot_op_roc   <- list()
plot_op_trend <- list()

for(i in seq(opPlottableStrategies$Strategy)){
# Draw funnel plot --------------------------------------------------------

    funnel <- funl_Data(opFunnelPoints %>% filter(Strategy == opPlottableStrategies$Strategy[i]) 
                      , col.unit = "CCGCode"
                      , col.group = "Strategy"
                      , col.O = "Spells" # Should be costs (?) but doesn't work in poisson
                      , col.n = "DerivedPopulation"
                      , col.rt = "DSRate"
                      , target = NULL
                      , smoothness = 100
                      , fnlMinEvents = NULL
                      , fnlMaxEvents = NULL
  ) 

  plotFunnels <- funnel[[1]] %>% convert_dsr_100k() %>% filter(fnlLimit == "threeSigma") # only 3 sigma
  plotUnits   <- funnel[[2]] %>% 
    left_join(opFunnelPoints %>% filter(Strategy == opPlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k()
  
  plot_op_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = op_colour[1])
  

  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- opRoC %>%
    filter(Strategy == opPlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- opRoCFunnels %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotRocSummary <- opRoCSummary %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plot_op_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = op_colour[1])
  
  

# Draw trend plots --------------------------------------------------------
  plotTrendActive <- opTrendActive %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotTrendComparators <- opTrendComparators %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plot_op_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate,
                                   colour_block = op_colour[1])
  
# ***** ---------------------------------------------------
}
rm(
  #plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
   funnel, plotFunnels, plotUnits
   , plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)


# Save Plots ---------------------------------------------------------
setwd(paste0(baseDir, "output/"))

qipp_save <- function(x, y){
  ggsave(filename = x,
         plot = y,
         width    = 9.5*1.414, # A4 ratio
         height   = 9.5,
         units    = "cm")
  }


# IP

for(i in seq_along(ipPlottableStrategies$Strategy)){
  
  qipp_save(paste0(i, "_fun_", ipPlottableStrategies$Strategy[i], ".png"),
            plot_ip_fun[[i]])
  
  qipp_save(paste0(i, "_roc_", ipPlottableStrategies$Strategy[i], ".png"),
            plot_ip_roc[[i]])
  
  qipp_save(paste0(i, "_trend_", ipPlottableStrategies$Strategy[i], ".png"),
            plot_ip_trend[[i]])
}


# AE

for(i in seq_along(aePlottableStrategies$Strategy)){
  
  qipp_save(paste0(i, "_fun_", aePlottableStrategies$Strategy[i], ".png"),
            plot_ae_fun[[i]])
  
  qipp_save(paste0(i, "_roc_", aePlottableStrategies$Strategy[i], ".png"),
            plot_ae_roc[[i]])
  
  qipp_save(paste0(i, "_trend_", aePlottableStrategies$Strategy[i], ".png"),
            plot_ae_trend[[i]])
}

# OP

for(i in seq_along(opPlottableStrategies$Strategy)){
  
  qipp_save(paste0(i, "_fun_", opPlottableStrategies$Strategy[i], ".png"),
            plot_op_fun[[i]])
  
  qipp_save(paste0(i, "_roc_", opPlottableStrategies$Strategy[i], ".png"),
            plot_op_roc[[i]])
  
  qipp_save(paste0(i, "_trend_", opPlottableStrategies$Strategy[i], ".png"),
            plot_op_trend[[i]])
}




# Experiment with purrr WALK
## see pwalk in r
# tmp_fnames <- map_chr(seq_along(ipPlottableStrategies$Strategy) , function(i) paste0(i , "_fun_", ipPlottableStrategies$Strategy[i] ,".png"))
# # tmp_fnames <- str_c(baseDir, "output/",as.character(seq(1, length(tmp_fnames))), ".1_", tmp_fnames)
# # 
# pwalk(list(tmp_fnames, plot_ip_fun), qipp_save)

"Next time you come to print, try to write a closure to reduce duplication."

# save_plots <- function(vector_of_strats, list_of_plots){
#   
#   png_filenames <- map_chr(seq_along(vector_of_strats),
#                            function(i) paste0(i ,# This just means that one doesn't need to specify "trend" etc.
#                                               str_c("_", 
#                                                          c(unlist(
#                                                            str_split(
#                                                              as.character(quote(vector_of_strats[i])), "\\_")
#                                                            )[c(F, F, T)]),
#                                                          "_"),
#                                                vector_of_strats[i],
#                                               ".png"
#                                               )
#                            )
#   
#   pwalk(list(png_filenames, list_of_plots), qipp_save)
# }
# 
# 
# str_c("_", 
#       c(unlist(
#         str_split(
#           as.character(quote(list_of_plots)), "\\_")
#       )[c(F, F, T)]),
#       "_"),
# vector_of_strats[i],
# ".png"
# )
# 
# save_plots(ipPlottableStrategies$Strategy, plot_ip_fun)
# save_plots(ipPlottableStrategies$Strategy, plot_ip_roc)
# save_plots(ipPlottableStrategies$Strategy, plot_ip_trend)
# 
# save_plots(aePlottableStrategies$Strategy, plot_ae_fun)
# save_plots(aePlottableStrategies$Strategy, plot_ae_roc)
# save_plots(aePlottableStrategies$Strategy, plot_ae_trend)
# 
# save_plots(opPlottableStrategies$Strategy, plot_op_fun)
# save_plots(opPlottableStrategies$Strategy, plot_op_roc)
# save_plots(opPlottableStrategies$Strategy, plot_op_trend)


# ***** --------------------------------------------------------------

# Summary tables -------------------------------------------------------------
setwd(baseDir)

summ_ipFunnelPoints    <- ip  %>% filter(FYear == f_year) 
summ_opFunnelPoints    <- op  %>% filter(FYear == f_year)
summ_aeFunnelPoints    <- ae  %>% filter(FYear == f_year)

summ_ipFunnelSummary <- summ_ipFunnelPoints %>% funnel_summary
summ_aeFunnelSummary <- summ_aeFunnelPoints %>% funnel_summary
summ_opFunnelSummary <- summ_opFunnelPoints %>% funnel_summary

summ_ipFunnelFunnels <- funnel_funnels(summ_ipFunnelSummary, funnelParameters$Smoothness, personYears)
summ_aeFunnelFunnels <- funnel_funnels(summ_aeFunnelSummary, funnelParameters$Smoothness, personYears)
summ_opFunnelFunnels <- funnel_funnels(summ_opFunnelSummary, funnelParameters$Smoothness, personYears)


#  *****--------------------------------------------------------------


# Inpatient ---------------------------------------------------------------

totalActivityIP <- total_activity(ipSmall)

savingsAnyOneIP <- ipTrendComparators %>% savings_any_one()

ipSignificance  <- significance_summary(summ_ipFunnelPoints, summ_ipFunnelFunnels, ipRoC, ipRoCFunnels)


summaryOutputIP <- ipSmall %>%
  filter(FYear == f_year & CCGCode == active_ccg) %>%
  select(-FYear, -DSRateVar, -DSCosts, -DSCostsVar, -CCGCode, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted,  -Spells, -Costs, -DSRate, convert = T) %>%
  mutate(aaf_reduction_factor = NA)

summaryOutputIP2 <- summaryOutputIP %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, Costs),
            funs(. * aaf_reduction_factor)) %>% 
  mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 

summaryOutputIP3 <- summaryOutputIP %>% 
  filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  bind_rows(summaryOutputIP2) %>% 
  select(-aaf_reduction_factor) %>% 
  group_by(Strategy, Highlighted) %>%
  summarise_all(
    funs(sum(., na.rm = TRUE))
    #, Spells, Costs, DSRate
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  left_join(savingsAnyOneIP, by = "Strategy") %>%
  mutate_at(vars(Average, TopQuartile, TopDecile),
            funs(
              Comparators = (.)
              , SavingsIf = (Costs - (. / DSRate) * Costs)) #generate savings if average
  ) %>%
  mutate_at(vars(matches("_SavingsIf")), funs(ifelse( . < 0 , 0, .))) %>% # remove negative savings
  mutate(
    SpellsRounded = roundTo(Spells, 10)
    , propSpells = Spells / totalActivityIP$Spells) %>%
  mutate_at(vars(matches("_SavingsIf"), Costs), 
            funs(
              Actual = (.)
              , Rounded = roundTo(., 1000)
            )) %>%
  left_join(activeStrategies, by = "Strategy") %>%
  select(-StrategyType) %>%
  left_join(ipSignificance, by ="Strategy") %>% 

# summaryOutputIP <- ipSmall %>% summary_output(., savingsAnyOneIP, ipSignificance, totalActivityIP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(ipTrendActive %>% 
    filter(FYear == f_year) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(ipCost %>% # is this actually needed now?s
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))

summaryOutputIP <- summaryOutputIP3

# IP labels for charts / tables------------------------------------------

labels_ip <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("ACS Acute",
                         "ACS Chronic",
                         "ACS Vaccine",
                         "Alcohol (wholly)",
                         "Alcohol (partially - chronic)",
                         "Alcohol (partially - acute)",
                         "",
                         "End of Life Care (3-14 days)",
                         "End of Life Care (0-2 days)",
                         "Falls",
                         "Frail Elderly (occasional)",
                         "Frail Elderly (usual)",
                         "Medically Unexplained",
                         "Medicines - Explicit",
                         "Medicines - Implicit AntiDiab",
                         "Medicines - Implicit Benzo",
                         "Medicines - Implicit Diuretics",
                         "Medicines - Implicit NSAIDs",
                         "Obesity (largely)",
                         "Obesity (marginal)",
                         "Obesity (somewhat)",
                         "PLCV Cosmetic",
                         "PLCV Alternative",
                         "PLCV Ineffective",
                         "PLCV Risks",
                         "Mental Health Admissions from ED",
                         "",
                         "Self-harm",
                         "Smoking (large)",
                         "Smoking (somewhat)",
                         "Zero Length of Stay (adult)",
                         "Zero Length of Stay (child)"))


# IP tbl summary -----------------------------------------------------

summ_ip_summ_out <- summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>%
  mutate(Costs_Rounded = Costs_Rounded / 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  `colnames<-`(c("Strategy", "Admissions", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change")) %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)

summ_ip_summ_out[4:5][summ_ip_summ_out[4:5] == "Not Significant"] <- "-"


# fn: add spacer column
add_spacer_ip <- . %>% 
  mutate(` `= " ") %>% 
  select(Opportunity, Admissions, `2016-17 Spend (000s)`, ` `, everything())

# other pods
add_spacer <- . %>% 
  mutate(` `= "     ") %>% 
  select(Opportunity, Activity, `2016-17 Spend (000s)`, ` `, everything())

summ_ip_summ_out <-  summ_ip_summ_out %>% add_spacer_ip()

flexify_summary <- function(df){
  
  data    <- vanilla.table(df)
 
  data[,] <- textProperties(font.family = "Segoe UI", font.size = 10)
  data[to = "header"]      <-  textProperties(font.size = 10, font.family = "Segoe UI", font.weight = "bold")
  
  data[, 1]                <- parLeft()
  data[, 1, to = "header"] <- parLeft()
  
  data[, 5:6]                <- parLeft()
  data[, 5:6, to = "header"] <- parLeft()
  
  data <- setFlexTableBorders(data
                              , inner.vertical = borderProperties( style = "dashed", color = "white" )
                              , inner.horizontal = borderProperties( style = "solid", color = "grey80"  )
                              , outer.vertical = borderProperties( width = 2, color = "white"  )
                              , outer.horizontal = borderProperties( width = 1, color = "grey30"  )
  )
  
  data
  }

flex_ip_summ <- flexify_summary(summ_ip_summ_out)

# add footnote "compared to CCGs in the West Midlands"


# IP cost summary ----------------------------------------------------

summ_ip_cost_out <- # head(
  summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded = pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  `colnames<-`(c("Strategy", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)

# add footnote "compared to CCGs in the West Midlands"


flexify_costs <- function(df){
  
  data    <-  vanilla.table(df)
  data[,] <-  textProperties(font.family = "Segoe UI" , font.size = 10)
  
  data[to = "header"]  <-  textProperties(font.size = 10, font.family = "Segoe UI", font.weight = "bold")
  
  data[, 1]                <- parLeft()
  data[, 1, to = "header"] <- parLeft()
  
  
  data <- setFlexTableBorders(data
                              , inner.vertical = borderProperties( style = "dashed", color = "white" )
                              , inner.horizontal = borderProperties( style = "solid", color = "grey80"  )
                              , outer.vertical = borderProperties( width = 2, color = "white"  )
                              , outer.horizontal = borderProperties( width = 1, color = "grey30"  )
  )
  
  
  data
}

flex_ip_cost <- flexify_costs(summ_ip_cost_out)


# IP savings plot -----------------------------------------------

savingsIP <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded) %>% 
  rename(average = Average_SavingsIf_Rounded 
         , quartile = TopQuartile_SavingsIf_Rounded 
         , decile = TopDecile_SavingsIf_Rounded 
  ) %>% 
  filter(Strategy != "Readmissions_v1", Strategy != "Canc_Op_v1")  %>% 
  # gather(level, saving, 2:4)  %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)



plot_the_savings_ip <- function(df, pod_colour){
  
  ggplot(df, aes(reorder(Opportunity, average), average))+
    geom_bar(stat = "identity", aes(fill = "myline1"))+
    geom_bar(aes(Opportunity, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
    # geom_bar(stat = "identity", position = "stack") +
    coord_flip()+
    theme_strategy_large()+
    scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
                       , position = "top"
                       , expand = c(0,0))+
    # scale_y_continuous(labels = scales::comma_format())
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.82, 0.18),
          legend.background = element_rect(fill = "white"))+
    scale_fill_manual(
      name = "line Colour"
      ,values=c(myline1 = pod_colour, myline2 = pod_colour)
      , labels=c("Savings if average", "Additional savings if top quartile"))+
    # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
    #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
    ylab("Potential savings (£ millions)")+
    guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2))))+ # the second alpha does not have to relate
  theme(# panel.grid.major = element_blank(),
        panel.background = element_blank())+
    scale_x_discrete(expand = c(0,0))
  
}
  
plot_savings_ip <- plot_the_savings_ip(savingsIP, ip_colour)   


# ***** --------------------------------------------------------------


# A&E ---------------------------------------------------------------------
totalActivityAE <- aeSmall %>% total_activity
savingsAnyOneAE <- aeTrendComparators %>% savings_any_one

aeSignificance <- significance_summary(summ_aeFunnelPoints, summ_aeFunnelFunnels, aeRoC, aeRoCFunnels)

summaryOutputAE <- aeSmall %>% summary_output(., savingsAnyOneAE, aeSignificance, totalActivityAE) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(aeTrendActive %>% 
    filter(FYear == f_year) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(aeCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))


# A&E labels for charts / tables------------------------------------------

labels_ae <- summaryOutputAE %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("Ambulance Conveyed, No Treatment",
                         "Frequent Attenders",
                         "Left Before Seen",
                         "Low Acuity ED"
                          ))


# AE tbl summary -----------------------------------------------------

summ_ae_summ_out <- summaryOutputAE %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  right_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change"))

summ_ae_summ_out[4:5][summ_ae_summ_out[4:5] == "Not Significant"] <- "-"

summ_ae_summ_out <- summ_ae_summ_out %>% add_spacer()

flex_ae_summ <- flexify_summary(summ_ae_summ_out)


# AE cost summary ----------------------------------------------------

summ_ae_cost_out <- # head(
  summaryOutputAE %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) 
# add footnote "compared to CCGs in the West Midlands"

flex_ae_cost    <- flexify_costs(summ_ae_cost_out)



# AE savings plot -----------------------------------------------

savingsAE <- summaryOutputAE %>%
  ungroup() %>% 
  select(Strategy, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded) %>% 
  rename(average = Average_SavingsIf_Rounded 
         , quartile = TopQuartile_SavingsIf_Rounded 
         , decile = TopDecile_SavingsIf_Rounded 
  ) %>% 
  left_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) 


plot_the_savings_other <- function(df, pod_colour){
  
  ggplot(df, aes(reorder(Opportunity, average), average))+
    geom_bar(stat = "identity", aes(fill = "myline1"))+
    geom_bar(aes(Opportunity, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
    # geom_bar(stat = "identity", position = "stack") +
    coord_flip()+
    theme_strategy_large()+
    scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
                       , position = "top"
                       , expand = c(0,0))+
    theme(legend.title = element_blank(),
          legend.background = element_rect(fill = "white"),
          legend.position  = "bottom",
          axis.title.y = element_blank(),
          # legend.position = c(0.82, 0.18),
          panel.background = element_blank())+
    scale_fill_manual(
      name = "line Colour"
      ,values=c(myline1 = pod_colour, myline2 = pod_colour)
      , labels=c("Savings if average", "Additional savings if top quartile"))+
    ylab("Potential savings (£ millions)")+
    guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2))))+ # the second alpha does not have to relate
    scale_x_discrete(expand = c(0,0))
  
}

plot_savings_ae <- plot_the_savings_other(savingsAE, ae_colour)

# ***** --------------------------------------------------------------


# Outpatients -------------------------------------------------------------
totalActivityOP <- opSmall %>% total_activity
savingsAnyOneOP <- opTrendComparators %>% savings_any_one

opSignificance <- significance_summary(summ_opFunnelPoints, summ_opFunnelFunnels, opRoC, opRoCFunnels)

summaryOutputOP <- opSmall %>% summary_output(., savingsAnyOneOP, opSignificance, totalActivityOP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(opTrendActive %>% 
    filter(FYear == f_year) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(opCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))


# OP labels for charts / tables------------------------------------------

labels_op <- summaryOutputOP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("Consultant-Consultant Referral",
                         "GP referred Medical (adult)",
                         "GP referred Medical (child)",
                         "GP referred Surg (adult)",
                         "GP referred Surg (child)",
                         ""
  ))




# OP tbl summary -----------------------------------------------------

summ_op_summ_out <- summaryOutputOP %>%
  filter(!Strategy %in% c("PLCV_v1")) %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change")) 

summ_op_summ_out[4:5][summ_op_summ_out[4:5] == "Not Significant"] <- "-"

summ_op_summ_out <- summ_op_summ_out %>% add_spacer()

flex_op_summ <- flexify_summary(summ_op_summ_out)



# OP cost summary ----------------------------------------------------

summ_op_cost_out <- summaryOutputOP %>%
  filter(!Strategy %in% c("PLCV_v1")) %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>%
# add footnote "compared to CCGs in the West Midlands"
  `colnames<-`(c("Opportunity", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) 

# add footnote "compared to CCGs in the West Midlands"

flex_op_cost    <- flexify_costs(summ_op_cost_out)


# OP savings plot -----------------------------------------------

savingsOP <- summaryOutputOP %>%
  filter(!Strategy %in% c("PLCV_v1")) %>%
  ungroup() %>% 
  select(Strategy, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded) %>% 
  rename(average = Average_SavingsIf_Rounded 
         , quartile = TopQuartile_SavingsIf_Rounded 
         , decile = TopDecile_SavingsIf_Rounded 
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)


plot_savings_op <- plot_the_savings_other(savingsOP, op_colour)



#  *****--------------------------------------------------------------

# Comparator table ---------------------------------------------------
"needs to include all, though?"

# comparatorsOut <- comparatorCCGs2 %>%
#   filter(CCGCode != active_ccg) %>%
#   select(CCGCode, CCGDescription)
# 
# flex_comparat    <- setZebraStyle(vanilla.table(comparatorsOut), odd = alpha("goldenrod1", 0.4), even = alpha("goldenrod1", 0.2))
# 
# 
# flex_comparat    <- vanilla.table(comparatorsOut)
# 
# 
# 
# flex_comparat[,] <- textProperties(font.family = "Segoe UI", font.size = 12)
# flex_comparat[to = "header"]      <-  textProperties(font.size = 14, font.family = "Segoe UI")
# 
# # align left
# flex_comparat[, ]                <- parLeft()
# flex_comparat[, , to = "header"] <- parLeft()
# 
# # borders
# flex_comparat <- setFlexTableBorders(flex_comparat
#                                     , inner.vertical = borderProperties( style = "dashed", color = "white" )
#                                     , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
#                                     , outer.vertical = borderProperties( width = 2, color = "white"  )
#                                     , outer.horizontal = borderProperties( width = 2, color = "white"  )
# )




# Population differences ---------------------------------------------
# 
# 
# # For comparison
# setwd(paste0(baseDir, "data"))
# ccg_regist <- read_csv("ccg-reg-patients.csv",
#                        col_types = cols_only(CCG_CODE = "c", TOTAL_ALL = "i"))
# 
# 
# pop_comparisons <- ccgPopulation %>%
#   left_join(ccg_regist, by = c("CCGCode" = "CCG_CODE")) %>%
#   right_join(comparatorCCGs2, by = "CCGCode") %>%
#   select(CCGDescription, everything(), - CCGCode, -ShortName) %>%
#   mutate(reg_minus_res = TOTAL_ALL - Population) %>%
#   rename(resident = Population, registered = TOTAL_ALL) %>%
#   mutate(greater = ifelse(reg_minus_res > 0, "registered", "resident")) %>%
#   mutate(diff_magnitude = abs(reg_minus_res)) %>%
#   select(-reg_minus_res) %>%
#   mutate(res_over_reg = round(resident/registered, 2)*100) %>%
#   arrange(res_over_reg)
#   # mutate(flag = ifelse(res_over_reg <0.95 | res_over_reg > 1.05, "!", "")) %>% 
# 
# 
# 
# 
# flex_pop    <-  setZebraStyle(vanilla.table(pop_comparisons), odd = alpha("dodgerblue2", 0.15), even = alpha("dodgerblue2", 0.1))
# flex_pop[,] <-  textProperties(font.family = "Segoe UI"
#                                    , font.size = 12)
# 
# flex_pop[to = "header"]  <-  textProperties(font.size = 14,
#                                                 font.family = "Segoe UI")
# 
# flex_pop[, 1]                <- parLeft()
# flex_pop[, 1, to = "header"] <- parLeft()
# 
# 
# flex_pop <- setFlexTableBorders(flex_pop
#                                     , inner.vertical = borderProperties( style = "dashed", color = "white" )
#                                     , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
#                                     , outer.vertical = borderProperties( width = 2, color = "white"  )
#                                     , outer.horizontal = borderProperties( width = 2, color = "white"  )
# )


# write_csv(pop_comparisons, "population_comparisons.csv")

#  END-------------------------------------------------------------


cat("Ends")
activeCCGInfo$CCGDescription

