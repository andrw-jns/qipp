############################################################################
" QIPP: INITIATE "
###########################################################################

# ***** --------------------------------------------------------------
"No Plots" 

# Packages ----------------------------------------------------------------

library(here)
library(readxl)
library(scales, warn.conflicts = FALSE)
# library(testthat)
library(extrafont) # for theme_strategy.
library(stringr)
library(ReporteRs)
suppressPackageStartupMessages(library(tidyverse))
library(ggrepel)


# Parameters 1--------------------------------------------------------

baseDir  <- "C:/2017_projects/qipp/" # using here, now

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
source_here("funnelPlotFunctions.R")
source_here("trendPlotFunctions.R")
source_here("costPlotFunctions.R")
source_here("summaryFunctions.R") 

# This, believe it or not, works like a function : pound()
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
      .$CCGCode == "05T" ~ "Swa",
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

"Now do some wrangling to add new set of names to activeStrategies:"

# manually assign ids (based on matching old strats to master list with names):
id_ref <- c(1,   1,   1,  2,  2,  2, 26, 25, 29,  5, 8,
            7,  24,  33, 33, 33, 33, 35, 23,  6,  9,
            9,   9,   9,  9, 28, 28, 28, 28, 11, 12,
            14, 14,  NA, 13, 32, 32, 32, 32, 13, 13,     # OP PLCV = NA
            10,  39, 39,  4,  3)

new_strat_list <- qippStrategiesMasterList_csv <- read_excel("C:/2017_projects/qipp/data/qippStrategiesMasterList.xlsx") %>% 
  filter(id %in% id_ref)


activeStrategies <- read_csv("listActiveStrategies.csv") %>% 
  # manually assign ids (based on matching old strats to master list with names)
  mutate(id = id_ref) %>% 
  # left_join(new_strat_list, by = "id") %>% 
  arrange(id)

# removing unused strategies (FUF, OP PLCV):
test <- slice(activeStrategies, c(1:34, 39:45))


"Adapt (wrangle) new names master list:"
tmp <- new_strat_list %>% 
  select(id, oldName, shortName, longName, breakdownAvailable, matches("breakdown[1-5]"))

# fix for alcohol
tmp[2, 7:8] <- "should be updated to match indicator source"


tmp2 <- gather(tmp, "breakdown", "sub_header", 6:10)
# tmp2 <- tmp2 %>% distinct(id, breakdown, .keep_all = T)

tmp3 <- tmp2 %>% 
  filter(breakdownAvailable == 0) %>% 
  distinct(longName, .keep_all = T)

tmp4 <- tmp2 %>% 
  filter(breakdownAvailable == 1) %>% 
  arrange(id) %>% 
  na.omit()

tmp_final <- bind_rows(tmp3, tmp4) %>% 
  arrange(id)

# This "join" is in fact a bind cols - so may not work in future:

activeStrategies <- bind_cols(test, tmp_final)
# List of strategies
activeStrategies <- read_csv("listActiveStrategies.csv")
# if run from powershell:

# How many strategies for each type of data
numberOfStrategies <- activeStrategies %>% 
  count(TableType)

# Load sus data

sus_regex <- tibble(ip = "IP[0-9]{4}.csv", op = "OP[0-9]{4}.csv", ae = "AE[0-9]{4}.csv")
sus_csvs  <- map(sus_regex, function(x) list.files(pattern = x))


read_sus <- function(filename, col_headers){
  read_csv(filename, col_headers, na = "NULL", skip = 2)
}

load_sus <- function(filenames_vector){ # eg. sus_csvs$ip
  cols <- map(filenames_vector, read_csv, n_max = 0) %>% map(colnames)
  map2_df(filenames_vector, cols, read_sus)
}

ipData <- load_sus(sus_csvs$ip)
aeData <- load_sus(sus_csvs$ae)
opData <- load_sus(sus_csvs$op)


# List of CCGs (Now in data folder. Previously:)
# setwd(paste0(ifelse(inOffice, "S:/Commissioning Intelligence And Strategy/Strategic Analytics/", "change file path"), "Jonathan Spencer/FrequentFiles/Classification/Organisations/CCG"))

allCCGs <- read_excel("CCG Index.xlsx", sheet = "England") %>%
  filter(CCGActiveDate <= "2014-04-01") %>% 
  # we're still using the old 3x Newcastle CCGs 
  select(CCGCode, CCGDescription, ShortName) %>%
  mutate(CCGDescription  = stringr::str_c(CCGDescription, " CCG"))


# CCG populations for cost charts
ccgPopulation <- read_csv("CCGPopulation.csv", skip = 1)

