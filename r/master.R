############################################################################
" MASTER REDUX"
" CREATE QIPP PACK"
###########################################################################

# TODO ---------------------------------------------------------------
" CHECK THAT WE AGREED TO DISPLAY COST AS 
  cost per unit activity"
# Cost plot can look odd if rounded for low cost activities.


# ***** --------------------------------------------------------------


# Packages ----------------------------------------------------------------
" When code runs clean, replace with tidyverse"
library(tidyverse)
library(readxl)
library(scales, warn.conflicts = FALSE)
library(testthat)
library(extrafont) # for theme_strategy.


# Parameters 1--------------------------------------------------------

inOffice <- TRUE
baseDir  <- ifelse(inOffice, "C:/2017_projects/qipp/", "specify a path")
setwd(baseDir)

active_ccg <- "05L"

f_year     <- 201617

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


# Trend
trendParameters <- tibble(
  Significance = 0.95
  )
trendCV <- qnorm((1 - trendParameters$Significance)/2, lower.tail = FALSE)


# Functions ---------------------------------------------------------------
"Careful with summary functions <- many old parameters exist here"
"some hard coded"

setwd(paste0(baseDir, "r"))
source("roundingAndChartLimitFunctions.R")
source("funnelPlotFunctions.R")
source("trendPlotFunctions.R")
source("costPlotFunctions.R")
source("summaryFunctions.R") 
source("theme_strategy.R")

setwd("C:/2017_projects/funnel/funnel/")
source("funlData.R")

pound <- dollar_format(prefix = "£")


plot_trend <- function(active_df, comparator_df, quote_y, active_y, comparator_y, comparator = T){
  
  p <- ggplot()+
    geom_area(data = active_df,
              aes(
                FYearIntToChar(FYear),
                get(quote_y),
                group = 1
              ),
              alpha = 0.1
              , fill = '#c52828'
              )+
    geom_line(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.8
              , colour = '#c52828'
    )+
    geom_line(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.2
              , colour = '#c52828'
              , size = 2
    )+
    ylim(0, 1.2*max(active_y, comparator_y))+
    theme_strategy()+
    theme(panel.background = element_rect(fill = "white"))+
    labs(x = "Financial Year",
         y = paste0("DSR per ",
                    scales::comma(funnelParameters$RatePerPeople)," population"),
         title = "Trend in Directly Standardised Rate")+
    scale_x_discrete(expand = c(0.025,0.025))

  if(comparator == T){
    
    p + geom_line(data = comparator_df %>% 
                    filter(Type == "Average"),
                  aes(
                    FYearIntToChar(FYear), 
                    get(quote_y),
                    group = 1
                  )
                  # ,linetype = "longdash"
                  , alpha = 0.4
    ) 
  } else {
    p  + geom_line(data = comparator_df,
                   aes(
                     FYearIntToChar(FYear), 
                     get(quote_y),
                     group = 1
                   )
                   # ,linetype = "longdash"
    )
  }
}

plot_trend(plotTrendActive,
           plotTrendComparators,
           "DSRate",
           plotTrendActive$DSRate,
           plotTrendComparators$DSRate)

plot_cost  <- function(df){
  ggplot(df) +
    geom_bar(aes(x = ShortName, y = DSCostsPerHead, fill = IsActiveCCG), stat = "identity") +
    geom_text(
      aes(
        x = ShortName
        , y = 1.01 * DSCostsPerHead # for label
        , family = "Segoe UI Light"
        , label = stringr::str_c("£", df$for_label)
        , hjust = 0
      )
      , size = 3) +
    coord_flip() +
    # scale_fill_manual(values = colourBlindPalette[c("green", "red")] %>% unname) +
    scale_y_continuous(labels = pound, limits = c(0,101)) +
    expand_limits(y = c(min(pretty(df$DSCostsPerHead)), max(pretty(df$DSCostsPerHead))*1.05)) +
    labs(x = NULL, y = NULL, title = "Directly Standardised Costs per head of Population") +
    theme_strategy()+
    theme(legend.position = "none")+
    # theme(panel.grid.major = element_blank())+
    scale_fill_grey(start = 0.6)
}

plot_fun   <- function(df_funnels, df_units){
  
  ggplot(df_funnels) +
    geom_line(aes(x = n, y = fnlLow, group = fnlLimit), linetype = "44") +
    geom_line(aes(x = n, y = fnlHigh, group = fnlLimit), linetype = "44") +
    geom_segment(aes(  x    = min(n)
                       , xend = max(n)
                       , y    = target
                       , yend = target))+
    #geom_hline(aes(yintercept = target)) +
    geom_point(data = df_units, aes(x = DerivedPopulation, y = DSRate, colour = IsActiveCCG), size = 3)+
    scale_x_continuous(labels = scales::comma
                       #, limits = c(0, 1000000) # forced x to 1M
    )+
    scale_y_continuous(labels = scales::comma)+
    theme_strategy()+
    theme(legend.position = "none")+
    labs(
      x = paste0("Standardised population ", FYearIntToChar(f_year))
      , y = paste0("DSR per ", scales::comma(funnelParameters$RatePerPeople)," population")
      , title = paste0("Directly Standardised Rate ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", '#c52828'))
}


convert_dsr_100k <- function(df) {
  if("target" %in% colnames(df)){
    mutate(df, target  = target*100000
           , fnlLow  = fnlLow*100000
           , fnlHigh = fnlHigh*100000)
  } else {
    mutate(df, DSRate = DSRate*100000)
  }
} # For funnel plots

# Colours -----------------------------------------------------------------
# colourBlindPalette <- c(
#     "#000000" #black
#   , "#E69F00" #orange
#   , "#56B4E9" #sky blue
#   , "#009E73" #green
#   , "#F0E442" #yellow
#   , "#0072B2" #blue
#   , "#D55E00" #red
#   , "#CC79A7" #pink
# )
# names(colourBlindPalette) <- c("black", "orange", "sky blue", "green", "yellow", "blue", "red", "pink")


# ***** --------------------------------------------------------------


# Load data ---------------------------------------------------------------
setwd(paste0(baseDir, "data"))

# List of strategies
activeStrategies <- read_csv("listActiveStrategies.csv")
# if run from powershell:
# activeStrategies <- read_csv("listActiveStrategies.csv", col_names = FALSE, skip = 2)
# colnames(activeStrategies) <- read_csv("listActiveStrategies.csv", n_max = 0) %>% colnames

# How many strategies for each type of data

numberOfStrategies <- activeStrategies %>% 
  count(TableType)

"WORK IN PROGRESS :"
# sus_names <- tibble(
#   ip = "IP[0-9]{4}.csv",
#   op = "OP[0-9]{4}.csv",
#   ae = "AE[0-9]{4}.csv"
# )
#  
# filenames <- list.files(pattern = sus_names$ip)

# because of PowerShell: 
# take_names <- map(filenames, read_csv, n_max = 0) %>% 
#   map(., colnames)
# if(length(unique(take_names)) != 1){stop("Inpatient column names are different somewhere.")}
# 
# ipData <- map(
#   filenames,
#   read_csv,
#   col_names = take_names[[1]],
#   na = "NULL",
#   skip = 2
# ) %>% 
#   bind_rows()


# Inpatients
filesToLoad <- list.files(pattern = "Output_SUS_IP[0-9]{4}.csv")
ipDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>% 
  lapply(., colnames)
if(length(unique(ipDataNames)) != 1){stop("Inpatient column names are different somewhere.")}

ipData <- lapply(
  filesToLoad
  , read_csv
  , col_names = ipDataNames[[1]]
  , na = "NULL"
  , skip = 2
  ) %>% bind_rows

rm(filesToLoad, ipDataNames)


# A & E
filesToLoad <- list.files(pattern = "Output_SUS_AE[0-9]{4}.csv")
aeDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>%
  lapply(., colnames)
if(length(unique(aeDataNames)) != 1){stop("A&E column names are different somewhere.")}

aeData <- lapply(
  filesToLoad
  , read_csv
  , col_names = aeDataNames[[1]]
  , na = "NULL"
  , skip = 2
  ) %>% bind_rows


rm(filesToLoad, aeDataNames)

# Outpatients
filesToLoad <- list.files(pattern = "Output_SUS_OP[0-9]{4}.csv")
opDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>%
  lapply(., colnames)
if(length(unique(opDataNames)) != 1){stop("Outpatient column names are different somewhere.")}

opData <- lapply(
  filesToLoad
  , read_csv
  , col_names = opDataNames[[1]]
  , na = "NULL"
  , skip = 2
  ) %>% bind_rows

rm(filesToLoad, opDataNames)


# List of CCGs
setwd(paste0(ifelse(inOffice, "S:/Commissioning Intelligence And Strategy/Strategic Analytics/", "change file path"), "Jonathan Spencer/FrequentFiles/Classification/Organisations/CCG"))

allCCGs <- read_excel("CCG Index.xlsx", sheet = "England") %>%
  filter(CCGActiveDate <= "2014-04-01") %>% 
  # we're still using the old 3x Newcastle CCGs 
  select(CCGCode, CCGDescription, ShortName) %>%
  mutate(CCGDescription  = stringr::str_c(CCGDescription, " CCG"))
  


setwd(paste0(baseDir, "Data"))
# CCG populations for cost charts
# AJ - did manually so no need to remove rows as below for powershell
# ccgPopulation <- read_csv("CCGPopulation.csv", col_names = FALSE, skip = 2)
# colnames(ccgPopulation) <- read_csv("CCGPopulation.csv", n_max = 0) %>% colnames

ccgPopulation <- read_csv("CCGPopulation.csv")

"How relevant are these checks now: modify/update?"
# setwd(paste0(baseDir, "r"))
# source("checks.R") # big and slow


# *****chckpnt**** -----------------------------------------------------------------
# Rerun from here if running multiple packs
setwd(paste0(baseDir, "data"))

# CCG selections
comparatorCCGs2 <- allCCGs %>%
  filter(CCGCode %in% qipp_ccgs,
         CCGCode != active_ccg)
  
activeCCGInfo <- allCCGs %>% 
  filter(CCGCode == active_ccg) %>% 
  mutate(CCGNameMinusCCG = stringr::str_replace(CCGDescription, " CCG", "")) # %>% 
  # unlist()

# Munge data ---------------------------------------------------------------
# setwd(paste0(baseDir, "data"))

comparatorCCGs2 <- allCCGs %>% 
  filter(CCGCode %in% qipp_ccgs)

# Remove rows where the CCGCode is not valid.
removeInvalidCCGs <- . %>%
  inner_join(comparatorCCGs2, by = "CCGCode")


ipSmall <- ipData %>% removeInvalidCCGs
rm(ipData) # RAM saver!
gc() # call after a large object has been removed

aeSmall <- aeData %>% removeInvalidCCGs
opSmall <- opData %>% removeInvalidCCGs %>%
  select(-starts_with("FUF"))

opSmallFUF <- opData %>%
  select(
    DSRate, DSRateVar, DSCosts, DSCostsVar, Attendances, Costs, FYear, CCGCode
    , starts_with("FUF")) %>% removeInvalidCCGs 


# I'm going to call attendances Spells so that I can reuse functions
colnames(aeSmall)    <- gsub("Attendances", "Spells", colnames(aeSmall))
colnames(opSmall)    <- gsub("Attendances", "Spells", colnames(opSmall))
colnames(opSmallFUF) <- gsub("Attendances", "Spells", colnames(opSmallFUF))

"Removed washer for now"

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

opFUFBase <- expand.grid(
  CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
  , Strategy = activeStrategies %>% filter(TableType == "OP" & grepl("^FUF.*", Strategy)) %>% select(Strategy) %>% unlist %>% unname
  , FYear = unique(opData$FYear)
  , stringsAsFactors = FALSE
)

aeBase <- expand.grid(
  CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
  , Strategy = activeStrategies %>% filter(TableType == "AE") %>% select(Strategy) %>% unlist %>% unname
  , FYear = unique(aeData$FYear)
  , stringsAsFactors = FALSE
)

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
ip <- ipSmall %>% process %>% right_join(ipBase, by = c("CCGCode", "Strategy", "FYear"))
op <- opSmall %>% process %>% right_join(opBase, by = c("CCGCode", "Strategy", "FYear"))
ae <- aeSmall %>% process %>% right_join(aeBase, by = c("CCGCode", "Strategy", "FYear"))

opFUFhold <- opSmallFUF %>%
  select(-CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -Spells, -CCGCode, -DSCosts
         , -DSCostsVar, -Costs, -DSRate, -DSRateVar, -FYear, convert = T) %>%
  filter(!is.na(Highlighted)) %>%
  group_by(Strategy, CCGCode, FYear, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , Costs = sum(Costs, na.rm = TRUE)
  ) %>%
  mutate(FUF = ifelse(Highlighted == 1, "First", "FollowUp")) %>%
  select(-Highlighted) 

opFUFSpells <- opFUFhold %>%
  select(-Costs) %>%
  spread(FUF, Spells) %>%
  mutate(
    FUFRatio =  FollowUp / First
    , IsActiveCCG = CCGCode == active_ccg) %>% 
  ungroup()

opFUFCosts <- opFUFhold %>%
  select(-Spells) %>%
  spread(FUF, Costs) %>%
  rename(CostsFirst = First, CostsFollowUp = FollowUp) %>% 
  ungroup()

opFUF <- opFUFBase %>%
  left_join(opFUFSpells, by = c("Strategy", "CCGCode", "FYear")) %>%
  left_join(opFUFCosts, by = c("Strategy", "CCGCode", "FYear"))

stopifnot(nrow(opFUF) == nrow(opFUFBase)) # # strategies * # ccgs * # financial years
rm(opFUFhold, opFUFSpells, opFUFCosts)



# ***** --------------------------------------------------------------


# Funnel ------------------------------------------------------------------
ipFunnelPoints    <- ip    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) 
opFunnelPoints    <- op    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000)
aeFunnelPoints    <- ae    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000)
opFUFFunnelPoints <- opFUF %>% filter(FYear == f_year) 

# ipFunnelSummary <- ipFunnelPoints %>% funnel_summary
# aeFunnelSummary <- aeFunnelPoints %>% funnel_summary
# opFunnelSummary <- opFunnelPoints %>% funnel_summary
# 
# ipFunnelFunnels <- funnel_funnels(ipFunnelSummary, funnelParameters$Smoothness, personYears)
# aeFunnelFunnels <- funnel_funnels(aeFunnelSummary, funnelParameters$Smoothness, personYears)
# opFunnelFunnels <- funnel_funnels(opFunnelSummary, funnelParameters$Smoothness, personYears)
# 
# 
opFunnelSummaryFUF <- opFUF %>%
  ungroup() %>%
  group_by(Strategy) %>%
  summarise(
    AverageFUF = sum(FollowUp, na.rm = TRUE) / sum(First, na.rm = TRUE)
    , ActualMinFirst = min(First, na.rm = TRUE)
    , ActualMaxFirst = max(First, na.rm = TRUE)
    , ActualMinFUFRatio = min(FUFRatio, na.rm = TRUE)
    , ActualMaxFUFRatio = max(FUFRatio, na.rm = TRUE)
  ) %>%
  group_by(Strategy) %>%
  mutate(
    NewMinFirst = chartLimits(ActualMinFirst, ActualMaxFirst)["Min"]
    , NewMaxFirst = chartLimits(ActualMinFirst, ActualMaxFirst)["Max"]
    , NewMinFUFRatio = chartLimits(ActualMinFUFRatio, ActualMaxFUFRatio)["Min"]
    , NewMaxFUFRatio = chartLimits(ActualMinFUFRatio, ActualMaxFUFRatio)["Max"]
  )


opFunnelFunnelsFUF <-
  expand.grid(
    Strategy = unique(opFunnelSummaryFUF$Strategy)
    , RowNumber = seq(1, funnelParameters$Smoothness, 1)
    , stringsAsFactors = FALSE
  ) %>%
  left_join(opFunnelSummaryFUF, by = "Strategy") %>%
  mutate(EventSpells = NA) %>%
  arrange(Strategy)

  for (i in seq(length(opFunnelFunnelsFUF$EventSpells))){
    if(opFunnelFunnelsFUF$RowNumber[i] == 1){
      opFunnelFunnelsFUF$EventSpells[i] <- max(1, opFunnelFunnelsFUF$NewMinFirst[i])
    } else {
      opFunnelFunnelsFUF$EventSpells[i] <-
        max(
          round(
            ((opFunnelFunnelsFUF$NewMaxFirst[i]) /
               opFunnelFunnelsFUF$EventSpells[i - 1]) ^
              (1 / ((funnelParameters$Smoothness + 1) - opFunnelFunnelsFUF$RowNumber[i])) * opFunnelFunnelsFUF$EventSpells[i - 1]
          )
          , opFunnelFunnelsFUF$EventSpells[i - 1] + 1
        )
    }
  }

  opFunnelFunnelsFUF <- opFunnelFunnelsFUF %>%
    mutate(
      Denominator = EventSpells
      , FUIfAverage = Denominator * AverageFUF
      , StandardError =  sqrt(1 / FUIfAverage + 1 / Denominator)
      , ThreeSigmaLower  = (AverageFUF - (3 * StandardError))
      , TwoSigmaLower  = (AverageFUF - (2 * StandardError))
      , TwoSigmaHigher  = (AverageFUF +(2 * StandardError))
      , ThreeSigmaHigher  = (AverageFUF + (3 * StandardError))
    )

# Trend in DSRate ---------------------------------------------------------
ipTrendActive <- ipSmall %>% trend_active 
aeTrendActive <- aeSmall %>% trend_active 
opTrendActive <- opSmall %>% trend_active 

opTrendFUF <- opSmallFUF %>%
  mutate(IsActiveCCG = CCGCode == active_ccg) %>%
  select(-DSCosts, -DSCostsVar, -DSRate, -DSRateVar, -CCGCode, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -Spells, -Costs, -IsActiveCCG, -FYear, convert = T) %>%
  group_by(Strategy, IsActiveCCG, FYear, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    ) %>%
  filter(!is.na(Highlighted)) %>%
  mutate(FUF = ifelse(Highlighted == 1,"First", "FollowUp")) %>%
  select(-Highlighted) %>%
  spread(FUF, Spells) %>%
  mutate(FUFRatio =  FollowUp / First)


ipTrendComparators <- ipSmall %>% trend_comparators 
aeTrendComparators <- aeSmall %>% trend_comparators 
opTrendComparators <- opSmall %>% trend_comparators 

# Cost --------------------------------------------------------------------
ipCost <- ipSmall %>% cost_ds 
aeCost <- aeSmall %>% cost_ds 
opCost <- opSmall %>% cost_ds 

opCostFUF <- opSmallFUF %>%
  filter(FYear == f_year) %>%
  select(-FYear, -DSRate, -DSRateVar, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -Costs, -DSCosts, -DSCostsVar, convert = T) %>%
  group_by(CCGCode, Strategy, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , Costs = sum(Costs, na.rm = TRUE)
    , DSCosts = sum(DSCosts, na.rm = TRUE)
    , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
    ) %>%
  filter(Highlighted == 0) %>%
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
    , DSCostsPerHead = DSCosts / Population
    , DSCostsPerHeadUpper = DSCostsCIUpper / Population
    , DSCostsPerHeadLower = DSCostsCILower / Population
    , IsActiveCCG = CCGCode == active_ccg) %>%
  left_join(allCCGs, by = "CCGCode")

opCost <- opCost %>% bind_rows(opCostFUF)

# ***** --------------------------------------------------------------


# Inpatient plots ---------------------------------------------------------
setwd(baseDir)
ipPlottableStrategies <- activeStrategies %>%
  filter(TableType == "IP") %>%
  filter(Strategy != "Canc_Op_v1") %>%
  filter(Strategy != "Readmissions_v1")

# trendColours <- c(scales::brewer_pal("seq", palette = "Blues")(6)[2:5], colourBlindPalette["red"] %>% unname)
# names(trendColours) <- c("Minimum to 1st decile", "1st decile to 1st quartile", "1st quartile to average", "Average to max", activeCCGInfo$ShortName)

# RColorBrewer::display.brewer.all(colorblindFriendly = T)

plot_ip_fun     <- list()
plot_ip_cost    <- list()
plot_ip_trend   <- list()


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
 
  plotFunnels <- funnel[[1]] %>% convert_dsr_100k()
  plotUnits   <- funnel[[2]] %>% 
    left_join(ipFunnelPoints %>% filter(Strategy == ipPlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k()
  
  plot_ip_fun[[i]] <- plot_fun(plotFunnels, plotUnits)
  
 # plotFunnelPoints <- ipFunnelPoints %>%
 #   filter(Strategy == ipPlottableStrategies$Strategy[i])
 # plotFunnelFunnels <- ipFunnelFunnels %>%
 #   filter(Strategy == ipPlottableStrategies$Strategy[i])
 # plotFunnelSummary <- ipFunnelSummary %>%
 #   filter(Strategy == ipPlottableStrategies$Strategy[i])
 # 
 # 
 # plot_ip_fun[[i]] <- ggplot(data = plotFunnelFunnels) +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "grey40", linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "grey40", linetype = "longdash") +
 #   geom_hline(aes(yintercept = Average)) +
 #   geom_point(
 #     data = plotFunnelPoints
 #     , aes(x = DerivedPopulation, y = DSRate, colour = IsActiveCCG)
 #     , size = 4
 #     , shape = 20
 #   ) +
 #   scale_colour_manual(values = colourBlindPalette[c("blue", "red")] %>% unname) +
 #   scale_x_continuous(
 #     labels = scales::comma
 #      , limits = c(plotFunnelSummary$NewMinDerivedPopulation 
 #                   , plotFunnelSummary$NewMaxDerivedPopulation) 
 #    ) +
 #   scale_y_continuous(
 #     labels = scales::comma
 #     , limits = c(plotFunnelSummary$NewMinDSRate
 #                   , plotFunnelSummary$NewMaxDSRate) 
 #    ) +
 #   labs(
 #     x = paste0("Standardised population ", FYearIntToChar(f_year))
 #     , y = paste0("Direct standardised rate per ", scales::comma(funnelParameters$RatePerPeople)," population")
 #     , title = paste0("Direct standardised rate ", FYearIntToChar(f_year))
 #   ) +
 #   theme(
 #     axis.line = element_line(colour="grey80")
 #     , axis.line.x = element_blank()
 #     , axis.text = element_text(colour = "black")
 #     , axis.ticks = element_line(colour = "black")
 #     , axis.title.y = element_text(size = 10)
 #     , legend.position = "none"
 #     , plot.background = element_blank()
 #     , plot.title = element_text(hjust = 0)
 #     , panel.grid.major.x = element_blank()
 #     , panel.grid.major.y = element_line(colour = "grey95")
 #     , panel.grid.minor = element_blank()
 #     , panel.border = element_blank()
 #     , panel.background= element_blank()
 #   )

 # Draw cost plot ----------------------------------------------------------
  plotCostData <- ipCost %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  
  plotCostFactorLevels <- plotCostData %>% 
    ungroup() %>%
    arrange(desc(DSCostsPerHead)) %>% 
    select(ShortName) %>% unlist %>% unname
  
  plotCostData <- plotCostData %>%
    mutate(ShortName = factor(ShortName, levels = plotCostFactorLevels)) %>% 
    mutate(for_label = format(round(DSCostsPerHead, 1), nsmall = 2))
  
  
  plot_ip_cost[[i]] <- plot_cost(plotCostData)
 
  
# Draw trend plots --------------------------------------------------------

  plotTrendActive <- ipTrendActive %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  plotTrendComparators <- ipTrendComparators %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])

  
  plot_ip_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate)
   
# ***** -----------------------------------------------------
}
rm(
   # plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
  funnel, plotFunnels, plotUnits
   #, plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)


# A&E plots ---------------------------------------------------------------
aePlottableStrategies <- activeStrategies %>%
  filter(TableType == "AE") 

plot_ae_fun   <- list()
plot_ae_cost  <- list()
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
  
  # What happens when you have a rate which does not come directly from (spells/population)?
  # This situation (qipp) works only because the population has been derived (from Spells/DSRate)
  
  plotFunnels <- funnel[[1]] %>% convert_dsr_100k()
  plotUnits   <- funnel[[2]] %>% 
    left_join(aeFunnelPoints %>% filter(Strategy == aePlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k()
  
  plot_ae_fun[[i]] <- plot_fun(plotFunnels, plotUnits)
  
  
  # plotFunnelPoints <- aeFunnelPoints %>%
 #   filter(Strategy == aePlottableStrategies$Strategy[i])
 # plotFunnelFunnels <- aeFunnelFunnels %>%
 #   filter(Strategy == aePlottableStrategies$Strategy[i])
 # plotFunnelSummary <- aeFunnelSummary %>%
 #   filter(Strategy == aePlottableStrategies$Strategy[i])
 # 
 # plot_ae_fun[[i]] <- ggplot(data = plotFunnelFunnels) +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "grey40", linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "grey40", linetype = "longdash") +
 #   geom_hline(aes(yintercept = Average)) +
 #   geom_point(
 #     data = plotFunnelPoints
 #     , aes(x = DerivedPopulation, y = DSRate, colour = IsActiveCCG)
 #     , size = 4
 #     , shape = 20
 #   ) +
 #   scale_colour_manual(values = colourBlindPalette[c("green", "red")] %>% unname) +
 #   scale_x_continuous(
 #     labels = scales::comma
 #      , limits = c(plotFunnelSummary$NewMinDerivedPopulation 
 #                   , plotFunnelSummary$NewMaxDerivedPopulation) 
 #    ) +
 #   scale_y_continuous(
 #     labels = scales::comma
 #     , limits = c(plotFunnelSummary$NewMinDSRate
 #                   , plotFunnelSummary$NewMaxDSRate) 
 #    ) +
 #   labs(
 #     x = paste0("Standardised population ", FYearIntToChar(f_year))
 #     , y = paste0("Direct Standardised Rate per ", scales::comma(funnelParameters$RatePerPeople)," population")
 #     , title = paste0("Direct Standardised Rate ", FYearIntToChar(f_year))
 #   ) +
 #   theme(
 #     axis.line = element_line(colour="grey80")
 #     , axis.line.x = element_blank()
 #     , axis.text = element_text(colour = "black")
 #     , axis.ticks = element_line(colour = "black")
 #     , axis.title.y = element_text(size = 10)
 #     , legend.position = "none"
 #     , plot.background = element_blank()
 #     , panel.grid.major.x = element_blank()
 #     , panel.grid.major.y = element_line(colour = "grey95")
 #     , panel.grid.minor = element_blank()
 #     , panel.border = element_blank()
 #     , panel.background= element_blank()
 #     , plot.title = element_text(hjust = 0)
 #   ) 
 # +
 #   ggsave(
 #     filename = paste0("Images/AE_", aePlottableStrategies$Strategy[i], "_Funnel.png")
 #     , height = 8.9
 #     , width = 13.3
 #     , units = "cm")
 
# Draw cost plot ----------------------------------------------------------
  plotCostData <- aeCost %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])   
 
  plotCostFactorLevels <- plotCostData %>% 
    ungroup() %>%
    arrange(desc(DSCostsPerHead)) %>% 
    select(ShortName) %>% unlist %>% unname

  plotCostData <- plotCostData %>%
    mutate(ShortName = factor(ShortName, levels = plotCostFactorLevels)) %>% 
    mutate(for_label = format(round(DSCostsPerHead, 1), nsmall = 2))
  
  plot_ae_cost[[i]] <-  plot_cost(plotCostData) 

  
# Draw trend plots --------------------------------------------------------
  plotTrendActive <- aeTrendActive %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotTrendComparators <- aeTrendComparators %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  
  plot_ae_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate)
  
  
# ***** ------------------------------------------------------------
}
rm(
  #plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
    funnel, plotFunnels, plotUnits
   #, plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)

# Outpatient plots --------------------------------------------------------
opPlottableStrategies <- activeStrategies %>%
  filter(TableType == "OP") %>%
  filter(!(grepl("^FUF*", Strategy)))

plot_op_fun <- list()
plot_op_cost    <- list()
plot_op_trend   <- list()

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
  
  # What happens when you have a rate which does not come directly from (spells/population)?
  # This situation (qipp) works only because the population has been derived (from Spells/DSRate)
  
  plotFunnels <- funnel[[1]] %>% convert_dsr_100k()
  plotUnits   <- funnel[[2]] %>% 
    left_join(opFunnelPoints %>% filter(Strategy == opPlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k()
  
  plot_op_fun[[i]] <- plot_fun(plotFunnels, plotUnits)
  
  
  
 #   plotFunnelPoints <- opFunnelPoints %>%
 #   filter(Strategy == opPlottableStrategies$Strategy[i])
 # plotFunnelFunnels <- opFunnelFunnels %>%
 #   filter(Strategy == opPlottableStrategies$Strategy[i])
 # plotFunnelSummary <- opFunnelSummary %>%
 #   filter(Strategy == opPlottableStrategies$Strategy[i])
 # 
 # plot_op_fun[[i]] <- ggplot(data = plotFunnelFunnels) +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "grey40", linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = "longdash") +
 #   geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "grey40", linetype = "longdash") +
 #   geom_hline(aes(yintercept = Average)) +
 #   geom_point(
 #     data = plotFunnelPoints
 #     , aes(x = DerivedPopulation, y = DSRate, colour = IsActiveCCG)
 #     , size = 4
 #     , shape = 20
 #   ) +
 #   scale_colour_manual(values = colourBlindPalette[c("sky blue", "red")] %>% unname) +
 #   scale_x_continuous(
 #     labels = scales::comma
 #      , limits = c(plotFunnelSummary$NewMinDerivedPopulation 
 #                   , plotFunnelSummary$NewMaxDerivedPopulation) 
 #    ) +
 #   scale_y_continuous(
 #     labels = scales::comma
 #     , limits = c(plotFunnelSummary$NewMinDSRate
 #                   , plotFunnelSummary$NewMaxDSRate) 
 #    ) +
 #   labs(
 #     x = paste0("Standardised population ", FYearIntToChar(f_year))
 #     , y = paste0("Direct Standardised Rate per ", scales::comma(funnelParameters$RatePerPeople)," population")
 #     , title = paste0("Direct Standardised Rate ", FYearIntToChar(f_year))
 #   ) +
 #   theme(
 #     axis.line = element_line(colour="grey80")
 #     , axis.line.x = element_blank()
 #     , axis.text = element_text(colour = "black")
 #     , axis.ticks = element_line(colour = "black")
 #     , axis.title.y = element_text(size = 10)
 #     , legend.position = "none"
 #     , plot.background = element_blank()
 #     , panel.grid.major.x = element_blank()
 #     , panel.grid.major.y = element_line(colour = "grey95")
 #     , panel.grid.minor = element_blank()
 #     , panel.border = element_blank()
 #     , panel.background= element_blank()
 #     , plot.title = element_text(hjust = 0)
 #   ) 
 # +
 #   ggsave(
 #     filename = paste0("Images/OP_", opPlottableStrategies$Strategy[i], "_Funnel.png")
 #     , height = 8.9
 #     , width = 13.3
 #     , units = "cm")
 # 

# Draw cost plot ----------------------------------------------------------
  plotCostData <- opCost %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plotCostFactorLevels <- plotCostData %>% 
    ungroup() %>%
    arrange(desc(DSCostsPerHead)) %>% 
    select(ShortName) %>% unlist %>% unname
  
  plotCostData <- plotCostData %>%
    mutate(ShortName = factor(ShortName, levels = plotCostFactorLevels)) %>% 
    mutate(for_label = format(round(DSCostsPerHead, 1), nsmall = 2))
  
  
  plot_op_cost[[i]] <- plot_cost(plotCostData)
  
# Draw trend plots --------------------------------------------------------
  plotTrendActive <- opTrendActive %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotTrendComparators <- opTrendComparators %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plot_op_trend[[i]] <- plot_trend(plotTrendActive,
                                   plotTrendComparators,
                                   "DSRate",
                                   plotTrendActive$DSRate,
                                   plotTrendComparators$DSRate)
  
# ***** ---------------------------------------------------
}
rm(
  #plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
   funnel, plotFunnels, plotUnits
   #, plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)

# FUF plots ---------------------------------------------------------------
opPlottableFUFStrategies <- activeStrategies %>%
  filter(TableType == "OP") %>%
  filter((grepl("^FUF*", Strategy)))

plot_fuf_fun   <- list()
plot_fuf_cost  <- list()
plot_fuf_trend <- list()

for(i in seq(opPlottableFUFStrategies$Strategy)){

# Draw funnel plot --------------------------------------------------------
  
  plotFunnelPoints <- opFUFFunnelPoints %>%
   filter(Strategy == opPlottableFUFStrategies$Strategy[i])
 plotFunnelFunnels <- opFunnelFunnelsFUF %>%
   filter(Strategy == opPlottableFUFStrategies$Strategy[i])
 plotFunnelSummary <- opFunnelSummaryFUF %>%
   filter(Strategy == opPlottableFUFStrategies$Strategy[i])
 
 plot_fuf_fun[[i]] <- ggplot(data = plotFunnelFunnels) +
   geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "black", linetype =  44) +
   geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = 44) +
   geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = 44) +
   geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "black", linetype =  44) +
   # geom_hline(aes(yintercept = AverageFUF)) +
   geom_segment(aes(  x      = min(Denominator)
                      , xend = max(Denominator)
                      , y    = AverageFUF
                      , yend = AverageFUF))+
   geom_point(
     data = plotFunnelPoints
     , aes(x = First, y = FUFRatio, colour = IsActiveCCG)
     , size = 3
   )+
   scale_x_continuous(labels = scales::comma)+
   scale_y_continuous(limits = c(0.8*min(plotFunnelPoints$FUFRatio)
                                 , 1.2*max(plotFunnelPoints$FUFRatio)
                                 )
                      , labels = scales::comma)+
   theme_strategy()+
   theme(legend.position = "none")+
   labs(
     x = paste0("First appointments ", FYearIntToChar(f_year))
     , y = "Ratio of follow-ups to first"
     , title = paste0("Ratio of Follow-ups to First Appointments ", FYearIntToChar(f_year))
   )+
   scale_color_manual(values = c("grey70", '#c52828'))


# Draw cost plot ----------------------------------------------------------
  plotCostData <- opCostFUF %>%
    filter(Strategy == opPlottableFUFStrategies$Strategy[i])
  
  plotCostFactorLevels <- plotCostData %>% 
    ungroup() %>%
    arrange(desc(DSCostsPerHead)) %>% 
    select(ShortName) %>% unlist %>% unname
  
  plotCostData <- plotCostData %>%
    mutate(ShortName = factor(ShortName, levels = plotCostFactorLevels)) %>% 
    mutate(for_label = format(round(DSCostsPerHead, 1), nsmall = 2))
  
  
  plot_fuf_cost[[i]] <- plot_cost(plotCostData)
 
# Draw trend plots --------------------------------------------------------
"May have to look into this. IsActiveCCG == False may be all other CCGs?"
  plotTrendActive <- opTrendFUF %>%
    filter(Strategy == opPlottableFUFStrategies$Strategy[i])
   # plotTrendComparators <- opTrendComparators %>%
   #   filter(Strategy == opPlottableFUFStrategies$Strategy[i])
  
  plot_fuf_trend[[i]] <-   plot_trend(plotTrendActive %>% filter(IsActiveCCG == T),
                                      plotTrendActive %>% filter(IsActiveCCG == F),
                                      "FUFRatio",
                                      plotTrendActive$FUFRatio,
                                      plotTrendActive$FUFRatio, # or 0,
                                      F # for comparator
                                      )
  
  
# ***** --------------------------------------------------------
}
rm(plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
   #, plotFUFRocPoints, plotFUFRocFunnels, plotFUFRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive
   , i)

detach("package:testthat", unload=TRUE)
library(dplyr)

# Summary csv -------------------------------------------------------------
setwd(baseDir)
# Inpatient ---------------------------------------------------------------
totalActivityIP <- ipSmall %>% total_activity
savingsAnyOneIP <- ipTrendComparators %>% savings_any_one

ipSignificance <- significance_summary(ipFunnelPoints, ipFunnelFunnels, ipRoC, ipRoCFunnels)

summaryOutputIP <- ipSmall %>% summary_output(., savingsAnyOneIP, ipSignificance, totalActivityIP) %>%
  mutate(
    StrategyDescription = ifelse(StrategyID %in% c(25,26,27,91,92,93,104,105)
                                 , paste0(StrategyDescription, "\U2020") # we have to bodge this in XL
                                 , StrategyDescription)) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(ipTrendActive %>% 
    filter(FYear == 201415) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(ipCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))


# write.table(summaryOutputIP, "Data/R_SummaryOutputIP.csv", sep = ",", row.names = FALSE)
# write.table(summaryOutputIP, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputIP.csv"), sep = ",", row.names = FALSE)

# A&E ---------------------------------------------------------------------
totalActivityAE <- aeSmall %>% total_activity
savingsAnyOneAE <- aeTrendComparators %>% savings_any_one
aeSignificance <- significance_summary(aeFunnelPoints, aeFunnelFunnels, aeRoC, aeRoCFunnels)
summaryOutputAE <- aeSmall %>% summary_output(., savingsAnyOneAE, aeSignificance, totalActivityAE) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(aeTrendActive %>% 
    filter(FYear == 201415) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(aeCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))

#write.table(summaryOutputAE, "Data/R_SummaryOutputAE.csv", sep = ",", row.names = FALSE)
#write.table(summaryOutputAE, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputAE.csv"), sep = ",", row.names = FALSE)


# Outpatients -------------------------------------------------------------
totalActivityOP <- opSmall %>% total_activity
savingsAnyOneOP <- opTrendComparators %>% savings_any_one
opSignificance <- significance_summary(opFunnelPoints, opFunnelFunnels, opRoC, opRoCFunnels)
summaryOutputOP <- opSmall %>% summary_output(., savingsAnyOneOP, opSignificance, totalActivityOP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(opTrendActive %>% 
    filter(FYear == 201415) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(opCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))


totalActivityOPFUF <- opSmallFUF %>% total_activity

spendFUF <- opCostFUF %>%
  filter(CCGCode == active_ccg) %>%
  select(Strategy, Costs)

opTopFUF <- opSmallFUF %>%
  select(-DSCosts, -DSCostsVar, -DSRate, -DSRateVar, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -Spells, -Costs, -CCGCode, -FYear, convert = T) %>%
  group_by(Strategy, CCGCode, FYear, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    ) %>%
  filter(!is.na(Highlighted)) %>%
  mutate(FUF = ifelse(Highlighted == 1,"First", "FollowUp")) %>%
  select(-Highlighted) %>%
  spread(FUF, Spells) %>%
  mutate(FUFRatio =  FollowUp / First) %>%
  filter(FYear == f_year) %>%
  group_by(Strategy) %>%
  summarise(
    Average = sum(FollowUp, na.rm = TRUE) / sum(First, na.rm = TRUE)
    , TopQuartile = quantile(FUFRatio, 0.25, na.rm = TRUE)
    , TopDecile = quantile(FUFRatio, 0.1, na.rm = TRUE)
    #, MaxFUFRatio = max(FUFRatio, na.rm = TRUE)
    #, MinFUFRatio = min(FUFRatio, na.rm = TRUE)
  ) 


savingsAnyOneOPFUF <- opTrendFUF %>% 
  filter(FYear == f_year, IsActiveCCG) %>%
  left_join(spendFUF, by = "Strategy") %>%
  select(-IsActiveCCG) %>%
  left_join(opTopFUF, by = "Strategy") %>%
  mutate(
    SavingsIfAverage = Costs - (Average / FUFRatio) * Costs 
    , SavingsIfTopQuartile = Costs - (TopQuartile / FUFRatio) * Costs
    , SavingsIfTopDecile = Costs - (TopDecile / FUFRatio) * Costs
  ) %>%
  ungroup() %>%
  select(Strategy, SavingsIfAverage, SavingsIfTopQuartile, SavingsIfTopDecile) %>%
  mutate(
    SavingsIfAverage = ifelse(SavingsIfAverage < 0, 0, SavingsIfAverage)
    , SavingsIfTopQuartile = ifelse(SavingsIfTopQuartile < 0, 0, SavingsIfTopQuartile)
    , SavingsIfTopDecile = ifelse(SavingsIfTopDecile < 0, 0, SavingsIfTopDecile)
  )


opSignificanceFUF <- opFUFFunnelPoints %>% 
  filter(CCGCode == active_ccg) %>%
  left_join(opFunnelFunnelsFUF, by = "Strategy") %>%
  group_by(Strategy) %>%
  mutate(Test = First - Denominator
         , AbsTest = abs(Test)
         , TestRank = rank(AbsTest, ties.method = "first")) %>%
  filter(TestRank == 1) %>%
  mutate(Significance = ifelse(FUFRatio > TwoSigmaHigher, "High", 
                      ifelse(FUFRatio < TwoSigmaLower, "Low", "Not Significant"))) %>%
  select(CCGCode, Strategy, Significance) 

  opRocFUF2 <- opRocFUF %>%
    filter(CCGCode == active_ccg) %>% ungroup() %>% select(-CCGCode)
  opRocFunnelsFUF2 <- opRocFunnelsFUF %>% select(-FYear)

opSignificanceFUF <- opSignificanceFUF %>%
  left_join(opRocFUF2, by = "Strategy") %>%
  left_join(opRocFunnelsFUF2, by = "Strategy") %>%
  mutate(Test = FirstInBaseYear - Denominator
         , AbsTest = abs(Test)
         , TestRank = rank(AbsTest, ties.method = "first")) %>%
  filter(TestRank == 1) %>%
  mutate(
    RocSignificance = ifelse(RateOfChange > TwoSigmaHigher, "High"
                        , ifelse(RateOfChange < TwoSigmaLower, "Low", "Not Significant"))
    , ReviewNumber = ifelse(Significance == "Low", 1
                     , ifelse(Significance == "Not Significant", 2
                      , ifelse(Significance == "High", 3))) + 
                    ifelse(RocSignificance == "Low", 0
                     , ifelse(RocSignificance == "Not Significant", 3
                      , ifelse(RocSignificance == "High", 6)))
    , ReviewGroup = ifelse(ReviewNumber <= 3, "Review"
                     , ifelse(ReviewNumber == 8, "Close monitoring"
                      , ifelse(ReviewNumber %in% c(4, 5, 7), "Background monitoring", "Explore")))
  ) %>%
  select(CCGCode, Strategy, Significance, RocSignificance, ReviewNumber, ReviewGroup) 


summaryOutputOPFUF <- 
  opTrendFUF %>%
  filter(FYear == f_year & IsActiveCCG) %>%
  select(-IsActiveCCG) %>%
  left_join(opTopFUF, by = "Strategy") %>%
  left_join(savingsAnyOneOPFUF, by = "Strategy") %>%
  left_join(opSignificanceFUF, by = "Strategy") %>%
  left_join(activeStrategies, by = "Strategy") %>%
  left_join(spendFUF, by = c("Strategy", "CCGCode")) %>%
  mutate(
    SpellsRounded = round(FollowUp, -0.5) #FollowUp_Rounded
    , Costs_Rounded = round(Costs, -3)
    , Average_SavingsIf_Rounded = roundTo(SavingsIfAverage, 1000)
    , TopQuartile_SavingsIf_Rounded = roundTo(SavingsIfTopQuartile, 1000)
    , TopDecile_SavingsIf_Rounded = roundTo(SavingsIfTopDecile, 1000)
  ) %>%
  ungroup() %>%
  select(-IsActiveCCG)


# write.table(summaryOutputOP, "Data/R_SummaryOutputOP.csv", sep = ",", row.names = FALSE)
# write.table(summaryOutputOPFUF, "Data/R_SummaryOutputOPFUF.csv", sep = ",", row.names = FALSE)
# write.table(summaryOutputOP, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputOP.csv"), sep = ",", row.names = FALSE)
# write.table(summaryOutputOPFUF, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputOPFUF.csv"), sep = ",", row.names = FALSE)
# 
# comparatorsOut <- comparatorCCGs %>%
#   filter(NeighbourCCGCode != activeCCG) %>%
#   select(ClosenessRank, NeighbourCCGCode, CCGDescription)

comparatorsOut <- comparatorCCGs2 %>%
  filter(CCGCode != active_ccg) %>%
  select(CCGCode, CCGDescription)
# messes with the comparators table in Excel

# write.table(comparatorsOut, "Data/R_ComparatorCCGs.csv", sep = ",", row.names = FALSE)
# write.table(comparatorsOut, paste0("Data/ByCCG/R_", active_ccg, "ComparatorCCGs.csv"), sep = ",", row.names = FALSE)

#  -------------------------------------------------------------------


cat("Ends")
activeCCGInfo$CCGDescription

