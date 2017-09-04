############################################################################
" MASTER REDUX"
" CREATE QIPP PACK"
###########################################################################

# ***** --------------------------------------------------------------
"Use package check system? See email"

"Seems to be unaffected by update to dplyr 0.7 etc."


# Packages ----------------------------------------------------------------

library(readxl)
library(scales, warn.conflicts = FALSE)
library(testthat)
library(extrafont) # for theme_strategy.
library(stringr)
library(ReporteRs)
library(tidyverse)
library(ggrepel)

# Parameters 1--------------------------------------------------------

baseDir  <- "C:/2017_projects/qipp/"

active_ccg <- "05L"
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
              )
              , alpha = 0.4
              , fill = "#ec6555" # - SWB Red # '#c52828' - original red
              )+
    geom_line(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.8
              , colour = "#ec6555"
              , size = 1
    )+
    # geom_line(data = active_df,
    #           aes(
    #             FYearIntToChar(FYear), 
    #             get(quote_y),
    #             group = 1
    #           ),
    #           alpha = 0.2
    #           , colour = '#c52828'
    #           , size = 2
    # )+
    ylim(0, 1.2*max(active_y, comparator_y))+
    theme_strategy()+
    theme(panel.background = element_rect(fill = "white"),
          # plot.subtitle = element_text(face = "italic"),
          axis.title = element_blank())+
    labs(y = paste0("DSR per ",
                    scales::comma(funnelParameters$RatePerPeople)," population"),
         title = paste0("Trend in Directly Standardised Rate, ",
                        str_sub(rocParameters$From ,1, 4), "/",
                        str_sub(rocParameters$From ,5, 6), " to "
                        , str_sub(rocParameters$To ,1, 4), "/",
                        str_sub(rocParameters$To ,5, 6))
         , subtitle = "Directly Standardised Rate per 100k population [Vertical Axis]")+
    scale_x_discrete(expand = c(0.0,0.0))

  if(comparator == T){
    
    p + geom_line(data = comparator_df %>% 
                    filter(Type == "Average"),
                  aes(
                    FYearIntToChar(FYear), 
                    get(quote_y),
                    group = 1
                  )
                  # ,linetype = "longdash"
                  , alpha = 0.8
    ) 
  } else {
    p  + geom_line(data = comparator_df,
                   aes(
                     FYearIntToChar(FYear), 
                     get(quote_y),
                     group = 1
                   )
                   # ,linetype = "longdash"
                   , alpha =0.8
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
    geom_text(data = df_funnels
              , aes(x = 0.75*max(n), y = min(fnlLow, df_units$DSRate), label = "Standardised population 2016/17")
              # , vjust = "bottom"
              # , hjust = "right"
              , family = "Segoe UI"
              , size  = 3
              , fontface  = "plain"
              , color = "grey40"
               )+
    geom_text_repel(data = df_units
      , aes(x = DerivedPopulation
            , y = DSRate
            , label = ccg_label
           )
      , alpha = 0.5

      #, fontface = 'bold'
      , size = 3
      # box.padding = unit(0.25, "lines"),
      # point.padding = unit(0.5, "lines")
      # , nudge_y = -0.00050
      ) +
    scale_x_continuous(labels = scales::comma
                       #, limits = c(0, 1000000) # forced x to 1M
    )+
    scale_y_continuous(labels = scales::comma)+
    theme_strategy()+
    theme(legend.position = "none"
          , axis.title = element_blank()
          #, plot.subtitle = element_text(face = "italic")
          )+
    labs(
      x = paste0("Standardised population ", FYearIntToChar(f_year))
      , subtitle = paste0("DSR per 100k population [Vertical Axis]")  # , scales::comma(funnelParameters$RatePerPeople)," population")
      , title = paste0("Directly Standardised Rate, ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", '#c52828'))

}

x <- plot_fun(plotFunnels, plotUnits)

convert_dsr_100k <- function(df) { # For DSR funnel
  if("target" %in% colnames(df)){
    mutate(df, target  = target*100000
           , fnlLow  = fnlLow*100000
           , fnlHigh = fnlHigh*100000)
  } else {
    mutate(df, DSRate = DSRate*100000)
  }
} 

plot_roc <- function(funnel_df, points_df, summary_df){
  
  ggplot(data = funnel_df) +
    geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "black", linetype =  44) +
    geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = 44) +
    geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = 44) +
    geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "black", linetype =  44) +
    geom_segment(aes(x      = min(Denominator)
                     , xend = max(Denominator)
                     , y    = AverageRateOfChange
                     , yend = AverageRateOfChange))+
    geom_point(
      data = points_df
      , aes(x = SpellsInBaseYear,
            y = RateOfChange, colour = IsActiveCCG)
      , size = 3
    )+
    geom_text(data = summary_df
              , aes(x = max(NewMaxSpells), y = min(NewMinRateOfChange), label = "Related Spells 2012/13")
              , vjust = "bottom"
              , hjust = "right"
              , family = "Segoe UI"
              , size  = 3
              , fontface  = "plain"
              , color = "grey40"
    )+
    geom_text_repel(data = points_df
                    , aes(x = SpellsInBaseYear,
                          y = RateOfChange
                          , label = ccg_label
                    )
                    , alpha = 0.5
                    #, fontface = 'bold'
                    , size = 3
                    # box.padding = unit(0.25, "lines"),
                    # point.padding = unit(0.5, "lines")
                    # , nudge_y = -0.00050
    ) +
    scale_x_continuous(
      labels = scales::comma
      , limits = c(summary_df$NewMinSpells, summary_df$NewMaxSpells)) +
    scale_y_continuous(
      labels = scales::percent
      , limits = c(summary_df$NewMinRateOfChange, summary_df$NewMaxRateOfChange)) +
    labs(
      x = paste0("Related spells "
                 , points_df %>% 
                   filter(IsActiveCCG) %>% 
                   ungroup() %>% 
                   select(From) %>% 
                   unlist %>% unname %>% 
                   FYearIntToChar)
      , y = paste0("Percentage change")
      , title = 
        paste0("Rate of Change between "
               , points_df %>% 
                 filter(IsActiveCCG) %>% 
                 ungroup() %>% 
                 select(From) %>% 
                 unlist %>% unname %>% 
                 FYearIntToChar
               , " and "
               , points_df %>% 
                 filter(IsActiveCCG) %>% 
                 ungroup() %>% 
                 select(FYear) %>% 
                 unlist %>% unname %>% 
                 FYearIntToChar)
    ) +
    # scale_y_continuous(limits = c(0.8*min(plotRocPoints$RateOfChange)
    #                               , 1.2*max(plotRocPoints$RateOfChange)
    # )
    # , labels = scales::comma)+
    theme_strategy()+
    theme(legend.position = "none",
          axis.title = element_blank()
          # plot.subtitle = element_text(face = "italic"),
          # panel.background = element_rect(fill = "#E6E6FA")
          )+
    labs(
      # x = paste0("Rate of Change between ", FYearIntToChar(f_year))
      subtitle = "Percentage change in Directly Standardised Rate [Vertical Axis]"
      #, title = paste0("Ratio of Follow-ups to First Appointments ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", '#c52828'))
  
  #69D2E7,#A7DBD8,#E0E4CC,#F38630,#FA6900,#69D2E7,#A7DBD8,#E0E4CC
  #"grey70", '#c52828'
  #"#69D2E7", '#FA6900'
}

plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary)

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

# List of strategies
activeStrategies <- read_csv("listActiveStrategies.csv")
# if run from powershell:
# activeStrategies <- read_csv("listActiveStrategies.csv", col_names = FALSE, skip = 2)
# colnames(activeStrategies) <- read_csv("listActiveStrategies.csv", n_max = 0) %>% colnames

# How many strategies for each type of data

numberOfStrategies <- activeStrategies %>% 
  count(TableType)


# LOAD SUS - COULD BE MORE EFFICIENT IF DO ONE FUNCTION AND THEN CALL IP, AE...
sus_names <- tibble(ip = "IP[0-9]{4}.csv", op = "OP[0-9]{4}.csv", ae = "AE[0-9]{4}.csv")

tmp <- map(sus_names, function(x) list.files(pattern = x))

# because of PowerShell: 
cols_ip <- map(tmp$ip, read_csv, n_max = 0) %>% map(colnames)
cols_op <- map(tmp$op, read_csv, n_max = 0) %>% map(colnames)
cols_ae <- map(tmp$ae, read_csv, n_max = 0) %>% map(colnames)

read_sus <- function(filename, col_headers){
  
  read_csv(filename, col_headers, na = "NULL", skip = 2)
}

ipData <- map2_df(tmp$ip, cols_ip, read_sus)
aeData <- map2_df(tmp$ae, cols_ae, read_sus)
opData <- map2_df(tmp$op, cols_op, read_sus)

# Just df does row whereas providing a vector sus_names$regex does col.

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


# # Inpatients
# filesToLoad <- list.files(pattern = "Output_SUS_IP[0-9]{4}.csv")
# ipDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>% 
#   lapply(., colnames)
# if(length(unique(ipDataNames)) != 1){stop("Inpatient column names are different somewhere.")}
# 
# ipData <- lapply(
#   filesToLoad
#   , read_csv
#   , col_names = ipDataNames[[1]]
#   , na = "NULL"
#   , skip = 2
#   ) %>% bind_rows
# 
# rm(filesToLoad, ipDataNames)
# 
# 
# # A & E
# filesToLoad <- list.files(pattern = "Output_SUS_AE[0-9]{4}.csv")
# aeDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>%
#   lapply(., colnames)
# if(length(unique(aeDataNames)) != 1){stop("A&E column names are different somewhere.")}
# 
# aeData <- lapply(
#   filesToLoad
#   , read_csv
#   , col_names = aeDataNames[[1]]
#   , na = "NULL"
#   , skip = 2
#   ) %>% bind_rows
# 
# rm(filesToLoad, aeDataNames)
# 
# # Outpatients
# filesToLoad <- list.files(pattern = "Output_SUS_OP[0-9]{4}.csv")
# opDataNames <- lapply(filesToLoad, read_csv, n_max = 0) %>%
#   lapply(., colnames)
# if(length(unique(opDataNames)) != 1){stop("Outpatient column names are different somewhere.")}
# 
# opData <- lapply(
#   filesToLoad
#   , read_csv
#   , col_names = opDataNames[[1]]
#   , na = "NULL"
#   , skip = 2
#   ) %>% bind_rows
# 
# rm(filesToLoad, opDataNames)


# List of CCGs
# setwd(paste0(ifelse(inOffice, "S:/Commissioning Intelligence And Strategy/Strategic Analytics/", "change file path"), "Jonathan Spencer/FrequentFiles/Classification/Organisations/CCG"))

allCCGs <- read_excel("CCG Index.xlsx", sheet = "England") %>%
  filter(CCGActiveDate <= "2014-04-01") %>% 
  # we're still using the old 3x Newcastle CCGs 
  select(CCGCode, CCGDescription, ShortName) %>%
  mutate(CCGDescription  = stringr::str_c(CCGDescription, " CCG"))
  


#setwd(paste0(baseDir, "Data"))
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
# setwd(paste0(baseDir, "data"))

# CCG selections
comparatorCCGs2 <- allCCGs %>%
  filter(CCGCode %in% qipp_ccgs,
         CCGCode != active_ccg)
  
activeCCGInfo <- allCCGs %>% 
  filter(CCGCode == active_ccg) %>% 
  mutate(CCGNameMinusCCG = stringr::str_replace(CCGDescription, " CCG", "")) # %>% 
  # unlist()



# What years do we want to run the rate of change for?
# activeMinMaxIp <- ipCheck %>% 
#   filter(CCGCode == active_ccg & !is.na(Spells)) %>%
#   group_by(Strategy) %>%
#   summarise(
#     From = min(FYear, na.rm = TRUE)
#     , To = max(FYear, na.rm = TRUE)
#   ) %>%
#   gather(Type, FYear, -Strategy, convert = TRUE)
#   
# activeMinMaxOp <- opCheck %>% 
#   filter(CCGCode == active_ccg & !is.na(Attendances)) %>%
#   group_by(Strategy) %>%
#   summarise(
#     From = min(FYear, na.rm = TRUE)
#     , To = max(FYear, na.rm = TRUE)
#   ) %>%
#   gather(Type, FYear, -Strategy, convert = TRUE)
# 
# activeMinMaxAe <- aeCheck %>% 
#   filter(CCGCode == active_ccg & !is.na(Attendances)) %>%
#   group_by(Strategy) %>%
#   summarise(
#     From = min(FYear, na.rm = TRUE)
#     , To = max(FYear, na.rm = TRUE)
#   ) %>%
#   gather(Type, FYear, -Strategy, convert = TRUE)
#   
#   
# # Make sure that we're not missing any strategies where allll years are NA
# activeMinMaxIp <- activeStrategies %>%
#   filter(TableType == "IP") %>%
#   select(Strategy) %>%
#   left_join(activeMinMaxIp, by = "Strategy")
# 
# activeMinMaxOp <- activeStrategies %>%
#   filter(TableType == "OP") %>%
#   select(Strategy) %>%
#   left_join(activeMinMaxOp, by = "Strategy")
#   
# activeMinMaxAe <- activeStrategies %>%
#   filter(TableType == "AE") %>%
#   select(Strategy) %>%
#   left_join(activeMinMaxAe, by = "Strategy")
#   
# 
# # This is a list of the Strategies to totally exlude
# activeIPExclude <- activeMinMaxIp %>%
#   filter(is.na(FYear)) %>%
#   select(Strategy) %>% unlist %>% unname
# 
# activeOPExclude <- activeMinMaxOp %>%
#   filter(is.na(FYear)) %>%
#   select(Strategy) %>% unlist %>% unname
# 
# activeAEExclude <- activeMinMaxAe %>%
#   filter(is.na(FYear)) %>%
#   select(Strategy) %>% unlist %>% unname
# 
# 
# 


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
ipFunnelPoints    <- ip    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
opFunnelPoints    <- op    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
aeFunnelPoints    <- ae    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
opFUFFunnelPoints <- opFUF %>% filter(FYear == f_year) 


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
  

# Roc ----------------------------------------------------------
  setwd(paste0(baseDir, "r"))
  
   source("rateOfChangePlotFunctions.R")
  
  ## AmbNoInvNoTreat has no results anywhere in 200910 or 201011
  #checkAmbNoInvNoTreat <- aeData %>%
  #  filter(AmbNoInvNoTreat_v1 == 1) %>%
  #  group_by(CCGCode, FYear) %>%
  #  summarise(Attendances = sum(Attendances, na.rm = TRUE))
  
  ## So we need to create some exceptions
  
  # rocExceptions <- data.frame( # I will fill this with any exceptions to the rules.
  #   CCGCode = character()
  #   , Strategy = character()
  #   , From = integer()
  #   , To = integer()
  #   , stringsAsFactors = FALSE
  # )
  # # 
  # 
  #   source("opplcvExceptions.R")
  # #
  # rocExceptions <- rbind(
  #   rocExceptions, 
  #   (allCCGs %>% 
  #      select(CCGCode) %>%
  #      mutate(
  #        Strategy = "AmbNoInvNoTreat_v1"
  #        , From = 201213
  #        , To=  201617))
  #   , (activeStrategies %>% 
  #        filter(Strategy != "AmbNoInvNoTreat_v1") %>%
  #        mutate(
  #          CCGCode = "03J" # North Kirklees
  #          , From = 201213
  #          , To =  201617) %>%
  #        select(CCGCode, Strategy, From, To))
  #   , (activeStrategies %>% 
  #        filter(Strategy != "AmbNoInvNoTreat_v1") %>%
  #        mutate(
  #          CCGCode = "03R" #Wakefield
  #          , From = 201213
  #          , To =  201617) %>%
  #        select(CCGCode, Strategy, From, To))) %>% 
  #   bind_rows(opplcvExceptions)
  
  ## Amendments to Jonathan's original code in the line above:
  ## CHANGE OF YEAr
  # "BUT MAY NOT NECESSARY. AT SOME POINT WILL HAVE TO UNDERSTAND"
  
  
  #General method to add a single exception:
  #rocExceptions[nrow(rocExceptions) + 1, ] <- list("05X", "FrequentFlyers_v1", 200910, 201415)
  
  ####
  
  ipRoCAll <- ip %>% roc_all 
  ipRoCAll <- ipRoCAll %>% left_join(
    ipRoCAll %>% 
      select(CCGCode, Strategy, FYear, Spells) %>%
      rename(SpellsInBaseYear = Spells, From = FYear) 
    , by = c("CCGCode", "Strategy", "From")) %>% 
    unique.data.frame() # to remove duplicates from incl. base yr in roc_all

# tmp   <- ip %>% roc_all  %>% filter(CCGCode == "13P", Strategy == "ACS_Acute_v3")
# tmp_a <- tmp %>% left_join(
#   tmp %>% 
#     filter(CCGCode == "13P", Strategy == "ACS_Acute_v3") %>% 
#     select(CCGCode, Strategy, FYear, Spells) %>%
#     rename(SpellsInBaseYear = Spells, From = FYear) 
#   , by = c("CCGCode", "Strategy", "From"))
#   
# tmp_c <- ip %>% filter(FYear == 201213) %>% rename(SpellsInBaseYear = Spells)
#   
# tmp_d <- tmp_a %>% left_join(tmp_c, by = c("CCGCode", "Strategy", "From" = "FYear"))
  
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
  
  
  
  # opRocFUF <- opSmallFUF %>%
  #   select(-DSCosts, -DSCostsVar, -Costs, -DSRate, -DSRateVar, -CCGDescription, -ShortName) %>%
  #   gather(Strategy, Highlighted, -Spells, -FYear, -CCGCode, convert = T ) %>%
  #   group_by(Strategy, CCGCode, FYear, Highlighted) %>%
  #   summarise(Spells = sum(Spells, na.rm = TRUE)) %>%
  #   filter(!is.na(Highlighted)) %>%
  #   mutate(FUF = ifelse(Highlighted == 1, "First", "FollowUp")) %>%
  #   select(-Highlighted) %>%
  #   spread(FUF, Spells) %>%
  #   mutate(
  #     FUFRatio =  FollowUp / First
  #     , IsActiveCCG = CCGCode == active_ccg) %>%
  #   group_by(CCGCode, Strategy, add = FALSE) %>%
  #   left_join(rocExceptions, by = c("CCGCode", "Strategy", "FYear" = "From")) %>%
  #   left_join(rocExceptions, by = c("CCGCode", "Strategy", "FYear" = "To")) %>%
  #   mutate(IsException = !is.na(From)|!is.na(To)) %>%
  #   select(-From, -To) %>%
  #   filter(
  #     (FYear == rocParameters$From | FYear == rocParameters$To)
  #     | IsException 
  #   ) %>% 
  #   left_join(rocExceptions, by = c("CCGCode", "Strategy")) %>%
  #   filter(IsException | is.na(From)) %>%
  #   select(-From, -To) %>% 
  #   mutate(
  #     RateOfChange = FUFRatio - lag(FUFRatio, 1)
  #     , FUFInBaseYear = lag(FUFRatio, 1)
  #     , FirstInBaseYear = lag(First, 1)
  #     , BaseYear = lag(FYear, 1)
  #     , IsActiveCCG = CCGCode == active_ccg) %>%
  #   filter(row_number() == n())
  
  # opRocSummaryFUF <- opRocFUF %>%
  #   ungroup() %>%
  #   group_by(Strategy, FYear) %>%
  #   summarise(
  #     AverageRateOfChange = mean(RateOfChange, na.rm = TRUE)
  #     , ActualMinFirst = min(First, na.rm = TRUE)
  #     , ActualMaxFirst = max(First, na.rm = TRUE)
  #     , ActualMinFUFRatio = min(FUFRatio, na.rm = TRUE)
  #     , ActualMaxFUFRatio = max(FUFRatio, na.rm = TRUE)
  #     , ActualMinRateOfChange = min(RateOfChange, na.rm = TRUE)
  #     , ActualMaxRateOfChange = max(RateOfChange, na.rm = TRUE)
  #   ) %>%
  #   group_by(Strategy, FYear) %>%
  #   mutate(
  #     NewMinFirst = chartLimits(ActualMinFirst, ActualMaxFirst)["Min"]
  #     , NewMaxFirst = chartLimits(ActualMinFirst, ActualMaxFirst)["Max"]
  #     , NewMinFUFRatio = chartLimits(ActualMinFUFRatio, ActualMaxFUFRatio)["Min"]
  #     , NewMaxFUFRatio = chartLimits(ActualMinFUFRatio, ActualMaxFUFRatio)["Max"]
  #     , NewMinRateOfChange = roundTo(ActualMinRateOfChange -0.05, 0.05)
  #     , NewMaxRateOfChange = roundTo(ActualMaxRateOfChange + 0.05, 0.05)
  #   ) 
  # 
  # 
  # opRocFunnelsFUF <- 
  #   expand.grid(
  #     Strategy = unique(opRocSummaryFUF$Strategy)
  #     , RowNumber = seq(1, funnelParameters$Smoothness, 1)
  #     , stringsAsFactors = FALSE
  #   ) %>%
  #   left_join(opRocSummaryFUF, by = "Strategy") %>%
  #   mutate(EventSpells = NA) %>%
  #   arrange(Strategy)  
  # 
  # for (i in seq(length(opRocFunnelsFUF$EventSpells))){
  #   if(opRocFunnelsFUF$RowNumber[i] == 1){
  #     opRocFunnelsFUF$EventSpells[i] <- max(1, opRocFunnelsFUF$NewMinFirst[i])
  #   } else {
  #     opRocFunnelsFUF$EventSpells[i] <- 
  #       max(
  #         round(
  #           ((opRocFunnelsFUF$NewMaxFirst[i]) / 
  #              opRocFunnelsFUF$EventSpells[i - 1]) ^ 
  #             (1 / ((funnelParameters$Smoothness + 1) - opRocFunnelsFUF$RowNumber[i])) * opRocFunnelsFUF$EventSpells[i - 1]
  #         )
  #         , opRocFunnelsFUF$EventSpells[i - 1] + 1
  #       )
  #   }
  # }
  # 
  # opRocFunnelsFUF <- opRocFunnelsFUF %>%
  #   mutate(
  #     Denominator = EventSpells 
  #     , StandardError =  sqrt(1 / Denominator + 1 / (Denominator * (1 + AverageRateOfChange)))
  #     , ThreeSigmaLower  = (AverageRateOfChange - (3 * StandardError)) 
  #     , TwoSigmaLower  = (AverageRateOfChange - (2 * StandardError))
  #     , TwoSigmaHigher  = (AverageRateOfChange +(2 * StandardError))
  #     , ThreeSigmaHigher  = (AverageRateOfChange + (3 * StandardError))
  #   )
  
  
  
  
  
  # #Checking row numbers
  # if(n_groups(ipRoCSummary) * 2 != nrow(ipRoCSummary)){
  #   cat("There is at least one group without a comparator year.")}
  #   ipRoCSummary %>% count(vars = Strategy) %>% filter(n < 2)
  
  #funnelsWithNaN <- filter(rocFunnels, is.nan(StandardError))
  
  
  # Remove bits from the ends of the funnels that would be above/below visible range of the funnel plot
  # This is just to avoid a warning when plotting.
  #rocFunnels <- rocFunnels %>%
  #  filter(TwoSigmaHigher <= NewMaxRateOfChange & TwoSigmaLower >= NewMinRateOfChange)
  

# Trend  ---------------------------------------------------------
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

# Cost [KEEP]-----------------------------------
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

plot_ip_fun   <- list()
plot_ip_roc   <- list()
plot_ip_cost  <- list()
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
 
  plotFunnels <- funnel[[1]] %>% convert_dsr_100k()
  plotUnits   <- funnel[[2]] %>% 
    left_join(ipFunnelPoints %>% filter(Strategy == ipPlottableStrategies$Strategy[i]), by = "CCGCode") %>%
    convert_dsr_100k() %>% label_ccg()
  
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

# Draw roc plot ------------------------------------------------------

  plotRocPoints <- ipRoC %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- ipRoCFunnels %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  plotRocSummary <- ipRoCSummary %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
 
  plot_ip_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary)
  
  # plot_ip_funroc[[i]] <- ggplot(data = plotRocFunnels) +
  #   geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "grey40", linetype = "longdash") +
  #   geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = "longdash") +
  #   geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = "longdash") +
  #   geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "grey40", linetype = "longdash") +
  #   geom_hline(aes(yintercept = AverageRateOfChange)) +
  #   geom_point(
  #     data = plotRocPoints
  #     , aes(x = SpellsInBaseYear, y = RateOfChange, colour = IsActiveCCG)
  #     , size = 4
  #     , shape = 20
  #   ) +
  #   scale_colour_manual(values = colourBlindPalette[c("blue", "red")] %>% unname) +
  #   scale_x_continuous(
  #     labels = scales::comma
  #     , limits = c(plotRocSummary$NewMinSpells, plotRocSummary$NewMaxSpells)) +
  #   scale_y_continuous(
  #     labels = scales::percent
  #     , limits = c(plotRocSummary$NewMinRateOfChange, plotRocSummary$NewMaxRateOfChange)) +
  #   labs(
  #     x = paste0("Related spells "
  #                , plotRocPoints %>% 
  #                  filter(IsActiveCCG) %>% 
  #                  ungroup() %>% 
  #                  select(From) %>% 
  #                  unlist %>% unname %>% 
  #                  FYearIntToChar)
  #     , y = paste0("Percentage change")
  #     , title = 
  #       paste0("Rate of Change in DSR between "
  #              , plotRocPoints %>% 
  #                filter(IsActiveCCG) %>% 
  #                ungroup() %>% 
  #                select(From) %>% 
  #                unlist %>% unname %>% 
  #                FYearIntToChar
  #              , " and "
  #              , plotRocPoints %>% 
  #                filter(IsActiveCCG) %>% 
  #                ungroup() %>% 
  #                select(FYear) %>% 
  #                unlist %>% unname %>% 
  #                FYearIntToChar)
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
  #     filename = paste0("Images/IP_", ipPlottableStrategies$Strategy[i], "_RoC.png")
  #     , height = 8.9
  #     , width = 13.3
  #     , units = "cm")
  
  
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
   , plotRocPoints, plotRocFunnels, plotRocSummary
   , plotCostData, plotCostFactorLevels
   , plotTrendActive, plotTrendComparators
   , i)


# A&E plots ---------------------------------------------------------------
aePlottableStrategies <- activeStrategies %>%
  filter(TableType == "AE") 

plot_ae_fun   <- list()
plot_ae_roc   <- list()
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
  
  
  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- aeRoC %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotRocFunnels <- aeRoCFunnels %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotRocSummary <- aeRoCSummary %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  
  plot_ae_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary)
  
  
  
 
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
plot_op_cost  <- list()
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
  

  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- opRoC %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotRocFunnels <- opRoCFunnels %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotRocSummary <- opRoCSummary %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plot_op_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary)
  
  

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


qipp_save <- function(x, y){
  
  ggsave(filename = x,
         plot = y,
         width    = 9.5*1.414, # A4 ratio
         height   = 9.5,
         units    = "cm")
}


# Experiment with purrr WALK
## see pwalk in r
# tmp_fnames <- map_chr(seq_along(ipPlottableStrategies$Strategy) , function(i) paste0(i , "_fun_", ipPlottableStrategies$Strategy[i] ,".png"))
# # tmp_fnames <- str_c(baseDir, "output/",as.character(seq(1, length(tmp_fnames))), ".1_", tmp_fnames)
# # 
# pwalk(list(tmp_fnames, plot_ip_fun), qipp_save)

"Next time you come to print, try to write a closure."

save_plots <- function(vector_of_strats, list_of_plots){
  
  png_filenames <- map_chr(seq_along(vector_of_strats),
                           function(i) paste0(i ,# This just means that one doesn't need to specify "trend" etc.
                                              str_c("_", 
                                                         c(unlist(
                                                           str_split(
                                                             as.character(quote(list_of_plots)), "\\_")
                                                         )[c(F, F, T)]),
                                                         "_"),
                                               vector_of_strats[i] ,".png")
                           )
  
  pwalk(list(png_filenames, list_of_plots), qipp_save)
}

save_plots(ipPlottableStrategies$Strategy, plot_ip_fun)
save_plots(ipPlottableStrategies$Strategy, plot_ip_roc)
save_plots(ipPlottableStrategies$Strategy, plot_ip_trend)

save_plots(aePlottableStrategies$Strategy, plot_ae_fun)
save_plots(aePlottableStrategies$Strategy, plot_ae_roc)
save_plots(aePlottableStrategies$Strategy, plot_ae_trend)

save_plots(opPlottableStrategies$Strategy, plot_op_fun)
save_plots(opPlottableStrategies$Strategy, plot_op_roc)
save_plots(opPlottableStrategies$Strategy, plot_op_trend)


# ***** --------------------------------------------------------------



# # FUF plots ---------------------------------------------------------------
# opPlottableFUFStrategies <- activeStrategies %>%
#   filter(TableType == "OP") %>%
#   filter((grepl("^FUF*", Strategy)))
# 
# plot_fuf_fun   <- list()
# plot_fuf_cost  <- list()
# plot_fuf_trend <- list()
# 
# for(i in seq(opPlottableFUFStrategies$Strategy)){
# 
# # Draw funnel plot --------------------------------------------------------
#   
#   plotFunnelPoints <- opFUFFunnelPoints %>%
#    filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#  plotFunnelFunnels <- opFunnelFunnelsFUF %>%
#    filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#  plotFunnelSummary <- opFunnelSummaryFUF %>%
#    filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#  
#  plot_fuf_fun[[i]] <- ggplot(data = plotFunnelFunnels) +
#    geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "black", linetype =  44) +
#    geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = 44) +
#    geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = 44) +
#    geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "black", linetype =  44) +
#    # geom_hline(aes(yintercept = AverageFUF)) +
#    geom_segment(aes(  x      = min(Denominator)
#                       , xend = max(Denominator)
#                       , y    = AverageFUF
#                       , yend = AverageFUF))+
#    geom_point(
#      data = plotFunnelPoints
#      , aes(x = First, y = FUFRatio, colour = IsActiveCCG)
#      , size = 3
#    )+
#    scale_x_continuous(labels = scales::comma)+
#    scale_y_continuous(limits = c(0.8*min(plotFunnelPoints$FUFRatio)
#                                  , 1.2*max(plotFunnelPoints$FUFRatio)
#                                  )
#                       , labels = scales::comma)+
#    theme_strategy()+
#    theme(legend.position = "none")+
#    labs(
#      x = paste0("First appointments ", FYearIntToChar(f_year))
#      , y = "Ratio of follow-ups to first"
#      , title = paste0("Ratio of Follow-ups to First Appointments ", FYearIntToChar(f_year))
#    )+
#    scale_color_manual(values = c("grey70", '#c52828'))
# 
# 
# # Draw cost plot ----------------------------------------------------------
#   plotCostData <- opCostFUF %>%
#     filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#   
#   plotCostFactorLevels <- plotCostData %>% 
#     ungroup() %>%
#     arrange(desc(DSCostsPerHead)) %>% 
#     select(ShortName) %>% unlist %>% unname
#   
#   plotCostData <- plotCostData %>%
#     mutate(ShortName = factor(ShortName, levels = plotCostFactorLevels)) %>% 
#     mutate(for_label = format(round(DSCostsPerHead, 1), nsmall = 2))
#   
#   
#   plot_fuf_cost[[i]] <- plot_cost(plotCostData)
#  
# # Draw trend plots --------------------------------------------------------
# "May have to look into this. IsActiveCCG == False may be all other CCGs?"
#   plotTrendActive <- opTrendFUF %>%
#     filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#    # plotTrendComparators <- opTrendComparators %>%
#    #   filter(Strategy == opPlottableFUFStrategies$Strategy[i])
#   
#   plot_fuf_trend[[i]] <-   plot_trend(plotTrendActive %>% filter(IsActiveCCG == T),
#                                       plotTrendActive %>% filter(IsActiveCCG == F),
#                                       "FUFRatio",
#                                       plotTrendActive$FUFRatio,
#                                       plotTrendActive$FUFRatio, # or 0,
#                                       F # for comparator
#                                       )
#   
#   
# # ***** --------------------------------------------------------
# }
# rm(plotFunnelPoints, plotFunnelFunnels, plotFunnelSummary
#    #, plotFUFRocPoints, plotFUFRocFunnels, plotFUFRocSummary
#    , plotCostData, plotCostFactorLevels
#    , plotTrendActive
#    , i)
# 
# detach("package:testthat", unload=TRUE) # remove this conflict by loading tidyverse last


# Summary tables -------------------------------------------------------------
setwd(baseDir)
"CAREFUL IF RE-RUNNING THE FUNNEL PLOTS AFTERWARD"
"These required for summary tables"
summ_ipFunnelPoints    <- ip    %>% filter(FYear == f_year) 
summ_opFunnelPoints    <- op    %>% filter(FYear == f_year)
summ_aeFunnelPoints    <- ae    %>% filter(FYear == f_year)
summ_opFUFFunnelPoints <- opFUF %>% filter(FYear == f_year) 

summ_ipFunnelSummary <- summ_ipFunnelPoints %>% funnel_summary
summ_aeFunnelSummary <- summ_aeFunnelPoints %>% funnel_summary
summ_opFunnelSummary <- summ_opFunnelPoints %>% funnel_summary

summ_ipFunnelFunnels <- funnel_funnels(summ_ipFunnelSummary, funnelParameters$Smoothness, personYears)
summ_aeFunnelFunnels <- funnel_funnels(summ_aeFunnelSummary, funnelParameters$Smoothness, personYears)
summ_opFunnelFunnels <- funnel_funnels(summ_opFunnelSummary, funnelParameters$Smoothness, personYears)


comparatorsOut <- comparatorCCGs2 %>%
  filter(CCGCode != active_ccg) %>%
  select(CCGCode, CCGDescription)

# Comparator table

flex_comparat    <- setZebraStyle(vanilla.table(comparatorsOut), odd = alpha("goldenrod1", 0.4), even = alpha("goldenrod1", 0.2))

flex_comparat[,] <- textProperties(font.family = "Segoe UI", font.size = 12)
flex_comparat[to = "header"]      <-  textProperties(font.size = 14, font.family = "Segoe UI")

flex_comparat[, ]                <- parLeft()
flex_comparat[, , to = "header"] <- parLeft()


flex_comparat <- setFlexTableBorders(flex_comparat
                                    , inner.vertical = borderProperties( style = "dashed", color = "white" )
                                    , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                                    , outer.vertical = borderProperties( width = 2, color = "white"  )
                                    , outer.horizontal = borderProperties( width = 2, color = "white"  )
)


#  *****--------------------------------------------------------------


# Inpatient ---------------------------------------------------------------

totalActivityIP <- ipSmall %>% total_activity
savingsAnyOneIP <- ipTrendComparators %>% savings_any_one

ipSignificance  <- significance_summary(summ_ipFunnelPoints, summ_ipFunnelFunnels, ipRoC, ipRoCFunnels)

summaryOutputIP <- ipSmall %>% summary_output(., savingsAnyOneIP, ipSignificance, totalActivityIP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(ipTrendActive %>% 
    filter(FYear == f_year) %>%
    ungroup() %>%
    select(Strategy, DSRateCIUpper, DSRateCILower)
    , by = "Strategy") %>%
  left_join(ipCost %>%
    ungroup() %>%
    select(CCGCode, Strategy, DSCostsPerHead)
    , by = c("CCGCode", "Strategy"))


# IP tbl summary -----------------------------------------------------

summ_ip_summ_out <- summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) 

flex_ip_summ    <- setZebraStyle(vanilla.table(summ_ip_summ_out), odd = alpha("goldenrod1", 0.4), even = alpha("goldenrod1", 0.2))

flex_ip_summ[,] <- textProperties(font.family = "Segoe UI", font.size = 12)
flex_ip_summ[to = "header"]      <-  textProperties(font.size = 14, font.family = "Segoe UI")

flex_ip_summ[, 1]                <- parLeft()
flex_ip_summ[, 1, to = "header"] <- parLeft()


flex_ip_summ <- setFlexTableBorders(flex_ip_summ
                                    , inner.vertical = borderProperties( style = "dashed", color = "white" )
                                    , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                                    , outer.vertical = borderProperties( width = 2, color = "white"  )
                                    , outer.horizontal = borderProperties( width = 2, color = "white"  )
)


# IP cost summary ----------------------------------------------------

summ_ip_cost_out <- # head(
  summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  )


flex_ip_cost    <-  setZebraStyle(vanilla.table(summ_ip_cost_out), odd = alpha("dodgerblue2", 0.2), even = alpha("white", 1))
flex_ip_cost[,] <-  textProperties(font.family = "Segoe UI"
                                   , font.size = 12)

flex_ip_cost[to = "header"]  <-  textProperties(font.size = 14,
                                                font.family = "Segoe UI")

flex_ip_cost[, 1]                <- parLeft()
flex_ip_cost[, 1, to = "header"] <- parLeft()


flex_ip_cost <- setFlexTableBorders(flex_ip_cost
                                    , inner.vertical = borderProperties( style = "dashed", color = "white" )
                                    , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                                    , outer.vertical = borderProperties( width = 2, color = "white"  )
                                    , outer.horizontal = borderProperties( width = 2, color = "white"  )
)



# IP save plot -----------------------------------------------

savingsIP <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded) %>% 
  rename(average = Average_SavingsIf_Rounded 
         , quartile = TopQuartile_SavingsIf_Rounded 
         , decile = TopDecile_SavingsIf_Rounded 
  ) %>% 
  filter(Strategy != "Readmissions_v1", Strategy != "Canc_Op_v1") # %>% 
  # gather(level, saving, 2:4)

# basic name adjust
savingsIP$Strategy <-  savingsIP$Strategy %>% 
    str_replace_all("v[0-9]?", "") %>%
    str_replace_all("\\_"," ") %>%
    str_trim()

# so have multiple variables again.
# error in stacked  bars! should not be stacked
# ggplot(savingsIP, aes(reorder(Strategy, saving), saving, fill = level))+  
plot_savings_ip <- ggplot(savingsIP, aes(reorder(Strategy, average), average))+
geom_bar(stat = "identity", colour = "black", aes(fill = "myline1"))+
geom_bar(aes(Strategy, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
  # geom_bar(stat = "identity", position = "stack") +
  coord_flip()+
  theme_strategy_large()+
  scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
                     , position = "top")+
  # scale_y_continuous(labels = scales::comma_format())
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.82, 0.18),
        legend.background = element_rect(fill = "white"))+
  scale_fill_manual(
    name = "line Colour"
    ,values=c(myline1 = "lightblue", myline2 = "lightblue")
                    , labels=c("Savings if Average", "Savings if Top Quartile"))+
  # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
  #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
  ylab("Potential savings (millions of pounds)")+
  guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2)))) # the second alpha does not have to relate


# ***** --------------------------------------------------------------


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


# totalActivityOPFUF <- opSmallFUF %>% total_activity
# 
# spendFUF <- opCostFUF %>%
#   filter(CCGCode == active_ccg) %>%
#   select(Strategy, Costs)
# 
# opTopFUF <- opSmallFUF %>%
#   select(-DSCosts, -DSCostsVar, -DSRate, -DSRateVar, -CCGDescription, -ShortName) %>%
#   gather(Strategy, Highlighted, -Spells, -Costs, -CCGCode, -FYear, convert = T) %>%
#   group_by(Strategy, CCGCode, FYear, Highlighted) %>%
#   summarise(
#     Spells = sum(Spells, na.rm = TRUE)
#     ) %>%
#   filter(!is.na(Highlighted)) %>%
#   mutate(FUF = ifelse(Highlighted == 1,"First", "FollowUp")) %>%
#   select(-Highlighted) %>%
#   spread(FUF, Spells) %>%
#   mutate(FUFRatio =  FollowUp / First) %>%
#   filter(FYear == f_year) %>%
#   group_by(Strategy) %>%
#   summarise(
#     Average = sum(FollowUp, na.rm = TRUE) / sum(First, na.rm = TRUE)
#     , TopQuartile = quantile(FUFRatio, 0.25, na.rm = TRUE)
#     , TopDecile = quantile(FUFRatio, 0.1, na.rm = TRUE)
#     #, MaxFUFRatio = max(FUFRatio, na.rm = TRUE)
#     #, MinFUFRatio = min(FUFRatio, na.rm = TRUE)
#   ) 
# 
# 
# savingsAnyOneOPFUF <- opTrendFUF %>% 
#   filter(FYear == f_year, IsActiveCCG) %>%
#   left_join(spendFUF, by = "Strategy") %>%
#   select(-IsActiveCCG) %>%
#   left_join(opTopFUF, by = "Strategy") %>%
#   mutate(
#     SavingsIfAverage = Costs - (Average / FUFRatio) * Costs 
#     , SavingsIfTopQuartile = Costs - (TopQuartile / FUFRatio) * Costs
#     , SavingsIfTopDecile = Costs - (TopDecile / FUFRatio) * Costs
#   ) %>%
#   ungroup() %>%
#   select(Strategy, SavingsIfAverage, SavingsIfTopQuartile, SavingsIfTopDecile) %>%
#   mutate(
#     SavingsIfAverage = ifelse(SavingsIfAverage < 0, 0, SavingsIfAverage)
#     , SavingsIfTopQuartile = ifelse(SavingsIfTopQuartile < 0, 0, SavingsIfTopQuartile)
#     , SavingsIfTopDecile = ifelse(SavingsIfTopDecile < 0, 0, SavingsIfTopDecile)
#   )
# 
# 
# opSignificanceFUF <- opFUFFunnelPoints %>% 
#   filter(CCGCode == active_ccg) %>%
#   left_join(opFunnelFunnelsFUF, by = "Strategy") %>%
#   group_by(Strategy) %>%
#   mutate(Test = First - Denominator
#          , AbsTest = abs(Test)
#          , TestRank = rank(AbsTest, ties.method = "first")) %>%
#   filter(TestRank == 1) %>%
#   mutate(Significance = ifelse(FUFRatio > TwoSigmaHigher, "High", 
#                       ifelse(FUFRatio < TwoSigmaLower, "Low", "Not Significant"))) %>%
#   select(CCGCode, Strategy, Significance) 
# 
#   opRocFUF2 <- opRocFUF %>%
#     filter(CCGCode == active_ccg) %>% ungroup() %>% select(-CCGCode)
#   opRocFunnelsFUF2 <- opRocFunnelsFUF %>% select(-FYear)
# 
# opSignificanceFUF <- opSignificanceFUF %>%
#   left_join(opRocFUF2, by = "Strategy") %>%
#   left_join(opRocFunnelsFUF2, by = "Strategy") %>%
#   mutate(Test = FirstInBaseYear - Denominator
#          , AbsTest = abs(Test)
#          , TestRank = rank(AbsTest, ties.method = "first")) %>%
#   filter(TestRank == 1) %>%
#   mutate(
#     RocSignificance = ifelse(RateOfChange > TwoSigmaHigher, "High"
#                         , ifelse(RateOfChange < TwoSigmaLower, "Low", "Not Significant"))
#     , ReviewNumber = ifelse(Significance == "Low", 1
#                      , ifelse(Significance == "Not Significant", 2
#                       , ifelse(Significance == "High", 3))) + 
#                     ifelse(RocSignificance == "Low", 0
#                      , ifelse(RocSignificance == "Not Significant", 3
#                       , ifelse(RocSignificance == "High", 6)))
#     , ReviewGroup = ifelse(ReviewNumber <= 3, "Review"
#                      , ifelse(ReviewNumber == 8, "Close monitoring"
#                       , ifelse(ReviewNumber %in% c(4, 5, 7), "Background monitoring", "Explore")))
#   ) %>%
#   select(CCGCode, Strategy, Significance, RocSignificance, ReviewNumber, ReviewGroup) 
# 
# 
# summaryOutputOPFUF <- 
#   opTrendFUF %>%
#   filter(FYear == f_year & IsActiveCCG) %>%
#   select(-IsActiveCCG) %>%
#   left_join(opTopFUF, by = "Strategy") %>%
#   left_join(savingsAnyOneOPFUF, by = "Strategy") %>%
#   left_join(opSignificanceFUF, by = "Strategy") %>%
#   left_join(activeStrategies, by = "Strategy") %>%
#   left_join(spendFUF, by = c("Strategy", "CCGCode")) %>%
#   mutate(
#     SpellsRounded = round(FollowUp, -0.5) #FollowUp_Rounded
#     , Costs_Rounded = round(Costs, -3)
#     , Average_SavingsIf_Rounded = roundTo(SavingsIfAverage, 1000)
#     , TopQuartile_SavingsIf_Rounded = roundTo(SavingsIfTopQuartile, 1000)
#     , TopDecile_SavingsIf_Rounded = roundTo(SavingsIfTopDecile, 1000)
#   ) %>%
#   ungroup() %>%
#   select(-IsActiveCCG)


# write.table(summaryOutputOP, "Data/R_SummaryOutputOP.csv", sep = ",", row.names = FALSE)
# write.table(summaryOutputOPFUF, "Data/R_SummaryOutputOPFUF.csv", sep = ",", row.names = FALSE)
# write.table(summaryOutputOP, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputOP.csv"), sep = ",", row.names = FALSE)
# write.table(summaryOutputOPFUF, paste0("Data/ByCCG/R_", active_ccg, "SummaryOutputOPFUF.csv"), sep = ",", row.names = FALSE)
# 
# comparatorsOut <- comparatorCCGs %>%
#   filter(NeighbourCCGCode != activeCCG) %>%
#   select(ClosenessRank, NeighbourCCGCode, CCGDescription)

# messes with the comparators table in Excel

# write.table(comparatorsOut, "Data/R_ComparatorCCGs.csv", sep = ",", row.names = FALSE)
# write.table(comparatorsOut, paste0("Data/ByCCG/R_", active_ccg, "ComparatorCCGs.csv"), sep = ",", row.names = FALSE)

#  -------------------------------------------------------------------


cat("Ends")
activeCCGInfo$CCGDescription



# JOYPLOTS? OF RATES ----------------------------------------

tmp_summary <- ipSmall %>%
  filter(FYear == f_year) %>%
  select(-FYear, -DSRateVar, -DSCosts, -DSCostsVar) %>%
  gather(Strategy, Highlighted,  -Spells, -Costs, -DSRate, -CCGCode,  -CCGDescription, -ShortName, convert = T) %>% 
  group_by(Strategy, CCGCode ,CCGDescription, ShortName, Highlighted) %>%
  summarise_each(
    funs(sum(., na.rm = TRUE))
    , Spells, Costs, DSRate
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>% 
  filter(Strategy == "ACS_Acute_v3")

# Binwidth calculated using the Freedman-Diaconis rule:
# 2*IQR(tmp_summary$DSRate)*(length(tmp_summary$DSRate)^(-1/3))

ggplot(tmp_summary, aes(DSRate))+
  geom_histogram(binwidth = 2*IQR(tmp_summary$DSRate)*(length(tmp_summary$DSRate)^(-1/3)))+
  geom_segment()
