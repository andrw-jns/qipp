############################################################################
" MASTER REDUX"
" CREATE QIPP PACK"
###########################################################################

# ***** --------------------------------------------------------------
"Use package check system: checkpoint? See email"
"Code seems to be unaffected by update to dplyr 0.7 etc except summaries"
"which now updated to use mutate_at() etc."
"Check colour blind friendly"
"Fix size of savings plots"
"Savings plots don't really give enough detail - must be used in
conjunction with funnels to work out where easiest savings are"
"GP referrals v1 vs v2? Strategy (from SQL vs oldName)"

# Packages ----------------------------------------------------------------

library(here)
library(readxl)
library(scales, warn.conflicts = FALSE)
library(testthat)
library(extrafont) # for theme_strategy.
library(stringr)
library(ReporteRs)
suppressPackageStartupMessages(library(tidyverse))
library(ggrepel)


# Parameters 1--------------------------------------------------------

baseDir  <- "C:/2017_projects/qipp/" # using here, now

active_ccg <- "05T"
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

ip_colours <- c("#EC6555", "#EC6555") # SU red
ae_colours <- c("#91F5AD", "#91F5AD") # teal deer, alternative #75BBA7 ; "#91F5AD"
op_colours <- c("#5881c1", "#5881c1") # SU blue

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
source_here("roundingAndChartLimitFunctions.R")
source_here("funnelPlotFunctions.R")
source_here("trendPlotFunctions.R")
source_here("costPlotFunctions.R")
source_here("summaryFunctions.R") 
source_here("theme_strategy.R")

#setwd("C:/2017_projects/funnel/funnel/")
source("C:/2017_projects/funnel/funnel/funlplotr20171024.R")
# to hopefully get the newest version of the funnel plot code.

# This - believe it or not - works like a function : pound()
pound <- dollar_format(prefix = "£")


plot_trend <- function(active_df, comparator_df, quote_y, active_y, comparator_y, colour_block, comparator = T){
  
  p <- ggplot()+
    geom_area(data = active_df,
              aes(
                FYearIntToChar(FYear),
                get(quote_y),
                group = 1
              )
              , alpha = 0.2
              , fill = colour_block # "#c52828" # SWB Red: # '#c52828' - original red
              )+
    geom_line(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.8
              , colour = colour_block
              , size = 1
    )+
    # Semi transparent points:
    geom_point(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.2
              , colour = colour_block
              , size = 3
    )+
    # Highlighted points:
    geom_point(data = active_df %>% 
                 filter(FYear == f_year),
               aes(
                 FYearIntToChar(FYear), 
                 get(quote_y),
                 group = 1
               )
               , colour = colour_block
               , size = 3
    )+
    ylim(0, 1.2*max(active_y, comparator_y))+
    theme_strategy()+
    theme(panel.background = element_rect(fill = "white"),
          # plot.subtitle = element_text(face = "italic"),
          axis.title = element_blank())+
    labs(y = paste0("DSR per ",
                    scales::comma(funnelParameters$RatePerPeople)," population"),
         title = paste0("Trend in Directly Standardised Rate, ",
                        str_sub(rocParameters$From ,1, 4), "-",
                        str_sub(rocParameters$From ,5, 6), " to "
                        , str_sub(rocParameters$To ,1, 4), "-",
                        str_sub(rocParameters$To ,5, 6))
         , subtitle = "DSR per 100k population [Vertical Axis]")+
    scale_x_discrete(expand = c(0.02,0.0))

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
    )+ 
    geom_segment(data = comparator_df,
                 aes(
                   y = 0.1*max(comparator_y), yend = 0.1*max(comparator_y),
                   x = 4.05, xend = 4.25
                 )
    )+
    geom_text(data = comparator_df,
              aes(x = 4.6, y = 0.1*max(comparator_y),
                  label ="W.M. average",
                  family = "Segoe UI",
                  fontface = "plain"),
               size = 2.5)
    
      
  } else {
    p  + geom_line(data = comparator_df,
                   aes(
                     FYearIntToChar(FYear), 
                     get(quote_y),
                     group = 1
                   )
                   # ,linetype = "longdash"
                   , alpha = 0.8
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

plot_fun   <- function(df_funnels, df_units, colour_block){
  
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
              , aes(x = 0.75*max(n), y = min(fnlLow, df_units$DSRate), label = "Standardised population 2016-17")
              # , vjust = "bottom"
              # , hjust = "right"
              , family = "Segoe UI"
              , size  = 2.5
              , fontface  = "plain"
              , color = "grey30"
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
    scale_color_manual(values = c("grey70", colour_block))

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

plot_roc <- function(funnel_df, points_df, summary_df, colour_block){
  
  ggplot(data = funnel_df) +
    geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "black", linetype =  44) +
    # geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = 44) +
    # geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = 44) +
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
              , aes(x = max(NewMaxSpells), y = min(NewMinRateOfChange), label = "Related activity 2012-13")
              , vjust = "bottom"
              , hjust = "right"
              , family = "Segoe UI"
              , size  = 2.5
              , fontface  = "plain"
              , color = "grey30"
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
        paste0("Percentage Change in Directly Standardised Rate, "
               , points_df %>% 
                 filter(IsActiveCCG) %>% 
                 ungroup() %>% 
                 select(From) %>% 
                 unlist %>% unname %>% 
                 FYearIntToChar
               , " to "
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
      subtitle = "Percentage change in DSR [Vertical Axis]"
      #, title = paste0("Ratio of Follow-ups to First Appointments ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", colour_block))
  
  #69D2E7,#A7DBD8,#E0E4CC,#F38630,#FA6900,#69D2E7,#A7DBD8,#E0E4CC
  #"grey70", '#c52828'
  #"#69D2E7", '#FA6900'
}

plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, "grey70")

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

"Now do some wrangling to add new set of names to activeStrategies:"

"WARNING: THIS IS A MANUAL PROCESS"
"FOUNDATIONS (THE FORMAT OF THE SOURCE CSV) COULD BE IMPROVED IN FUTURE"
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

# trial <- activeStrategies %>% select(Strategy, oldName)

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
  

# CCG populations for cost charts from ONS projections
ccgPopulation <- read_csv("CCGPopulation.csv", skip = 1)


# *****chckpnt**** -----------------------------------------------------------------

# CCG selections
# comparatorCCGs2 <- allCCGs %>%
#   filter(CCGCode %in% qipp_ccgs,
#          CCGCode != active_ccg)

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

# opSmallFUF <- opData %>%
#   select(
#     DSRate, DSRateVar, DSCosts, DSCostsVar, Attendances, Costs, FYear, CCGCode
#     , starts_with("FUF")) %>% removeInvalidCCGs 


# I'm going to call attendances Spells so that I can reuse functions
colnames(aeSmall)    <- gsub("Attendances", "Spells", colnames(aeSmall))
colnames(opSmall)    <- gsub("Attendances", "Spells", colnames(opSmall))
# colnames(opSmallFUF) <- gsub("Attendances", "Spells", colnames(opSmallFUF))


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
ipTrendActive <- ipSmall %>% trend_active 
aeTrendActive <- aeSmall %>% trend_active 
opTrendActive <- opSmall %>% trend_active 

ipTrendComparators <- ipSmall %>% trend_comparators 
aeTrendComparators <- aeSmall %>% trend_comparators 
opTrendComparators <- opSmall %>% trend_comparators 

# Cost [KEEP]-----------------------------------

ipCost <- ipSmall %>% cost_ds 
aeCost <- aeSmall %>% cost_ds 
opCost <- opSmall %>% cost_ds 

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
  
  plot_ip_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = ip_colours[1])
  

# Draw roc plot ------------------------------------------------------

  plotRocPoints <- ipRoC %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- ipRoCFunnels %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
  plotRocSummary <- ipRoCSummary %>%
    filter(Strategy == ipPlottableStrategies$Strategy[i])
 
  plot_ip_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = ip_colours[2])
  
  
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
                                   colour_block = ip_colours[1])
   
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
  
  plot_ae_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = ae_colours[1])
 
  
  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- aeRoC %>%
    filter(Strategy == aePlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- aeRoCFunnels %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  plotRocSummary <- aeRoCSummary %>%
    filter(Strategy == aePlottableStrategies$Strategy[i])
  
  plot_ae_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = ae_colours[2])
  
  
  
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
                                   colour_block = ae_colours[1])
  
  
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
  
  plot_op_fun[[i]] <- plot_fun(plotFunnels, plotUnits, colour_block = op_colours[1])
  

  # Draw roc plot ------------------------------------------------------
  
  plotRocPoints <- opRoC %>%
    filter(Strategy == opPlottableStrategies$Strategy[i]) %>% 
    label_ccg()
  plotRocFunnels <- opRoCFunnels %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  plotRocSummary <- opRoCSummary %>%
    filter(Strategy == opPlottableStrategies$Strategy[i])
  
  plot_op_roc[[i]] <-plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, colour_block = op_colours[2])
  
  

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
                                   colour_block = op_colours[1])
  
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


# Comparator table

comparatorsOut <- comparatorCCGs2 %>%
  filter(CCGCode != active_ccg) %>%
  select(CCGCode, CCGDescription)

flex_comparat    <- setZebraStyle(vanilla.table(comparatorsOut), odd = alpha("goldenrod1", 0.4), even = alpha("goldenrod1", 0.2))


flex_comparat    <- vanilla.table(comparatorsOut)



flex_comparat[,] <- textProperties(font.family = "Segoe UI", font.size = 12)
flex_comparat[to = "header"]      <-  textProperties(font.size = 14, font.family = "Segoe UI")

# align left
flex_comparat[, ]                <- parLeft()
flex_comparat[, , to = "header"] <- parLeft()

# borders
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


# IP labels for charts / tables------------------------------------------

labels_ip <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("ACS Acute",
                         "ACS Chronic",
                         "ACS Vaccine",
                         "Alcohol (25% to 75%)",
                         "Alcohol (5% to 25%)",
                         "Alcohol (75% to 100%)",
                         "",
                         "EOLC (3-14 days)",
                         "EOLC (0-2 days)",
                         "Falls",
                         "Frail Elderly (occasional)",
                         "Frail Elderly (usual)",
                         "Medically Unexplained",
                         "Meds Explicit",
                         "Meds Implicit AntiDiab",
                         "Meds Implicit Benzo",
                         "Meds Implicit Diuretics",
                         "Meds Implicit NSAIDs",
                         "Obesity (largely)",
                         "Obesity (marginal)",
                         "Obesity (somewhat)",
                         "PLCV Cosmetic",
                         "PLCV Alternative",
                         "PLCV Ineffective",
                         "PLCV Risks",
                         "MH admissions from ED",
                         "",
                         "Self-harm",
                         "Smoking (large)",
                         "Smoking (somewhat)",
                         "Zero LOS (adult)",
                         "Zero LOS (child)"))


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



flexify_summary <- function(df){
  
  data    <- vanilla.table(df)
 
  data[,] <- textProperties(font.family = "Segoe UI", font.size = 10)
  data[to = "header"]      <-  textProperties(font.size = 10, font.family = "Segoe UI", font.weight = "bold")
  
  data[, 1]                <- parLeft()
  data[, 1, to = "header"] <- parLeft()
  
  data[, 4:5]                <- parLeft()
  data[, 4:5, to = "header"] <- parLeft()
  
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


plot_the_savings <- function(df){
  
  ggplot(df, aes(reorder(Opportunity, average), average))+
    geom_bar(stat = "identity",  aes(fill = "myline1"))+
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
      ,values=c(myline1 = "#5881c1", myline2 = "#5881c1")
      , labels=c("Savings if average", "Savings if top quartile"))+
    # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
    #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
    ylab("Potential savings (£ millions)")+
    guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2))))+ # the second alpha does not have to relate
  theme(# panel.grid.major = element_blank(),
        panel.background = element_blank())+
    scale_x_discrete(expand = c(0,0))
  
}
  
plot_savings_ip <- plot_the_savings(savingsIP)   


# so have multiple variables again.
# error in stacked  bars! should not be stacked
# ggplot(savingsIP, aes(reorder(Strategy, saving), saving, fill = level))+  
# plot_savings_ip <- ggplot(savingsIP, aes(reorder(Opportunity, average), average))+
#   geom_bar(stat = "identity", colour = "black", aes(fill = "myline1"))+
#   geom_bar(aes(Opportunity, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
#   # geom_bar(stat = "identity", position = "stack") +
#   coord_flip()+
#   theme_strategy_large()+
#   scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
#                      , position = "top")+
#   # scale_y_continuous(labels = scales::comma_format())
#   theme(legend.title = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = c(0.82, 0.18),
#         legend.background = element_rect(fill = "white"))+
#   scale_fill_manual(
#     name = "line Colour"
#     ,values=c(myline1 = "#5881c1", myline2 = "#5881c1")
#                     , labels=c("Savings if Average", "Savings if Top Quartile"))+
#   # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
#   #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
#   ylab("Potential savings (millions of pounds)")+
#   guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2)))) # the second alpha does not have to relate


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
  mutate(Opportunity = c("Ambulance Treat",
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

plot_savings_ae <- plot_the_savings(savingsAE)

# basic name adjust
# savingsAE$Strategy <-  savingsAE$Strategy %>% 
#   str_replace_all("v[0-9]?", "") %>%
#   str_replace_all("\\_"," ") %>%
#   str_trim()

"Can make this plot a function?"
# # so have multiple variables again.
# # error in stacked  bars! should not be stacked
# # ggplot(savingsIP, aes(reorder(Strategy, saving), saving, fill = level))+  
# plot_savings_ae <- ggplot(savingsAE, aes(reorder(Opportunity, average), average))+
#   geom_bar(stat = "identity", colour = "black", aes(fill = "myline1"))+
#   geom_bar(aes(Opportunity, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
#   # geom_bar(stat = "identity", position = "stack") +
#   coord_flip()+
#   theme_strategy_large()+
#   scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
#                      , position = "top")+
#   # scale_y_continuous(labels = scales::comma_format())
#   theme(legend.title = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = c(0.82, 0.18),
#         legend.background = element_rect(fill = "white"))+
#   scale_fill_manual(
#     name = "line Colour"
#     ,values=c(myline1 = "lightblue", myline2 = "lightblue")
#     , labels=c("Savings if Average", "Savings if Top Quartile"))+
#   # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
#   #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
#   ylab("Potential savings (millions of pounds)")+
#   guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2)))) # the second alpha does not have to relate


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
  mutate(Opportunity = c("Consultant-Consultant Refer",
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

# basic name adjust
# savingsOP$Strategy <-  savingsOP$Strategy %>% 
#   str_replace_all("v[0-9]?", "") %>%
#   str_replace_all("\\_"," ") %>%
#   str_trim()

plot_savings_op <- plot_the_savings(savingsOP)

# so have multiple variables again.
# error in stacked  bars! should not be stacked
# ggplot(savingsIP, aes(reorder(Strategy, saving), saving, fill = level))+  
# plot_savings_op <- ggplot(savingsOP, aes(reorder(Opportunity, average), average))+
#   geom_bar(stat = "identity", colour = "black", aes(fill = "myline1"))+
#   geom_bar(aes(Opportunity, quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
#   # geom_bar(stat = "identity", position = "stack") +
#   coord_flip()+
#   theme_strategy_large()+
#   scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-6, digits = 1)
#                      , position = "top")+
#   # scale_y_continuous(labels = scales::comma_format())
#   theme(legend.title = element_blank(),
#         axis.title.y = element_blank(),
#         legend.position = c(0.82, 0.18),
#         legend.background = element_rect(fill = "white"))+
#   scale_fill_manual(
#     name = "line Colour"
#     ,values=c(myline1 = "lightblue", myline2 = "lightblue")
#     , labels=c("Savings if Average", "Savings if Top Quartile"))+
#   # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
#   #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
#   ylab("Potential savings (millions of pounds)")+
#   guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2)))) # the second alpha does not have to relate



#  *****--------------------------------------------------------------


# Population differences ---------------------------------------------


# For comparison
setwd(paste0(baseDir, "data"))
ccg_regist <- read_csv("ccg-reg-patients.csv",
                       col_types = cols_only(CCG_CODE = "c", TOTAL_ALL = "i"))


pop_comparisons <- ccgPopulation %>%
  left_join(ccg_regist, by = c("CCGCode" = "CCG_CODE")) %>%
  right_join(comparatorCCGs2, by = "CCGCode") %>%
  select(CCGDescription, everything(), - CCGCode, -ShortName) %>%
  mutate(reg_minus_res = TOTAL_ALL - Population) %>%
  rename(resident = Population, registered = TOTAL_ALL) %>%
  mutate(greater = ifelse(reg_minus_res > 0, "registered", "resident")) %>%
  mutate(diff_magnitude = abs(reg_minus_res)) %>%
  select(-reg_minus_res) %>%
  mutate(res_over_reg = round(resident/registered, 2)*100) %>%
  arrange(res_over_reg)
  # mutate(flag = ifelse(res_over_reg <0.95 | res_over_reg > 1.05, "!", "")) %>% 




flex_pop    <-  setZebraStyle(vanilla.table(pop_comparisons), odd = alpha("dodgerblue2", 0.15), even = alpha("dodgerblue2", 0.1))
flex_pop[,] <-  textProperties(font.family = "Segoe UI"
                                   , font.size = 12)

flex_pop[to = "header"]  <-  textProperties(font.size = 14,
                                                font.family = "Segoe UI")

flex_pop[, 1]                <- parLeft()
flex_pop[, 1, to = "header"] <- parLeft()


flex_pop <- setFlexTableBorders(flex_pop
                                    , inner.vertical = borderProperties( style = "dashed", color = "white" )
                                    , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                                    , outer.vertical = borderProperties( width = 2, color = "white"  )
                                    , outer.horizontal = borderProperties( width = 2, color = "white"  )
)


# write_csv(pop_comparisons, "population_comparisons.csv")

#  END-------------------------------------------------------------


cat("Ends")
activeCCGInfo$CCGDescription



# JOYPLOTS? OF RATES ----------------------------------------
setwd("C:/2017_projects/")
saveRDS(ipSmall, "ipSmall.RDS")

ipSmall <- read_rds("ipSmall.RDS")

tmp_summary <- ipSmall %>%
  filter(FYear == f_year) %>%
  select(-FYear, -DSRateVar, -DSCosts, -DSCostsVar) %>%
  gather(Strategy, Highlighted,  -Spells, -Costs, -DSRate, -CCGCode,  -CCGDescription, -ShortName, convert = T) %>% 
  group_by(Strategy, CCGCode ,CCGDescription, ShortName, Highlighted) %>%
  summarise_all(
    funs(sum(., na.rm = TRUE))
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>% 
  filter(Strategy == "ACS_Acute_v3")

# Binwidth calculated using Freedman-Diaconis rule:
# 2*IQR(tmp_summary$DSRate)*(length(tmp_summary$DSRate)^(-1/3))

ggplot(tmp_summary, aes(DSRate))+
  geom_freqpoly(binwidth = 2*IQR(tmp_summary$DSRate)*(length(tmp_summary$DSRate)^(-1/3)))
