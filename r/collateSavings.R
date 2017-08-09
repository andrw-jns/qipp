# Combine the available savings data in a form suitable for Andy in the BICPOS pack
rm(list = ls())

library(dplyr)
library(readr)
library(readxl)


# Add CCG description -----------------------------------------------------

setwd("S:/Commissioning Intelligence And Strategy/Strategic Analytics/Jonathan Spencer/FrequentFiles/Classification/Organisations/CCG")
allCCGs <- read_excel("CCG Index.xlsx", sheet = "England") %>%
  filter(CCGActiveDate <= "2014-04-01") %>%
  select(CCGCode, CCGDescription, ShortName)



# Get data ----------------------------------------------------------------

setwd("H:/QIPP/Data/ByCCG")

ipSavingsFiles <- list.files(pattern = "^R_.{3}SummaryOutputIP")
opSavingsFiles <- list.files(pattern = "SummaryOutputOP.csv")
opfufSavingsFiles <- list.files(pattern = "SummaryOutputOPFUF")
aeSavingsFiles <- list.files(pattern = "SummaryOutputAE")


# Test for consistent format ----------------------------------------------

columnNames <- function(x){
  lapply(x, readr::read_csv, n_max = 0) %>% 
  lapply(., colnames)
}


sameColumnNames <- function(x){
  if(lapply(x, readr::read_csv, n_max = 0) %>% 
    lapply(., colnames) %>% unique %>% length == 1){
  return(TRUE)
  } else {
  return(FALSE)
  }
}
sameColumnNames(ipSavingsFiles)
sameColumnNames(opSavingsFiles)
sameColumnNames(opfufSavingsFiles)
sameColumnNames(aeSavingsFiles)

columnNames(ipSavingsFiles)

# Import data -------------------------------------------------------------
# Note that you will get some parsing failures due to early versions of savings having 
# scientific notation in some columns. These errors are in derived (rounded) columns,
# so I just re-round them later in the script.
ipTest <- read_csv(
  ipSavingsFiles[[1]]
  , col_names = TRUE
  , col_types = "ciiddddddddddididddiiiiiccccciciddd"
  , na = "NULL"
  , skip = 0
  ) 

ipSavings <- lapply(
  ipSavingsFiles
  , read_csv
  , col_names = TRUE
  , col_types = "ciiddddddddddididddiiiiiccccciciddd"
  , na = "NULL"
  , skip = 0
  ) %>% bind_rows

opSavings <- lapply(
  opSavingsFiles
  , read_csv
  , col_names = TRUE
  , col_types = "ciiddddddddddididddiiiiiccccciciddd"
  , na = "NULL"
  , skip = 0
  ) %>% bind_rows

opfufSavings <- lapply(
  opfufSavingsFiles
  , read_csv
  , col_names = TRUE
  , col_types = "ciiidddddddccciciccciiiddd" #nb note this is different to the others.
  , na = "NULL"
  , skip = 0
  ) %>% bind_rows

aeSavings <- lapply(
  aeSavingsFiles
  , read_csv
  , col_names = TRUE
  , col_types = "ciiddddddddddididddiiiiiccccciciddd"
  , na = "NULL"
  , skip = 0
  ) %>% bind_rows



# Combine groups ----------------------------------------------------------
savingsOut <- . %>%
  mutate(
    Costs_Rounded = ifelse(is.na(Costs_Rounded), round(Costs_Actual, digits = -3), Costs_Rounded)
    , Average_SavingsIf_Rounded = ifelse(is.na(Average_SavingsIf_Rounded), round(Average_SavingsIf_Actual, digits = -3), Average_SavingsIf_Rounded)
    , TopQuartile_SavingsIf_Rounded = ifelse(is.na(TopQuartile_SavingsIf_Rounded), round(TopQuartile_SavingsIf_Actual, digits = -3), TopQuartile_SavingsIf_Rounded)
    , TopDecile_SavingsIf_Rounded = ifelse(is.na(TopDecile_SavingsIf_Rounded), round(TopDecile_SavingsIf_Actual, digits = -3), TopDecile_SavingsIf_Rounded)
  ) %>%
  select(CCGCode, TableType, StrategyDescription, Costs_Rounded, Average_SavingsIf_Rounded
         , TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded, SpellsRounded)

ipSavingsOut <- ipSavings %>% savingsOut
aeSavingsOut <- aeSavings %>% savingsOut
opSavingsOut <- opSavings %>% savingsOut

opfufSavingsOut <- opfufSavings %>% 
  mutate(
    Costs_Rounded = ifelse(is.na(Costs_Rounded), round(Costs, digits = -3), Costs_Rounded)
    , Average_SavingsIf_Rounded = ifelse(is.na(Average_SavingsIf_Rounded), round(SavingsIfAverage, digits = -3), Average_SavingsIf_Rounded)
    , TopQuartile_SavingsIf_Rounded = ifelse(is.na(TopQuartile_SavingsIf_Rounded), round(SavingsIfTopQuartile, digits = -3), TopQuartile_SavingsIf_Rounded)
    , TopDecile_SavingsIf_Rounded = ifelse(is.na(TopDecile_SavingsIf_Rounded), round(SavingsIfTopDecile, digits = -3), TopDecile_SavingsIf_Rounded)
  ) %>%
  select(CCGCode, TableType, StrategyDescription, Costs_Rounded, Average_SavingsIf_Rounded
         , TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded, SpellsRounded)

allSavings <- ipSavingsOut %>%
  bind_rows(aeSavingsOut, opSavingsOut, opfufSavingsOut) %>%
  left_join(allCCGs, by = "CCGCode") %>%
  select(CCGCode, CCGDescription, TableType, StrategyDescription, Costs_Rounded, Average_SavingsIf_Rounded,
         TopQuartile_SavingsIf_Rounded, TopDecile_SavingsIf_Rounded, SpellsRounded) %>%
  mutate(StrategyDescription = gsub("<86>", "", iconv(StrategyDescription, "", "ASCII", "byte"))) # removes the cross <U+0086> from the end of string

# check there's nothing missing
all(complete.cases(allSavings))


topQuartileLessThanAvg <- allSavings %>% filter(TopQuartile_SavingsIf_Rounded < Average_SavingsIf_Rounded)

# Send summary savings out to CSV -----------------------------------------
setwd("H:/QIPP/Data")
options(scipen = 30)

write_csv(allSavings, "R_CollatedSavings.csv")

# staffsSavings <- allSavings %>%
#   filter(CCGCode %in% c("04Y", "05D", "05G", "05Q", "05V", "05W"))
# 
# 
# write_csv(staffsSavings, "R_StaffordshireCollatedPotentialSavings.csv")
# 
# 
# 
# oldAllSavings <- read_csv("R_CollatedSavings_Old.csv")
# 
# oldCCGs <- oldAllSavings %>%
#   select(CCGCode) %>%
#   distinct(CCGCode) 
# 
# 
# nowCCGs <- allSavings %>%
#   select(CCGCode) %>%
#   distinct(CCGCode)
# 
# oldCCGs %>%
#   anti_join(nowCCGs, by = "CCGCode")
# 
# 
# 
# test <- allSavings %>%
#   inner_join(oldAllSavings, by = c("CCGCode", "StrategyDescription"))
# 
# diffCosts <- test %>% 
#   filter(abs(Costs_Rounded.y - Costs_Rounded.x) > 0) %>%
#   mutate(CostsDifference = abs(Costs_Rounded.y - Costs_Rounded.x))
# 
# diffSavings <- test %>% 
#   filter(abs(TopDecile_SavingsIf_Rounded.y - TopDecile_SavingsIf_Rounded.x) > 0) %>%
#   mutate(SavingsDifference = abs(TopDecile_SavingsIf_Rounded.y - TopDecile_SavingsIf_Rounded.x))
