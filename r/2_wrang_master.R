
# *****chckpnt**** -----------------------------------------------------------------

# CCG selections
comparatorCCGs2 <- allCCGs %>%
  filter(CCGCode %in% qipp_ccgs,
         CCGCode != active_ccg)


activeCCGInfo <- allCCGs %>% 
  filter(CCGCode == active_ccg) %>% 
  mutate(CCGNameMinusCCG = stringr::str_replace(CCGDescription, " CCG", "")) # %>% 
# unlist()


# Munge data ---------------------------------------------------------------
setwd(paste0(baseDir, "data"))

comparatorCCGs2 <- allCCGs %>% 
  filter(CCGCode %in% qipp_ccgs)

# Remove rows where the CCGCode is not valid.
removeInvalidCCGs <- . %>%
  inner_join(comparatorCCGs2, by = "CCGCode")


ipSmall <- ipData %>% removeInvalidCCGs
# rm(ipData) # RAM saver!
# gc() # call after a large object has been removed

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
