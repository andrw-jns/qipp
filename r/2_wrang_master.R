
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

# opSmallFUF <- opData %>%
#   select(
#     DSRate, DSRateVar, DSCosts, DSCostsVar, Attendances, Costs, FYear, CCGCode
#     , starts_with("FUF")) %>% removeInvalidCCGs 
# 

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


# To deal with new AAFs: (there may be a more elegant way (back in SQL)?)
"YES it's probably better to do this in the SQL. But for now..."
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
# aeTrendComparators <- aeSmall %>% trend_comparators 



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


# ipTrend adjustments for alcohol: (because ipSmall should really be changed):
# Active CCG:
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

# ipCost <- ipSmall %>% cost_ds 
aeCost <- aeSmall %>% cost_ds 
opCost <- opSmall %>% cost_ds 
