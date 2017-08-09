# Check for NA in first or last year

# Get counts of activity by ccg, strategy, and year
ipSummary <- ipData %>% 
  select(-Costs, -DSRate, -DSRateVar, -DSCosts, -DSCostsVar) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -FYear, convert = T) %>%
  group_by(CCGCode, Strategy, FYear, Highlighted) %>%
  summarise(Spells = sum(Spells, na.rm = TRUE)) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted)

opSummary <- opData %>% 
  select(-Costs, -DSRate, -DSRateVar, -DSCosts, -DSCostsVar, -starts_with("FUF")) %>%
  gather(Strategy, Highlighted, -CCGCode, -Attendances, -FYear, convert = T) %>%
  group_by(CCGCode, Strategy, FYear, Highlighted) %>%
  summarise(Attendances = sum(Attendances, na.rm = TRUE)) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted)

aeSummary <- aeData %>% 
  select(-Costs, -DSRate, -DSRateVar, -DSCosts, -DSCostsVar) %>%
  gather(Strategy, Highlighted, -CCGCode, -Attendances, -FYear, convert = T) %>%
  group_by(CCGCode, Strategy, FYear, Highlighted) %>%
  summarise(Attendances = sum(Attendances, na.rm = TRUE)) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted)


# The above method would miss any subgroupss with no activity, so let's check for those.
ipCheck <- expand.grid(
    CCGCode = allCCGs$CCGCode
    , Strategy = (activeStrategies %>% filter(TableType == "IP"))$Strategy
    , FYear = seq(rocParameters$From, rocParameters$To, 101)
    , stringsAsFactors = FALSE
  ) %>%
  left_join(ipSummary, by = c("CCGCode", "Strategy", "FYear")) 

opCheck <- expand.grid(
    CCGCode = allCCGs$CCGCode
    , Strategy = (activeStrategies %>% filter(TableType == "OP"))$Strategy
    , FYear = seq(rocParameters$From, rocParameters$To, 101)
    , stringsAsFactors = FALSE
  ) %>%
  left_join(opSummary, by = c("CCGCode", "Strategy", "FYear")) 

aeCheck <- expand.grid(
    CCGCode = allCCGs$CCGCode
    , Strategy = (activeStrategies %>% filter(TableType == "AE"))$Strategy
    , FYear = seq(rocParameters$From, rocParameters$To, 101)
    , stringsAsFactors = FALSE
  ) %>%
  left_join(aeSummary, by = c("CCGCode", "Strategy", "FYear")) 


# List the invalid ones
ipInvalid <- ipCheck %>% 
  filter(is.na(Spells), !Strategy %in% c("Readmissions_v1", "Canc_Op_v1")) %>%
  select(-Spells)

opInvalid <- opCheck %>%
  filter(is.na(Attendances)) %>%
  select(-Attendances) 

aeInvalid <- aeCheck %>%
  filter(is.na(Attendances)) %>%
  select(-Attendances) 




  
  
  