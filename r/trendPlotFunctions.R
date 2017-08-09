# Trend in DSRate plot functions
trend_active <- . %>%
  filter(CCGCode == active_ccg) %>%
  select(-DSCosts, -DSCostsVar, -Costs, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -Spells, -CCGCode, -FYear, convert = T) %>%
  group_by(Strategy, FYear, Highlighted) %>%
  summarise(
    DSRate = sum(DSRate, na.rm = TRUE)
    , DSRateVar = sum(DSRateVar, na.rm = TRUE)
    , Spells = sum(Spells, na.rm = TRUE)
    ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  mutate(
    SpellsCIUpper = (Spells + 1 ) * (1 - 1 / (9 * (Spells + 1)) + trendCV / (3 * sqrt(Spells + 1))) ^ 3
    , SpellsCILower = Spells * (1 - 1 / (9 * Spells) - trendCV / (3 * sqrt(Spells))) ^ 3
    , DSRateCIUpper = DSRate + sqrt(DSRateVar / Spells) * (SpellsCIUpper - Spells)
    , DSRateCILower = DSRate + sqrt(DSRateVar / Spells) * (SpellsCILower - Spells)
    , Group = activeCCGInfo$CCGDescription
  )
  

trend_comparators <- . %>%
  select(-DSCosts, -DSCostsVar, -Costs, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -DSRate, -DSRateVar, -Spells, -CCGCode, -FYear, convert = T) %>%
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