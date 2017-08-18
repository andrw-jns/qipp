total_activity <- . %>%
  filter(FYear == f_year & CCGCode == active_ccg) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , Costs = sum(Costs, na.rm = TRUE)
  )

savings_any_one <- . %>% 
  ungroup(.) %>% 
  filter(., FYear == f_year) %>% 
  select(., -FYear, -TypeNumber, -Low, -High) %>% 
  filter(., Type != "MinDSRate") %>% 
  spread(., Type, DSRate)

# savings_any_one <- . %>%
#   ungroup() %>%
#   filter(FYear == rocParameters$To) %>%
#   select(-FYear, -TypeNumber, -Low, -High) %>%
#   filter(Type != "MinDSRate") %>%
#   spread(Type, DSRate)

significance_summary <- function(funnelPoints, funnelFunnels, rocPoints, rocFunnels){
  rocPoints <- rocPoints %>%
    filter(CCGCode == active_ccg) %>% ungroup() %>% select(-CCGCode)
  rocFunnels <- rocFunnels %>% select(-FYear)
  
  significance <- funnelPoints %>%
    group_by(CCGCode, Strategy) %>%
    filter(CCGCode == active_ccg) %>%
    left_join(funnelFunnels, by = "Strategy") %>%
    mutate(Test = DerivedPopulation - Denominator
           , AbsTest = abs(Test)
           , TestRank = rank(AbsTest, ties.method = "first")) %>%
    filter(TestRank == 1) %>%
    mutate(Significance = ifelse(DSRate > TwoSigmaHigher, "High", 
                        ifelse(DSRate < TwoSigmaLower, "Low", "Not Significant"))) %>%
    select(CCGCode, Strategy, Significance) %>%
    left_join(rocPoints, by = "Strategy") %>%
    left_join(rocFunnels, by = "Strategy") %>%
    mutate(Test = SpellsInBaseYear - Denominator
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
  
  return(significance)
}


summary_output <- function(dataSmall, savingsAnyOne, significanceSummary, totalActivity){
  summaryOutput <- dataSmall %>%
    filter(FYear == f_year & CCGCode == active_ccg) %>%
    select(-FYear, -DSRateVar, -DSCosts, -DSCostsVar, -CCGCode, -CCGDescription, -ShortName) %>%
    gather(Strategy, Highlighted,  -Spells, -Costs, -DSRate, convert = T) %>%
    group_by(Strategy, Highlighted) %>%
    summarise_each(
      funs(sum(., na.rm = TRUE))
      , Spells, Costs, DSRate
    ) %>%
    filter(Highlighted == 1) %>%
    select(-Highlighted) %>%
    left_join(savingsAnyOne, by = "Strategy") %>%
    mutate_each(
      funs(
        Comparators = (.)
        , SavingsIf = (Costs - (. / DSRate) * Costs)) #generate savings if average
      , Average, TopQuartile, TopDecile) %>%
    mutate_each(funs(ifelse( . < 0 , 0, .)), matches("_SavingsIf")) %>% # remove negative savings
    mutate(
      SpellsRounded = roundTo(Spells, 10)
      , propSpells = Spells / totalActivity$Spells) %>%
    mutate_each(funs(
      Actual = (.)
      , Rounded = roundTo(., 1000)
    ), Costs, matches("_SavingsIf")) %>%
    left_join(activeStrategies, by = "Strategy") %>%
    select(-StrategyType) %>%
    left_join(significanceSummary, by ="Strategy")
  
  return(summaryOutput)
}