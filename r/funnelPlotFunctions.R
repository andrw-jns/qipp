# Funnel plot functions ---------------------------------------------------

# depends upon:
#library(dplyr)
#library(tidyr)
#source("roundingAndChartLimitFunctions.R")
#activeCCG <- "05C" or similar # the ccg you want to highlight
#rocParameters$To <- 201415 # the financial year the funnel is for.

# Usage:

 
#> funnelParameters$Smoothness # number of points in funnel curve
#[1] 200
#> personYears
#[1] 1e+05

# ipFunnelSummary <- ipFunnelPoints %>% funnel_summary
# ipFunnelFunnels <- funnel_funnels(ipFunnelSummary, funnelParameters$Smoothness, personYears)


# function used to convert raw data to ipFunnelPoints
# process <- . %>% 
#   select(-CCGDescription, -ShortName) %>%
#   gather(Strategy, Highlighted, -CCGCode, -Spells, -DSRate
#          , -FYear, -Costs, -DSCosts, -DSRateVar, -DSCostsVar, convert = T) %>%
#   filter(Highlighted == 1) %>%
#   select(-Highlighted) %>%
#   group_by(CCGCode, Strategy, FYear) %>%
#   summarise(
#     Spells = sum(Spells, na.rm = TRUE)
#     , DSRate = sum(DSRate, na.rm = TRUE)
#     , Costs = sum(Costs, na.rm = TRUE)
#     , DSCosts = sum(DSCosts, na.rm = TRUE)
#     , DSRateVar = sum(DSRateVar, na.rm = TRUE)
#     , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   mutate(
#     DerivedPopulation = (Spells / DSRate) * personYears
#     , IsActiveCCG = ifelse(CCGCode == activeCCG, TRUE, FALSE)  
#   )


#> str(ipFunnelPoints)
#Classes 'tbl_df', 'tbl' and 'data.frame':	630 obs. of  11 variables:
# $ CCGCode          : chr  "05A" "11H" "03A" "03N" ...
# $ Strategy         : chr  "ACS_Acute_v3" "ACS_Acute_v3" "ACS_Acute_v3" "ACS_Acute_v3" ...
# $ FYear            : int  201415 201415 201415 201415 201415 201415 201415 201415 201415 201415 ...
# $ Spells           : int  2487 2075 1375 2868 2504 2333 1532 2139 2340 1624 ...
# $ DSRate           : num  581 489 547 518 478 ...
# $ Costs            : int  4385275 3736846 1820000 4793896 3901956 2799433 2626692 2812274 3210424 2187483 ...
# $ DSCosts          : num  1119545 995629 770070 933969 765969 ...
# $ DSRateVar        : num  291 260 456 198 188 ...
# $ DSCostsVar       : num  604592 584486 679021 382859 311889 ...
# $ DerivedPopulation: num  428032 424034 251478 553374 523757 ...
# $ IsActiveCCG      : logi  TRUE FALSE FALSE FALSE FALSE FALSE ...

  
funnel_summary <- . %>% #. = processed object down to one year
  group_by(Strategy) %>%
  summarise(
    TotalPersonYears = sum(DerivedPopulation) * funnelParameters$Years
    , TotalSpells = sum(Spells)
    , ActualMinSpells = min(Spells, na.rm = TRUE)
    , ActualMaxSpells = max(Spells, na.rm = TRUE)
    , ActualMinDSRate = min(DSRate, na.rm = TRUE)
    , ActualMaxDSRate = max(DSRate, na.rm = TRUE)
    , ActualMinDerivedPopulation = min(DerivedPopulation, na.rm = TRUE)
    , ActualMaxDerivedPopulation = max(DerivedPopulation, na.rm = TRUE)
  ) %>%
  mutate(Average = (TotalSpells / TotalPersonYears) * personYears) %>%
  group_by(Strategy) %>%
  mutate(
    NewMinSpells = chartLimits(ActualMinSpells, ActualMaxSpells)["Min"]
    , NewMaxSpells = chartLimits(ActualMinSpells, ActualMaxSpells)["Max"]
    , NewMinDSRate = chartLimits(ActualMinDSRate, ActualMaxDSRate)["Min"]
    , NewMaxDSRate = chartLimits(ActualMinDSRate, ActualMaxDSRate)["Max"]
    , NewMinDerivedPopulation = chartLimits(ActualMinDerivedPopulation, ActualMaxDerivedPopulation)["Min"]
    , NewMaxDerivedPopulation = chartLimits(ActualMinDerivedPopulation, ActualMaxDerivedPopulation)["Max"]
  )



funnel_funnels <- function(funnelSummary, funnelSmoothness, personYears){
  funnelFunnels <- 
    expand.grid(
      Strategy = unique(funnelSummary$Strategy)
      , RowNumber = seq(1, funnelSmoothness, 1)
      , stringsAsFactors = FALSE
    ) %>%
    left_join(funnelSummary, by = "Strategy") %>%
    mutate(EventSpells = NA) %>%
    arrange(Strategy) 
  
  for (i in seq(length(funnelFunnels$EventSpells))){
    if(funnelFunnels$RowNumber[i] == 1){
      funnelFunnels$EventSpells[i] <- max(1, funnelFunnels$NewMinSpells[i])
    } else {
      funnelFunnels$EventSpells[i] <- 
        max(
          round(
            ((funnelFunnels$NewMaxDerivedPopulation[i]) / #formerly Spells
               funnelFunnels$EventSpells[i - 1]) ^ 
                  (1 / ((funnelSmoothness + 1) - funnelFunnels$RowNumber[i])) * 
                    funnelFunnels$EventSpells[i - 1]
          )
          , funnelFunnels$EventSpells[i - 1] + 1
        )
    }
  }
  funnelFunnels <- funnelFunnels %>%
  ungroup() %>%
  mutate(Denominator = EventSpells / Average * personYears)

  funnelFunnels$ThreeSigmaLower  <- sapply(funnelFunnels$EventSpells, function(x) poisson.test(x, alternative = "two.sided", conf.level = 0.99)$conf.int[1]) / funnelFunnels$Denominator * personYears
  funnelFunnels$TwoSigmaLower    <- sapply(funnelFunnels$EventSpells, function(x) poisson.test(x, alternative = "two.sided", conf.level = 0.95)$conf.int[1]) / funnelFunnels$Denominator * personYears
  funnelFunnels$TwoSigmaHigher   <- sapply(funnelFunnels$EventSpells, function(x) poisson.test(x, alternative = "two.sided", conf.level = 0.95)$conf.int[2]) / funnelFunnels$Denominator * personYears
  funnelFunnels$ThreeSigmaHigher <- sapply(funnelFunnels$EventSpells, function(x) poisson.test(x, alternative = "two.sided", conf.level = 0.99)$conf.int[2]) / funnelFunnels$Denominator * personYears
  
  # Remove bits from the ends of the funnels that would be above/below visible range of the funnel plot
  # This is just to avoid a warning when plotting.
  funnelFunnels <- funnelFunnels %>%
    filter(TwoSigmaHigher <= NewMaxDSRate 
           & TwoSigmaLower >= NewMinDSRate)
  
  return(funnelFunnels)
}
