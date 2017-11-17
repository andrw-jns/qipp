# Rate of change plot functions ---------------------------------------------------

# depends upon:
#library(dplyr)
#library(tidyr)
#source("roundingAndChartLimitFunctions.R")
#active_ccg <- "05C" or similar # the ccg you want to highlight


#funnelParameters <- data.frame(
#  Years = 1
#  , RatePerPeople = 100000
#  , Smoothness = 100 #Number of points making up the funnel curve
#)
#personYears <- funnelParameters$RatePerPeople * funnelParameters$Years

#rocParameters <- data.frame(
#  From = 201213
#  , To = 201617
#  , stringsAsFactors = FALSE
#)

# Usage:
# . is a dataframe like 
# DSRate, DSRateVar, DSCosts, DSCostsVar, Spells, Costs, FYear, CCGCode, ACS_Acute, ...

# e.g.
# ipRoCPoints <- ipData %>% roc_points
# ipRoCSummary <- ipData %>% roc_summary
# ipRoCFunnels <- roc_funnels(ipRoCSummary, funnelParameters$Smoothness, personYears)

rocHierarchy <- expand.grid(
  From = seq.int(201213, 201617, 101)
  , To = seq.int(201213, 201617, 101)
  , stringsAsFactors = FALSE
) %>%
  filter(To > From) %>%
  mutate(
    PotentialHierarchyDistance = abs(rocParameters$From - From) / 101 + abs(rocParameters$To - To) / 101 
  ) %>%
  arrange(PotentialHierarchyDistance, desc(To), desc(From)) %>%
  mutate(HierarchyDistance = row_number()) %>%
  select(-PotentialHierarchyDistance)


#rocPoints - the points on the rate of change curves

# Gather rate of change information for all years and all strategies
roc_all <- . %>%
  group_by(CCGCode, Strategy) %>%
  mutate(RateOfChange_1 = (DSRate / lag(DSRate, 1)) -1
    , RateOfChange_2 = (DSRate / lag(DSRate, 2)) -1    # 1213-1415
    , RateOfChange_3 = (DSRate / lag(DSRate, 3)) -1    # 1213-1516
    , RateOfChange_4 = (DSRate / lag(DSRate, 4)) -1    # 1216-1217 # mostly interested in 4 which I think is what we get
    # , RateOfChange_5 = (DSRate / lag(DSRate, 5)) -1  # not for SUS
  ) %>%
  ungroup() %>%
  select(CCGCode, Strategy, FYear, starts_with("RateOfChange"), Spells) %>%
  gather(Periods, RateOfChange, -CCGCode, -Strategy, -FYear, -Spells, convert = TRUE) %>%
  mutate(FYear = as.integer(FYear)) %>% 
  mutate(
    Periods = gsub("^RateOfChange_", "", Periods) %>% as.integer
    , From = ifelse(FYear == 201213, FYear, FYear - (Periods * 101))
  ) %>%
  filter(
    # FYear != 201213                    # no rate of change for base year but want to include it
     !(FYear == 201314 & Periods > 1)
    , !(FYear == 201415 & Periods > 2)
    , !(FYear == 201516 & Periods > 3)
    #, !(FYear == 201314 & Periods > 4)
  ) %>%
  mutate(
    IsActiveCCG = ifelse(CCGCode == active_ccg, TRUE, FALSE)
    , IsIdealYears = ifelse(
      From == rocParameters$From 
      & FYear == rocParameters$To
      , TRUE, FALSE)
    , IsValid = ifelse(
      !(is.na(RateOfChange))
      & abs(RateOfChange) <= 10
      # & !(CCGCode %in% c("03R", "03J")) # North Kirklees and Wakefield
      , TRUE, FALSE)
  )

# get the roc years for the active CCG
roc_active <- . %>% #roc_all object
  left_join(rocHierarchy, by = c("From", "FYear" = "To")) %>%
  filter(IsActiveCCG & IsValid) %>%
  group_by(Strategy) %>%
  arrange(HierarchyDistance) %>%
  filter(row_number() == 1) %>%
  select(Strategy, FYear, From)



####Get all of the points on the x-axis

#rocSummary summary information guiding the creation of the funnels.
#roc_summary <- . %>% # . = roc_points object
roc_summary <- . %>% # . = roc_all object
  ungroup() %>%
  group_by(Strategy, FYear) %>%
  summarise(
    TotalSpells = sum(Spells)
    , ActualMinSpells = min(SpellsInBaseYear, na.rm = TRUE)
    , ActualMaxSpells = max(SpellsInBaseYear, na.rm = TRUE)
    , ActualMinRateOfChange = min(RateOfChange, na.rm = TRUE)
    , ActualMaxRateOfChange = max(RateOfChange, na.rm = TRUE)
    , AverageRateOfChange = mean(RateOfChange, na.rm = TRUE)
    , AverageFiveTrimRateOfChange = mean(RateOfChange, trim = 0.05, na.rm = TRUE)
    , MedianRateOfChange = median(RateOfChange, na.rm = TRUE)
  ) %>%
  group_by(Strategy, FYear) %>%
  mutate(
    NewMinSpells = chartLimits(ActualMinSpells, ActualMaxSpells)["Min"]
    , NewMaxSpells = chartLimits(ActualMinSpells, ActualMaxSpells)["Max"]
    , NewMinRateOfChange = roundTo(ActualMinRateOfChange -0.05, 0.05)
    , NewMaxRateOfChange = roundTo(ActualMaxRateOfChange + 0.05, 0.05)
  ) 



roc_funnels <- function(rocSummary, funnelSmoothness, personYears){
  rocFunnels <- 
    expand.grid(
      Strategy = unique(rocSummary$Strategy)
      , RowNumber = seq(1, funnelSmoothness, 1)
      , stringsAsFactors = FALSE
    ) %>%
    left_join(rocSummary, by = "Strategy") %>%
    mutate(EventSpells = NA) %>%
    arrange(Strategy)  
  
  for (i in seq(length(rocFunnels$EventSpells))){
    if(rocFunnels$RowNumber[i] == 1){
      rocFunnels$EventSpells[i] <- max(1, rocFunnels$NewMinSpells[i])
    } else {
      rocFunnels$EventSpells[i] <- 
        max(
          round(
            ((rocFunnels$NewMaxSpells[i]) / 
               rocFunnels$EventSpells[i - 1]) ^ 
              (1 / ((funnelSmoothness + 1) - rocFunnels$RowNumber[i])) * rocFunnels$EventSpells[i - 1]
          )
          , rocFunnels$EventSpells[i - 1] + 1
        )
    }
  }
  
  rocFunnels <- rocFunnels %>%
    mutate(
      Denominator = EventSpells
      , StandardError =  sqrt(1 / Denominator + 1 / (Denominator * (1 + AverageRateOfChange)))
      , ThreeSigmaLower  = (AverageRateOfChange - (3 * StandardError)) 
      , TwoSigmaLower  = (AverageRateOfChange - (2 * StandardError))
      , TwoSigmaHigher  = (AverageRateOfChange +(2 * StandardError))
      , ThreeSigmaHigher  = (AverageRateOfChange + (3 * StandardError))
    ) #%>%
  #filter(!is.nan(StandardError))
  #cat("Worry not; if there were any NaNs, I filtered them out for you.")
  
  return(rocFunnels)
}
