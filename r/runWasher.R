# These packages should be loaded, but check anyway -----------------------
library(dplyr)
library(tidyr)
library(ggplot2)


# Load washer function ----------------------------------------------------
source("washer.R") #In QIPP\R

# Prepare to wash ---------------------------------------------------------
preWash <- . %>%
  select(-Costs, -DSRateVar, -DSCosts, -DSCostsVar, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted, -CCGCode, -Spells, -DSRate, -FYear, convert = T) %>%
  group_by(CCGCode, Strategy, FYear, Highlighted) %>%
  summarise(
    Spells = sum(Spells, na.rm = TRUE)
    , DSRate = sum(DSRate, na.rm = TRUE)
    ) %>%
  filter(Highlighted == 1) %>%
  select(Strategy, FYear, CCGCode, Spells) %>%
  mutate(IsActiveCCG = ifelse(CCGCode == activeCCG, TRUE, FALSE))


ipPreWasher <- ipSmall %>% preWash
aePreWasher <- aeSmall %>% preWash
opPreWasher <- opSmall %>% preWash

ipForWasher <- 
  expand.grid( # To guarantee all combinations
    Strategy = unique(ipPreWasher$Strategy)
    , FYear = unique(ipPreWasher$FYear)
    , CCGCode = comparatorCCGs$NeighbourCCGCode
    , stringsAsFactors = FALSE
  ) %>%
  left_join(ipPreWasher, by = c("Strategy", "FYear", "CCGCode")) %>%
  mutate(Spells = ifelse(is.na(Spells), 1, Spells))

aeForWasher <- 
  expand.grid( # To guarantee all combinations
    Strategy = unique(aePreWasher$Strategy)
    , FYear = unique(aePreWasher$FYear)
    , CCGCode = comparatorCCGs$NeighbourCCGCode
    , stringsAsFactors = FALSE
  ) %>%
  left_join(aePreWasher, by = c("Strategy", "FYear", "CCGCode")) %>%
  mutate(Spells = ifelse(is.na(Spells), 1, Spells))

opForWasher <- 
  expand.grid( # To guarantee all combinations
    Strategy = unique(opPreWasher$Strategy)
    , FYear = unique(opPreWasher$FYear)
    , CCGCode = comparatorCCGs$NeighbourCCGCode
    , stringsAsFactors = FALSE
  ) %>%
  left_join(opPreWasher, by = c("Strategy", "FYear", "CCGCode")) %>%
  mutate(Spells = ifelse(is.na(Spells), 1, Spells))

class(ipForWasher) <- "data.frame"
class(aeForWasher) <- "data.frame"
class(opForWasher) <- "data.frame"
rm(ipPreWasher, aePreWasher, opPreWasher)
# Wash --------------------------------------------------------------------

washed <- . %>%
  washer(.) %>%
  filter(test.AV > 5) %>%
  mutate(
    fen = as.character(fen)
    , t.2 = t.2 %>% as.character %>% as.integer
    , series = as.character(series)) %>%
  rename(Strategy = fen, FYear = t.2, CCGCode = series, Spells = y.2) %>%
  select(-y.1, -y.3, -n) %>%
  mutate(IsActiveCCG = ifelse(CCGCode == activeCCG, TRUE, FALSE))
  
ipWashed <- ipForWasher %>% washed
opWashed <- opForWasher %>% washed
aeWashed <- aeForWasher %>% washed


# Plot results ------------------------------------------------------------


ggplotwashed <- function(toWash, washed){
  
  ggplot() +
   geom_line(data = toWash, aes(x = FYear, y = Spells, group = CCGCode, colour = IsActiveCCG)) +
   geom_point(data = washed, aes(x = FYear, y = Spells, group = CCGCode, colour = IsActiveCCG)) +
   facet_wrap(~Strategy, scales= "free") +
   scale_colour_manual(values = c("grey60", colourBlindPalette["red"] %>% unname)) +
   theme(legend.position = "none")
}


# ggplotwashed(ipForWasher, ipWashed)
# ggplotwashed(opForWasher, opWashed)
# ggplotwashed(aeForWasher, aeWashed)
#


  