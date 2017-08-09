rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)

setwd("H:/QIPP/Data")

checkPbR <- read_csv(
  file = "CheckInpatientsPbR.csv"
  , col_names = FALSE
  , col_types = "idccicc"
  , na = "NULL"
  , skip = 2
  )
colnames(checkPbR) <- read_csv(file = "CheckInpatientsPbR.csv", n_max = 0) %>% colnames

# oops i forgot to SET NOCOUNT ON
checkPbR <- head(checkPbR, -2)

totalsCheck <- checkPbR %>%
  group_by(CCGCode, MonthEndOfSpell) %>%
  summarise(
    Events = sum(Events, na.rm = TRUE)
    , Cost = sum(Cost, na.rm = TRUE)
  ) %>%
  mutate(
    MonthEndOfSpell = 
      paste(substring(MonthEndOfSpell, 1, 4)
      , substring(MonthEndOfSpell, 5, 6)
      , "01", sep = "-") %>% as.Date
  ) %>%
  inner_join(allCCGs, by = "CCGCode")

setwd("H:/QIPP")
ggplot(totalsCheck) +
  geom_line(aes(x = MonthEndOfSpell, y = Events)) +
  facet_wrap(~CCGDescription, scales = "free", ncol = 2) +
  ggsave("Images/IPSpells.png", height = 250, width = 20, units = "cm", limitsize = FALSE)

ggplot(totalsCheck) +
  geom_line(aes(x = MonthEndOfSpell, y = Cost)) +
  facet_wrap(~CCGDescription, scales = "free", ncol = 2) +
  ggsave("Images/IPCosts.png", height = 250, width = 20, units = "cm", limitsize = FALSE)
