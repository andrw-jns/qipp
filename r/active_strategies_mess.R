"Now do some wrangling to add new set of names to activeStrategies:"

"WARNING: THIS IS A MANUAL PROCESS"
"FOUNDATIONS (THE FORMAT OF THE SOURCE CSV) COULD BE IMPROVED IN FUTURE"
# manually assign ids (based on matching old strats to master list with names):
id_ref <- c(1,   1,   1,  2,  2,  2, 26, 25, 29,  5, 8,
            7,  24,  33, 33, 33, 33, 35, 23,  6,  9,
            9,   9,   9,  9, 28, 28, 28, 28, 11, 12,
            14, 14,  NA, 13, 32, 32, 32, 32, 13, 13,     # OP PLCV = NA
            10,  39, 39,  4,  3)

new_strat_list <- qippStrategiesMasterList_csv <- read_excel("C:/2017_projects/qipp/data/qippStrategiesMasterList.xlsx") %>% 
  filter(id %in% id_ref)


activeStrategies <- read_csv("listActiveStrategies.csv") %>% 
  # manually assign ids (based on matching old strats to master list with names)
  mutate(id = id_ref) %>% 
  # left_join(new_strat_list, by = "id") %>% 
  arrange(id)

activeStrategies$Strategy[activeStrategies$Strategy == "Alcohol_25pcto75pc_v3"]  <- "alc_wholly"
activeStrategies$Strategy[activeStrategies$Strategy == "Alcohol_5pcto25pc_v3"]   <- "alc_chronic"
activeStrategies$Strategy[activeStrategies$Strategy == "Alcohol_75pcto100pc_v3"] <- "alc_acute"

# removing unused strategies (FUF, OP PLCV):
activeStrategies <- slice(activeStrategies, c(1:34, 39:45))


"Adapt (wrangle) new names master list:"
strats_new <- new_strat_list %>% 
  select(id, oldName, shortName, longName, breakdownAvailable, dplyr::matches("breakdown[1-5]"))

# fix for alcohol
# tmp[2, 7:8] <- "should be updated to match indicator source"

strats_new[2,6] <- "Wholly Attributable"
strats_new[2,7] <- "Partially Attributable - Chronic Conditions"
strats_new[2,8] <- "Partially Attributable - Acute Conditions"

strats_new2 <- gather(strats_new, "breakdown", "sub_header", dplyr::matches("breakdown[1-5]"))
# tmp2 <- tmp2 %>% distinct(id, breakdown, .keep_all = T)

strats_new3 <- strats_new2 %>% 
  filter(breakdownAvailable == 0) %>% 
  distinct(longName, .keep_all = T)

strats_new4 <- strats_new2 %>% 
  filter(breakdownAvailable == 1) %>% 
  arrange(id) %>% 
  na.omit()

strats_new_final <- bind_rows(strats_new3, strats_new4) %>% 
  arrange(id)

# This "join" is in fact a bind cols - so may not work in future:

activeStrategies <- bind_cols(activeStrategies, strats_new_final)

"NOW COMES A REAL BODGE:"

obes_some <- pull(activeStrategies[22,13])
obes_wh   <- pull(activeStrategies[21,13])

activeStrategies[22,13] <- obes_wh
activeStrategies[21,13] <- obes_some


plcv_aes <- pull(activeStrategies[31,13])
plcv_cost <- pull(activeStrategies[33,13])
plcv_ineff <- pull(activeStrategies[30,13])
plcv_harm <- pull(activeStrategies[32,13])


activeStrategies[30,13] <- plcv_aes
activeStrategies[31,13] <- plcv_cost
activeStrategies[32,13] <- plcv_ineff
activeStrategies[33,13] <- plcv_harm


med_ad <- pull(activeStrategies[36,13])
med_child <- pull(activeStrategies[38,13])
surg_ad <- pull(activeStrategies[35,13])
surg_child <- pull(activeStrategies[37,13])


activeStrategies[35,13] <- med_ad
activeStrategies[36,13] <- med_child
activeStrategies[37,13] <- surg_ad
activeStrategies[38,13] <- surg_child

activeStrategies[40:41,10] <- "No Overnight Stay, No Procedure, Discharged"
activeStrategies[29,10]    <- "Ambulance Conveyed, No Investigations, Not Admitted"

