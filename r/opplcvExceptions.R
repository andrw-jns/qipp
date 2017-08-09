# all OP PLCV in 201415
opplcvCheck <- opCheck %>% 
  filter(Strategy == "PLCV_v1" & 
           FYear == rocParameters$To) %>%
  select(-FYear) %>%
  rename(AttendancesToYear = Attendances)

# Derive rate of change for all possible years.
opplcvRocCheck <- opCheck %>% 
  filter(Strategy == "PLCV_v1") %>%
  group_by(CCGCode, Strategy) %>%
  left_join(opplcvCheck, by = c("CCGCode", "Strategy")) %>%
  filter(FYear != rocParameters$To) %>%
  mutate(RateOfChange = (AttendancesToYear / Attendances) -1)


# find the earliest year
opplcvExceptions <- opplcvRocCheck %>%
  inner_join(
    # all where rate of change > 1000% from 201011 to 201415
    opplcvRocCheck %>%
    filter(FYear == rocParameters$From &
           RateOfChange > 10) %>%
    select(CCGCode, Strategy)
    , by = c("CCGCode", "Strategy")) %>%
  filter(RateOfChange <= 10) %>%
  filter(row_number() == 1) %>%
  select(CCGCode, Strategy, FYear) %>%
  rename(From = FYear) %>%
  mutate(To = 201415)
