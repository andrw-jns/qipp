sus_coverage <- ipData %>%
  group_by(CCGCode, FYear) %>%
  summarise(attendances = sum(Spells)) %>% 
  spread(FYear, attendances) %>% 
  arrange(desc(`201617`)) %>% 
  left_join(allCCGs, by = "CCGCode") %>% 
  select(ShortName, everything(), -CCGDescription)

getwd()
write_csv(sus_coverage, "sus_coverage.csv")
