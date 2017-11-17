zero_test <- ipSmall$aaf_red_fact[!is.na(ipSmall[12:14])] <- ipSmall[]

ipSmall %>% 
  # slice(1:100) %>% 
  select(8:40) %>% 
  gather(Strategy, Highlighted, -CCGCode) %>% 
  group_by(Strategy, CCGCode) %>% 
  mutate(index = row_number()) %>% 
  spread(Strategy, Highlighted)

"Perfect"

zero_test <- ipSmall %>% 
  slice(1:100) %>% 
  select(-CCGDescription, -ShortName) %>% 
  gather(Strategy, Highlighted, -c(1:8)) %>% 
  group_by(DSRate, DSRateVar, DSCosts, DSCostsVar, Spells, Costs, FYear, CCGCode, Strategy) %>% #Strategy, CCGCode) %>% 
  mutate(index = row_number()) %>% # A grouped mutate to add indices is the answer?
  spread(Strategy, Highlighted)


weekend1 <- ipSmall %>% 
  # slice(1:100) %>% 
  select(-CCGDescription, -ShortName) %>% 
  gather(Strategy, Highlighted, -c(1:8)) %>%
  # second try with weekend: try the grouped mutate here
  group_by(DSRate, DSRateVar, DSCosts, DSCostsVar, Spells, Costs, FYear, CCGCode, Strategy) %>% #Strategy, CCGCode) %>% 
  mutate(index = row_number()) %>%
  mutate(aaf_reduction_factor = NA)


weekend2 <- weekend1 %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, Costs, DSCosts, DSRateVar, DSCostsVar),
            funs(. * aaf_reduction_factor)) %>% 
mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 
  
  weekend3 <- weekend1 %>% 
    filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
    bind_rows(weekend2) %>% 
    select(-aaf_reduction_factor) %>%
    # group_by(DSRate, DSRateVar, DSCosts, DSCostsVar, Spells, Costs, FYear, CCGCode, Strategy) %>% #Strategy, CCGCode) %>% 
    # mutate(index = row_number()) %>% # A grouped mutate to add indices is the answer?
    spread(Strategy, Highlighted)
    
  # Way2: 
  # Error: Duplicate identifiers for rows (8057430, 8057431, 8057433, 8057434, 8057435, 8057436, 8057437, 8057440, 8057441, 8057442, 8057443, 8057444, 8057445, 8057446, 8057447, 8057448, 8057449, 8057450, 8057451, 8057452, 8057453, 8057454, 8057455, 8057457, 8057458, 8057461, 8057462, 8057465, 8057466, 8057468, 8057469, 
