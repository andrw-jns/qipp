
# Summaries  ---------------------------------------------------------

setwd(baseDir)

summ_ipFunnelPoints    <- ip    %>% filter(FYear == f_year) 
summ_opFunnelPoints    <- op    %>% filter(FYear == f_year)
summ_aeFunnelPoints    <- ae    %>% filter(FYear == f_year)

summ_ipFunnelSummary <- summ_ipFunnelPoints %>% funnel_summary
summ_aeFunnelSummary <- summ_aeFunnelPoints %>% funnel_summary
summ_opFunnelSummary <- summ_opFunnelPoints %>% funnel_summary

summ_ipFunnelFunnels <- funnel_funnels(summ_ipFunnelSummary, funnelParameters$Smoothness, personYears)
summ_aeFunnelFunnels <- funnel_funnels(summ_aeFunnelSummary, funnelParameters$Smoothness, personYears)
summ_opFunnelFunnels <- funnel_funnels(summ_opFunnelSummary, funnelParameters$Smoothness, personYears)


# Inpatient ---------------------------------------------------------------

totalActivityIP <- ipSmall %>% total_activity
savingsAnyOneIP <- ipTrendComparators %>% savings_any_one

ipSignificance  <- significance_summary(summ_ipFunnelPoints, summ_ipFunnelFunnels, ipRoC, ipRoCFunnels)


summaryOutputIP <- ipSmall %>%
  filter(FYear == f_year & CCGCode == active_ccg) %>%
  select(-FYear, -DSRateVar, -DSCosts, -DSCostsVar, -CCGCode, -CCGDescription, -ShortName) %>%
  gather(Strategy, Highlighted,  -Spells, -Costs, -DSRate, convert = T) %>%
  mutate(aaf_reduction_factor = NA)

summaryOutputIP2 <- summaryOutputIP %>% 
  filter(Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  mutate(aaf_reduction_factor = Highlighted/Spells) %>% 
  mutate_at(vars(Spells, DSRate, Costs),
            funs(. * aaf_reduction_factor)) %>% 
  mutate(Highlighted = if_else(is.na(Highlighted)| Highlighted == 0, 0, 1)) 

summaryOutputIP3 <- summaryOutputIP %>% 
  filter(!Strategy %in% c("alc_wholly", "alc_chronic", "alc_acute")) %>% 
  bind_rows(summaryOutputIP2) %>% 
  select(-aaf_reduction_factor) %>% 
  group_by(Strategy, Highlighted) %>%
  summarise_all(
    funs(sum(., na.rm = TRUE))
    #, Spells, Costs, DSRate
  ) %>%
  filter(Highlighted == 1) %>%
  select(-Highlighted) %>%
  left_join(savingsAnyOneIP, by = "Strategy") %>%
  mutate_at(vars(Average, TopQuartile, TopDecile),
            funs(
              Comparators = (.)
              , SavingsIf = (Costs - (. / DSRate) * Costs)) #generate savings if average
  ) %>%
  mutate_at(vars(matches("_SavingsIf")), funs(ifelse( . < 0 , 0, .))) %>% # remove negative savings
  mutate(
    SpellsRounded = roundTo(Spells, 10)
    , propSpells = Spells / totalActivityIP$Spells) %>%
  mutate_at(vars(matches("_SavingsIf"), Costs), 
            funs(
              Actual = (.)
              , Rounded = roundTo(., 1000)
            )) %>%
  left_join(activeStrategies, by = "Strategy") %>%
  select(-StrategyType) %>%
  left_join(ipSignificance, by ="Strategy") %>% 
  
  # summaryOutputIP <- ipSmall %>% summary_output(., savingsAnyOneIP, ipSignificance, totalActivityIP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(ipTrendActive %>% 
              filter(FYear == f_year) %>%
              ungroup() %>%
              select(Strategy, DSRateCIUpper, DSRateCILower)
            , by = "Strategy") %>%
  left_join(ipCost %>% # is this actually needed now?s
              ungroup() %>%
              select(CCGCode, Strategy, DSCostsPerHead)
            , by = c("CCGCode", "Strategy"))

summaryOutputIP <- summaryOutputIP3

# IP labels for charts / tables------------------------------------------


labels_ip <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("ACS Acute",
                         "ACS Chronic",
                         "ACS Vaccine",
                         "Alcohol (wholly)",
                         "Alcohol (partially - chronic)",
                         "Alcohol (partially - acute)",
                         "",
                         "End of Life Care (3-14 days)",
                         "End of Life Care (0-2 days)",
                         "Falls",
                         "Frail Elderly (occasional)",
                         "Frail Elderly (usual)",
                         "Medically Unexplained",
                         "Medicines - Explicit",
                         "Medicines - Implicit AntiDiab",
                         "Medicines - Implicit Benzo",
                         "Medicines - Implicit Diuretics",
                         "Medicines - Implicit NSAIDs",
                         "Obesity (largely)",
                         "Obesity (marginal)",
                         "Obesity (somewhat)",
                         "PLCV Cosmetic",
                         "PLCV Alternative",
                         "PLCV Ineffective",
                         "PLCV Risks",
                         "Mental Health Admissions from ED",
                         "",
                         "Self-harm",
                         "Smoking (large)",
                         "Smoking (somewhat)",
                         "Zero Length of Stay (adult)",
                         "Zero Length of Stay (child)"))



# IP tbl summary -----------------------------------------------------

summ_ip_summ_out <- summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>%
  mutate(Costs_Rounded = Costs_Rounded / 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  `colnames<-`(c("Strategy", "Admissions", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change")) %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)

summ_ip_summ_out[4:5][summ_ip_summ_out[4:5] == "Not Significant"] <- "-"


# IP cost summary ----------------------------------------------------


summ_ip_cost_out <- # head(
  summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded = pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  `colnames<-`(c("Strategy", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)




# A&E ---------------------------------------------------------------------
totalActivityAE <- aeSmall %>% total_activity
savingsAnyOneAE <- aeTrendComparators %>% savings_any_one

aeSignificance <- significance_summary(summ_aeFunnelPoints, summ_aeFunnelFunnels, aeRoC, aeRoCFunnels)

summaryOutputAE <- aeSmall %>% summary_output(., savingsAnyOneAE, aeSignificance, totalActivityAE) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(aeTrendActive %>% 
              filter(FYear == f_year) %>%
              ungroup() %>%
              select(Strategy, DSRateCIUpper, DSRateCILower)
            , by = "Strategy") %>%
  left_join(aeCost %>%
              ungroup() %>%
              select(CCGCode, Strategy, DSCostsPerHead)
            , by = c("CCGCode", "Strategy"))



# A&E labels for charts / tables------------------------------------------


labels_ae <- summaryOutputAE %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("Ambulance Conveyed, No Treatment",
                         "Frequent Attenders",
                         "Left Before Seen",
                         "Low Acuity ED"
  ))

# AE tbl summary -----------------------------------------------------


summ_ae_summ_out <- summaryOutputAE %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  right_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change"))

summ_ae_summ_out[4:5][summ_ae_summ_out[4:5] == "Not Significant"] <- "-"


# AE cost summary ----------------------------------------------------


summ_ae_cost_out <- # head(
  summaryOutputAE %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) 


# Outpatients -------------------------------------------------------------
totalActivityOP <- opSmall %>% total_activity
savingsAnyOneOP <- opTrendComparators %>% savings_any_one

opSignificance <- significance_summary(summ_opFunnelPoints, summ_opFunnelFunnels, opRoC, opRoCFunnels)

summaryOutputOP <- opSmall %>% summary_output(., savingsAnyOneOP, opSignificance, totalActivityOP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(opTrendActive %>% 
              filter(FYear == f_year) %>%
              ungroup() %>%
              select(Strategy, DSRateCIUpper, DSRateCILower)
            , by = "Strategy") %>%
  left_join(opCost %>%
              ungroup() %>%
              select(CCGCode, Strategy, DSCostsPerHead)
            , by = c("CCGCode", "Strategy"))


# OP labels for charts / tables------------------------------------------

labels_op <- summaryOutputOP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("Consultant-Consultant Referral",
                         "GP referred Medical (adult)",
                         "GP referred Medical (child)",
                         "GP referred Surg (adult)",
                         "GP referred Surg (child)",
                         ""
  ))




# OP tbl summary -----------------------------------------------------


summ_op_summ_out <- summaryOutputOP %>%
  filter(!Strategy %in% c("PLCV_v1")) %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "2016-17 Spend (000s)",
                 "Rate", "Rate of Change")) 

summ_op_summ_out[4:5][summ_op_summ_out[4:5] == "Not Significant"] <- "-"

# OP cost summary ----------------------------------------------------

summ_op_cost_out <- summaryOutputOP %>%
  filter(!Strategy %in% c("PLCV_v1")) %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded = Costs_Rounded/ 1000,
         Average_SavingsIf_Rounded = Average_SavingsIf_Rounded / 1000,
         TopQuartile_SavingsIf_Rounded = TopQuartile_SavingsIf_Rounded/ 1000) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>%
  # add footnote "compared to CCGs in the West Midlands"
  `colnames<-`(c("Opportunity", "2016-17 Spend (000s)", "Savings if Average (000s)",
                 "Savings if Top Quartile (000s)")) 


