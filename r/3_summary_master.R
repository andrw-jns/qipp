
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

summaryOutputIP <- ipSmall %>% summary_output(., savingsAnyOneIP, ipSignificance, totalActivityIP) %>%
  group_by(ReviewNumber, add = FALSE) %>%
  mutate(ReviewDupe = row_number()) %>%
  mutate(ReviewDupe = paste0(ReviewNumber, ReviewDupe)) %>%
  left_join(ipTrendActive %>% 
              filter(FYear == f_year) %>%
              ungroup() %>%
              select(Strategy, DSRateCIUpper, DSRateCILower)
            , by = "Strategy") %>%
  left_join(ipCost %>%
              ungroup() %>%
              select(CCGCode, Strategy, DSCostsPerHead)
            , by = c("CCGCode", "Strategy"))



# IP labels for charts / tables------------------------------------------

labels_ip <- summaryOutputIP %>%
  ungroup() %>% 
  select(Strategy) %>% 
  mutate(Opportunity = c("ACS Acute",
                         "ACS Chronic",
                         "ACS Vaccine",
                         "Alcohol [25% to 75%]",
                         "Alcohol [5% to 25%]",
                         "Alcohol [75% to 100%]",
                         "",
                         "EOLC [3-14 days]",
                         "EOLC [0-2 days]",
                         "Falls",
                         "Frail Elderly [occasional]",
                         "Frail Elderly [usual]",
                         "Medically Unexplained",
                         "Meds Explicit",
                         "Meds Implicit AntiDiab",
                         "Meds Implicit Benzo",
                         "Meds Implicit Diuretics",
                         "Meds Implicit NSAIDs",
                         "Obesity [largely]",
                         "Obesity [marginal]",
                         "Obesity [somewhat]",
                         "PLCV Cosmetic",
                         "PLCV Alternative",
                         "PLCV Ineffective",
                         "PLCV Risks",
                         "MH admissions from ED",
                         "",
                         "Self-harm",
                         "Smoking [large]",
                         "Smoking [somewhat]",
                         "Zero LOS [adult]",
                         "Zero LOS [child]"))



# IP tbl summary -----------------------------------------------------

summ_ip_summ_out <- summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  `colnames<-`(c("Strategy", "Activity", "Spend 2016-17",
                 "Rate", "Rate of Change")) %>% 
  left_join(labels_ip, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy)


# IP cost summary ----------------------------------------------------

summ_ip_cost_out <- # head(
  summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  )%>% 
  `colnames<-`(c("Strategy", "Spend 2016-17", "Total Savings if Average",
                 "Total Savings if Top Quartile")) %>% 
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
  mutate(Opportunity = c("Ambulance Treat",
                         "Frequent Attenders",
                         "Left Before Seen",
                         "Low Acuity ED"
  ))


# AE tbl summary -----------------------------------------------------


summ_ae_summ_out <- summaryOutputAE %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) %>% 
  right_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "Spend 2016-17",
                 "Rate", "Rate of Change"))


# AE cost summary ----------------------------------------------------

summ_ae_cost_out <- # head(
  summaryOutputAE %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_ae, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Spend 2016-17", "Total Savings if Average",
                 "Total Savings if Top Quartile")) 



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
  mutate(Opportunity = c("Consultant-Consultant Refer",
                         "GP referred Medical [adult]",
                         "GP referred Medical [child]",
                         "GP referred Surg [adult]",
                         "GP referred Surg [child]",
                         ""
  ))


# OP tbl summary -----------------------------------------------------

summ_op_summ_out <- summaryOutputOP %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  )  %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>% 
  `colnames<-`(c("Opportunity", "Activity", "Spend 2016-17",
                 "Rate", "Rate of Change"))  


# OP cost summary ----------------------------------------------------

summ_op_cost_out <- # head(
  summaryOutputOP %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  ) %>% 
  left_join(labels_op, by = c("Strategy")) %>% 
  select(Opportunity, everything(), -Strategy) %>%
  # add footnote "compared to CCGs in the West Midlands"
  `colnames<-`(c("Opportunity", "Spend 2016-17", "Total Savings if Average",
                 "Total Savings if Top Quartile")) 



