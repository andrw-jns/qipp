
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


# IP tbl summary -----------------------------------------------------

summ_ip_summ_out <- summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) 


# IP cost summary ----------------------------------------------------

summ_ip_cost_out <- # head(
  summaryOutputIP %>%
  ungroup %>%
  filter(!Strategy %in% c("Canc_Op_v1", "Readmissions_v1")) %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  )




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


# AE tbl summary -----------------------------------------------------


summ_ae_summ_out <- summaryOutputAE %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) 


# AE cost summary ----------------------------------------------------

summ_ae_cost_out <- # head(
  summaryOutputAE %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  )



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



# OP tbl summary -----------------------------------------------------

summ_op_summ_out <- summaryOutputOP %>%
  ungroup %>%
  select(Strategy, SpellsRounded, Costs_Rounded, Significance, RocSignificance) %>% 
  mutate(SpellsRounded = scales::comma(SpellsRounded),
         Costs_Rounded =  pound(Costs_Rounded)
  ) 


# OP cost summary ----------------------------------------------------

summ_op_cost_out <- # head(
  summaryOutputOP %>%
  ungroup %>%
  select(Strategy, Costs_Rounded, Average_SavingsIf_Rounded, TopQuartile_SavingsIf_Rounded) %>% 
  mutate(Costs_Rounded =  pound(Costs_Rounded)
         , Average_SavingsIf_Rounded =  pound(Average_SavingsIf_Rounded)
         , TopQuartile_SavingsIf_Rounded =  pound(TopQuartile_SavingsIf_Rounded)
  )


