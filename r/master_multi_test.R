############################################################################
" QIPP WRAP "
" STP WRAPPER FOR QIPP PACK"
###########################################################################

suppressPackageStartupMessages(library(tidyverse))
library(DBI)
library(odbc)
library(dbplyr)

con_ref <- dbConnect(odbc::odbc(),
                     driver   = "SQL Server",
                     server   = "CSU-SQL-03",
                     database = "StrategicReference",
                     port     = 1433
)

qipp_ccgs  <- c(# Alphabetical:
  "13P", # BXC
  "04X", # BSC
  "04Y", # CAN
  "05C", # DUD
  "05D", # EST
  "05F", # HER
  "05G", # NST
  "05J", # RED
  "05L", # SWB
  "05N", # SHR
  "05P", # SOL
  "05Q", # SES
  # "05R", # SWK -- OUTSIDE CSU
  "05T", # SWC
  "05V", # SAS
  "05W", # STO
  "05X", # TEL
  "05Y", # WAL
  # "05H", # WKN -- OUTSIDE CSU
  "06A", # WOL#
  "06D"  # WYR#
)

# dbListTables(con_ref)

ccg_stp <- tbl(con_ref, "CCG_to_STP_slim") %>% collect()

ccg_stp <- read_rds("C:/2017_projects/qipp/data/ccg_stp.RDS")

list_stps <- ccg_stp %>% 
  filter(CCG16CDH %in% qipp_ccgs) 

tmp1 <- unique(list_stps$STP17NM)
# 
# for(i in seq_along(tmp1)){
#   assign(paste0("stp_", stringr::str_replace_all(tmp1[i], "[:lower:]|\\s", "")),
#          list_stps %>% filter(STP17NM == tmp1[i]))
#   
  df <- list_stps %>% filter(STP17NM == tmp1[1])
  
  rm(active_ccg)
  stp_avg      <- tibble()
  stp_top_qrt  <- tibble()


# * Commencer la loup ------------------------------------------------


  for(j in df$CCG16CDH){
  
    active_ccg <- df$CCG16CDH[1]
  
  # 2. Wrangle ---------------------------------------------------------
  # source_here("2_wrang_master.R")
  
    # CCG selections
    comparatorCCGs2 <- allCCGs %>%
      filter(CCGCode %in% qipp_ccgs,
             CCGCode != active_ccg)
    
    
    activeCCGInfo <- allCCGs %>% 
      filter(CCGCode == active_ccg) %>% 
      mutate(CCGNameMinusCCG = stringr::str_replace(CCGDescription, " CCG", "")) # %>% 
    # unlist()
    
    
    # Munge data ---------------------------------------------------------------
    setwd(paste0(baseDir, "data"))
    
    comparatorCCGs2 <- allCCGs %>% 
      filter(CCGCode %in% qipp_ccgs)
    
    # Remove rows where the CCGCode is not valid.
    removeInvalidCCGs <- . %>%
      inner_join(comparatorCCGs2, by = "CCGCode")
    
    
    ipSmall <- ipData %>% removeInvalidCCGs
    # rm(ipData) # RAM saver!
    # gc() # call after a large object has been removed
    
    aeSmall <- aeData %>% removeInvalidCCGs
    opSmall <- opData %>% removeInvalidCCGs %>%
      select(-starts_with("FUF"))
    
    opSmallFUF <- opData %>%
      select(
        DSRate, DSRateVar, DSCosts, DSCostsVar, Attendances, Costs, FYear, CCGCode
        , starts_with("FUF")) %>% removeInvalidCCGs 
    
    
    # I'm going to call attendances Spells so that I can reuse functions
    colnames(aeSmall)    <- gsub("Attendances", "Spells", colnames(aeSmall))
    colnames(opSmall)    <- gsub("Attendances", "Spells", colnames(opSmall))
    colnames(opSmallFUF) <- gsub("Attendances", "Spells", colnames(opSmallFUF))
    
    
    # How many rows should there be? 
    ipBase <- expand.grid(
      CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
      , Strategy = activeStrategies %>% filter(TableType == "IP") %>% select(Strategy) %>% unlist %>% unname
      , FYear = unique(ipSmall$FYear)
      , stringsAsFactors = FALSE
    ) %>% 
      filter(!(Strategy %in% c("Readmissions_v1", "Canc_Op_v1")))
    
    opBase <- expand.grid(
      CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
      , Strategy = activeStrategies %>% filter(TableType == "OP" & !grepl("^FUF.*", Strategy)) %>% select(Strategy) %>% unlist %>% unname
      , FYear = unique(opData$FYear)
      , stringsAsFactors = FALSE
    )
    
    aeBase <- expand.grid(
      CCGCode = comparatorCCGs2 %>% select(CCGCode) %>% unlist %>% unname
      , Strategy = activeStrategies %>% filter(TableType == "AE") %>% select(Strategy) %>% unlist %>% unname
      , FYear = unique(aeData$FYear)
      , stringsAsFactors = FALSE
    )
    
    process <- . %>% 
      select(-CCGDescription, -ShortName) %>%
      gather(Strategy, Highlighted, -CCGCode, -Spells, -DSRate
             , -FYear, -Costs, -DSCosts, -DSRateVar, -DSCostsVar, convert = T) %>%
      filter(Highlighted == 1) %>%
      select(-Highlighted) %>%
      group_by(CCGCode, Strategy, FYear) %>%
      summarise(
        Spells = sum(Spells, na.rm = TRUE)
        , DSRate = sum(DSRate, na.rm = TRUE)
        , Costs = sum(Costs, na.rm = TRUE)
        , DSCosts = sum(DSCosts, na.rm = TRUE)
        , DSRateVar = sum(DSRateVar, na.rm = TRUE)
        , DSCostsVar = sum(DSCostsVar, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      mutate(
        DerivedPopulation = (Spells / DSRate) * personYears
        , IsActiveCCG = ifelse(CCGCode == active_ccg, TRUE, FALSE)  
      )
    
    # Process the small data
    ip <- ipSmall %>% process %>% right_join(ipBase, by = c("CCGCode", "Strategy", "FYear"))
    op <- opSmall %>% process %>% right_join(opBase, by = c("CCGCode", "Strategy", "FYear"))
    ae <- aeSmall %>% process %>% right_join(aeBase, by = c("CCGCode", "Strategy", "FYear"))
    
    
    # ***** --------------------------------------------------------------
    
    
    # Funnel ------------------------------------------------------------------
    ipFunnelPoints    <- ip    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
    opFunnelPoints    <- op    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
    aeFunnelPoints    <- ae    %>% filter(FYear == f_year) %>% mutate(DSRate = DSRate/100000) %>% label_ccg()
    
    # Roc ----------------------------------------------------------
    setwd(paste0(baseDir, "r"))
    
    source("rateOfChangePlotFunctions.R")
    
    ipRoCAll <- ip %>% roc_all 
    ipRoCAll <- ipRoCAll %>% left_join(
      ipRoCAll %>% 
        select(CCGCode, Strategy, FYear, Spells) %>%
        rename(SpellsInBaseYear = Spells, From = FYear) 
      , by = c("CCGCode", "Strategy", "From")) %>% 
      unique.data.frame() # to remove duplicates from incl. base yr in roc_all
    
    
    ipRoCActive <- ipRoCAll %>% roc_active
    
    ipRoC <- ipRoCAll %>%
      inner_join(ipRoCActive, by = c("Strategy", "FYear", "From"))
    
    ipRoCCheck <- ipRoC %>% filter(!IsValid)
    ipRoC <- ipRoC %>% filter(IsValid) #N Kirklees and Wakefield
    
    ipRoCSummary <- ipRoC %>% roc_summary
    
    ipRoCFunnels <- roc_funnels(ipRoCSummary, funnelParameters$Smoothness, personYears)
    
    ####
    aeRoCAll <- ae %>% roc_all 
    aeRoCAll <- aeRoCAll %>% left_join(
      aeRoCAll %>% 
        select(CCGCode, Strategy, FYear, Spells) %>%
        rename(SpellsInBaseYear = Spells, From = FYear)
      , by = c("CCGCode", "Strategy", "From"))%>% 
      unique.data.frame() # to remove duplicates from incl. base yr in roc_all
    
    
    aeRoCActive <- aeRoCAll %>% roc_active
    
    aeRoC <- aeRoCAll %>%
      inner_join(aeRoCActive, by = c("Strategy", "FYear", "From"))
    
    aeRoCCheck <- aeRoC %>% filter(!IsValid)
    aeRoC <- aeRoC %>% filter(IsValid) 
    
    aeRoCSummary <- aeRoC %>% roc_summary
    
    aeRoCFunnels <- roc_funnels(aeRoCSummary, funnelParameters$Smoothness, personYears)
    ####
    opRoCAll <- op %>% roc_all 
    opRoCAll <- opRoCAll %>% left_join(
      opRoCAll %>% 
        select(CCGCode, Strategy, FYear, Spells) %>%
        rename(SpellsInBaseYear = Spells, From = FYear)
      , by = c("CCGCode", "Strategy", "From")) %>% 
      unique.data.frame() # to remove duplicates f
    
    opRoCActive <- opRoCAll %>% roc_active
    
    opRoC <- opRoCAll %>%
      inner_join(opRoCActive, by = c("Strategy", "FYear", "From"))
    
    opRoCCheck <- opRoC %>% filter(!IsValid)
    opRoC <- opRoC %>% filter(IsValid) 
    
    opRoCSummary <- opRoC %>% roc_summary
    
    opRoCFunnels <- roc_funnels(opRoCSummary, funnelParameters$Smoothness, personYears)
    
    
    
    # Trend  ---------------------------------------------------------
    ipTrendActive <- ipSmall %>% trend_active 
    aeTrendActive <- aeSmall %>% trend_active 
    opTrendActive <- opSmall %>% trend_active 
    
    ipTrendComparators <- ipSmall %>% trend_comparators 
    aeTrendComparators <- aeSmall %>% trend_comparators 
    opTrendComparators <- opSmall %>% trend_comparators 
    
    # Cost [KEEP]-----------------------------------
    
    ipCost <- ipSmall %>% cost_ds 
    aeCost <- aeSmall %>% cost_ds 
    opCost <- opSmall %>% cost_ds 
  
  # 3. Summaries -------------------------------------------------------
  # source_here("3_summary_master.R")
  
    
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
    
    
  # What is taken from the tables?
  
  strats <- summ_ip_cost_out %>% 
    select(Strategy)
  
  av_save  <- summ_ip_cost_out %>% select(get = Average_SavingsIf_Rounded)
  top_save <- summ_ip_cost_out %>% select(get = TopQuartile_SavingsIf_Rounded)
  
  stp_avg      <- bind_cols(stp_avg, av_save)
  stp_top_qrt  <- bind_cols(stp_top_qrt, top_save)
  
  rm(av_save, top_save, active_ccg)
  # rm(ipData) # RAM saver!
  # gc() # call after a large object has been removed
  
  }





