############################################################################
" RUN QIPP FOR STP SUMMARY TABLES"
###########################################################################

source_here <- function(name){
  source(here::here("r", name))
}

# 1. Load data (minus plot funs) -------------------------------------
source_here("1_init_master.R") # it loads the data (needed only once)


# 1.1. --------------------------------------------------------------------

ccg_stp <- read_rds("C:/2017_projects/qipp/data/ccg_stp.RDS") %>% 
  filter(CCG16CDH %in% qipp_ccgs) 

stp_unique <- unique(ccg_stp$STP17NM)


# Choose STP --------------------------------------------------------------
stp_choice <- stp_unique[2]
#  ------------------------------------------------------------------------

loop_df <- ccg_stp %>% filter(STP17NM == stp_choice)

# empty dfs for loop
stp_avg      <- tibble()
stp_top_qrt  <- tibble()



# 1.2. begin loop ---------------------------------------------------------

for(i in seq_along(loop_df$CCG16CDH)){
  
  active_ccg <- loop_df$CCG16CDH[i]

# 2. wrangle ---------------------------------------------------------
source_here("2_wrang_master.R")


# 3. Summaries -------------------------------------------------------
source_here("3_summary_master.R")

# Extract what is needed from the summaries:

  strats <- summ_ip_cost_out %>% 
    select(Strategy)
  
  av_save  <- summ_ip_cost_out %>% select(col = Average_SavingsIf_Rounded)
  top_save <- summ_ip_cost_out %>% select(col = TopQuartile_SavingsIf_Rounded)
  
  stp_avg      <- if(i == 1){av_save}  else {bind_cols(stp_avg, av_save)}
  stp_top_qrt  <- if(i == 1){top_save} else {bind_cols(stp_top_qrt, top_save)}
  
  rm(av_save, top_save, active_ccg)
 
}

final_av  <- bind_cols(strats, stp_avg %>%
                         # to sum rows must remove the pound and comma
                         mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                         mutate_all(funs(as.numeric)) %>% 
                         mutate(total = rowSums(.)) %>% 
                         mutate_all(funs(pound)) %>%  
                         mutate_all(funs(comma))) %>% 
                         `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                         mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))
                       
                       

                       
final_top <- bind_cols(strats, stp_top_qrt %>%
                         # to sum rows must remove the pound and comma
                         mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                         mutate_all(funs(as.numeric)) %>% 
                         mutate(total = rowSums(.)) %>% 
                         mutate_all(funs(pound)) %>%  
                         mutate_all(funs(comma))) %>% 
  `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
  mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))




flex_stp <- function(df){
  
  table <- setZebraStyle(vanilla.table(df), odd = alpha("goldenrod1", 0.4), even = alpha("goldenrod1", 0.2))
  
  table[,] <- textProperties(font.family = "Segoe UI", font.size = 12)
  table[to = "header"]      <-  textProperties(font.size = 14, font.family = "Segoe UI")
  
  table[, 1]                <- parLeft()
  table[, 1, to = "header"] <- parLeft()
  
  
  table <- setFlexTableBorders(table
                               , inner.vertical = borderProperties( style = "dashed", color = "white" )
                               , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                               , outer.vertical = borderProperties( width = 2, color = "white"  )
                               , outer.horizontal = borderProperties( width = 2, color = "white"  )
  )
  
  table
}

flex_top_q <- flex_stp(final_top)
flex_av <- flex_stp(final_av)
