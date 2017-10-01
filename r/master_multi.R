############################################################################
" QIPP: RUN "
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

loop_df <- ccg_stp %>% filter(STP17NM == stp_unique[1])

# empty dfs for loop
stp_avg      <- tibble()
stp_top_qrt  <- tibble()



# 1.2. begin loop ---------------------------------------------------------


for(i in seq_along(loop_df$CCG16CDH)){
  
  active_ccg <- loop_df$CCG16CDH[i]

# 2. Wrangle ---------------------------------------------------------
source_here("2_wrang_master.R")

# detach("package:testthat", unload=TRUE)
# 3. Summaries -------------------------------------------------------
source_here("3_summary_master.R")

# What is taken from the tables?

  strats <- summ_ip_cost_out %>% 
    select(Strategy)
  
  av_save  <- summ_ip_cost_out %>% select(col = Average_SavingsIf_Rounded)
  top_save <- summ_ip_cost_out %>% select(col = TopQuartile_SavingsIf_Rounded)
  
  stp_avg      <- if(i == 1){av_save}  else {bind_cols(stp_avg, av_save)}
  stp_top_qrt  <- if(i == 1){top_save} else {bind_cols(stp_top_qrt, top_save)}
  
  rm(av_save, top_save, active_ccg)
  # rm(ipData) # RAM saver!
  # gc() # call after a large object has been removed
  
}






