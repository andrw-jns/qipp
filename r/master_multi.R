############################################################################
" QIPP: RUN "
###########################################################################

source_here <- function(name){
  source(here::here("r", name))
}

# 1. Load data (minus plot funs) -------------------------------------
source_here("1_init_master.R") # it loads the data (needed only once)


# Then do the ccg_stp collection:

stp_avg      <- tibble()
stp_top_qrt  <- tibble()

# Start loop here:

# Select active CCG -----------------------------------------------
active_ccg <- "05L"

# 2. Wrangle ---------------------------------------------------------
source_here("2_wrang_master.R")

detach("package:testthat", unload=TRUE)
# 3. Summaries -------------------------------------------------------
source_here("3_summary_master.R")

# What is taken from the tables?

strats <- summ_ip_cost_out %>% 
  select(Strategy)

av_save  <- summ_ip_cost_out %>% select("ccg" = Average_SavingsIf_Rounded)
top_save <- summ_ip_cost_out %>% select("ccg" = TopQuartile_SavingsIf_Rounded)

stp_avg      <- bind_cols(stp_avg, av_save)
stp_top_qrt  <- bind_cols(stp_top_qrt, top_save)

rm(av_save, top_save)





