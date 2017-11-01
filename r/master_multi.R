############################################################################
" RUN QIPP FOR STP SUMMARY TABLES"
###########################################################################

source_here <- function(name){
  source(here::here("r", name))
}

# 1. Load data (minus plot funs) -------------------------------------
source_here("1_init_master.R") # it loads the data (needed only once)


# 1.01 Functions ----------------------------------------------

select_opportunity_col <- function(df){
  data <- df %>% 
    select(Opportunity)
}


format_rates_n_roc <- function(strats_pod, pod_df){
  
  data <- bind_cols(strats_pod, pod_df) %>%
    `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ","")))
}


format_savings <- function(strats_pod, pod_df){
  
  data <- bind_cols(strats_pod, pod_df %>%
                      # to sum rows must remove the pound and comma
                      mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                      mutate_all(funs(as.numeric)) %>% 
                      mutate(total = rowSums(.)) %>% 
                      mutate_all(funs(pound)) %>%  
                      mutate_all(funs(comma))) %>% 
    `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
    mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))
}


conditional_worcs <- function(df){
  
  rag_colours <- RColorBrewer::brewer.pal(9, "RdYlGn")
  
  data <- flexerize_gold(df)
  
  data[df$`Redditch and Bromsgrove CCG` == "Low", 2] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`Redditch and Bromsgrove CCG` == "Not Significant", 2] = chprop(myCellProps, background.color = rag_colours[4])
  data[df$`Redditch and Bromsgrove CCG` == "High", 2] = chprop(myCellProps, background.color = rag_colours[3])
  
  
  data[df$`South Worcestershire CCG` == "Low", 3] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`South Worcestershire CCG` == "Not Significant", 3] = chprop(myCellProps, background.color = rag_colours[4])
  data[df$`South Worcestershire CCG` == "High", 3] = chprop(myCellProps, background.color = rag_colours[3])
  
  
  data[df$`Wyre Forest CCG` == "Low", 4] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`Wyre Forest CCG` == "Not Significant", 4] = chprop(myCellProps, background.color = rag_colours[4])
  data[df$`Wyre Forest CCG` == "High", 4] = chprop(myCellProps, background.color = rag_colours[3])
  
  
  
  setFlexTableBorders(data
                      , inner.vertical = borderProperties( style = "dashed", color = "white" )
                      , inner.horizontal = borderProperties( style = "dashed", color = "white"  )
                      , outer.vertical = borderProperties( width = 2, color = "white"  )
                      , outer.horizontal = borderProperties( width = 2, color = "white"  )
  )
  
  
}


# 1.1. --------------------------------------------------------------------

ccg_stp <- read_rds("C:/2017_projects/qipp/data/ccg_stp.RDS") %>% 
  filter(CCG16CDH %in% qipp_ccgs) 

stp_unique <- unique(ccg_stp$STP17NM)


# Choose STP --------------------------------------------------------------
stp_choice <- stp_unique[4]
#  ------------------------------------------------------------------------

loop_df <- ccg_stp %>% filter(STP17NM   == stp_choice,
                              !CCG16CDH == "05F") # Remove Hereford.


# 1.2. begin loop ---------------------------------------------------------

for(i in seq_along(loop_df$CCG16CDH)){
  
  active_ccg <- loop_df$CCG16CDH[i]

# 2. wrangle ---------------------------------------------------------
source_here("2_wrang_master.R")


# 3. Summaries -------------------------------------------------------
source_here("3_summary_master.R")

# Extract what is needed from the summaries:


  strats_ip <- select_opportunity_col(summ_ip_cost_out)
  strats_ae <- select_opportunity_col(summ_ae_cost_out)
  strats_op <- select_opportunity_col(summ_op_cost_out)
  
  # tmp dfs (for iteration) --------------------------------------------
  
  spend_ip  <- summ_ip_cost_out %>% select(col = `Spend 2016-17`)
  spend_ae  <- summ_ae_cost_out %>% select(col = `Spend 2016-17`)
  spend_op  <- summ_op_cost_out %>% select(col = `Spend 2016-17`)
  
  av_save_ip  <- summ_ip_cost_out %>% select(col = `Total Savings if Average`)
  av_save_ae  <- summ_ae_cost_out %>% select(col = `Total Savings if Average`)
  av_save_op  <- summ_op_cost_out %>% select(col = `Total Savings if Average`)
  
  
  top_save_ip <- summ_ip_cost_out %>% select(col = `Total Savings if Top Quartile`)
  top_save_ae <- summ_ae_cost_out %>% select(col = `Total Savings if Top Quartile`)
  top_save_op <- summ_op_cost_out %>% select(col = `Total Savings if Top Quartile`)
  
  
  rate_comparison_ip <- summ_ip_summ_out  %>% select(col = Rate)
  rate_comparison_ae <- summ_ae_summ_out  %>% select(col = Rate)
  rate_comparison_op <- summ_op_summ_out  %>% select(col = Rate)
  
  roc_comparison_ip <- summ_ip_summ_out  %>% select(col = `Rate of Change`)
  roc_comparison_ae <- summ_ae_summ_out  %>% select(col = `Rate of Change`)
  roc_comparison_op <- summ_op_summ_out  %>% select(col = `Rate of Change`)
  
  
  # Spend Comparisons --------------------------------------------------
  
  stp_spend_ip    <- if(i == 1){spend_ip}  else {bind_cols(stp_spend_ip, spend_ip)}
  stp_spend_ae    <- if(i == 1){spend_ae}  else {bind_cols(stp_spend_ae, spend_ae)}
  stp_spend_op    <- if(i == 1){spend_op}  else {bind_cols(stp_spend_op, spend_op)}
  
  
  #  Cost comparisons -----------------------------------------------------------
  
  stp_avg_ip    <- if(i == 1){av_save_ip}  else {bind_cols(stp_avg_ip, av_save_ip)}
  stp_avg_ae    <- if(i == 1){av_save_ae}  else {bind_cols(stp_avg_ae, av_save_ae)}
  stp_avg_op    <- if(i == 1){av_save_op}  else {bind_cols(stp_avg_op, av_save_op)}
  
  
  stp_top_qrt_ip  <- if(i == 1){top_save_ip} else {bind_cols(stp_top_qrt_ip, top_save_ip)}
  stp_top_qrt_ae  <- if(i == 1){top_save_ae} else {bind_cols(stp_top_qrt_ae, top_save_ae)}
  stp_top_qrt_op  <- if(i == 1){top_save_op} else {bind_cols(stp_top_qrt_op, top_save_op)}
  

  # Rates and roc comparisons ------------------------------------------

  stp_rates_ip <- if(i == 1){rate_comparison_ip}  else {bind_cols(stp_rates_ip, rate_comparison_ip)}
  stp_rates_ae <- if(i == 1){rate_comparison_ae}  else {bind_cols(stp_rates_ae, rate_comparison_ae)}
  stp_rates_op <- if(i == 1){rate_comparison_op}  else {bind_cols(stp_rates_op, rate_comparison_op)}
  
  stp_roc_ip <- if(i == 1){roc_comparison_ip}  else {bind_cols(stp_roc_ip, roc_comparison_ip)}
  stp_roc_ae <- if(i == 1){roc_comparison_ae}  else {bind_cols(stp_roc_ae, roc_comparison_ae)}
  stp_roc_op <- if(i == 1){roc_comparison_op}  else {bind_cols(stp_roc_op, roc_comparison_op)}

  
  # rm objects for next iteration --------------------------------------
  
  rm(av_save_ip, av_save_ae, av_save_op,
     top_save_ip, top_save_ae, top_save_op,
     rate_comparison_ip, rate_comparison_ae, rate_comparison_op,
     roc_comparison_ip, roc_comparison_ae, roc_comparison_op,
     spend_ip, spend_ae, spend_op,
     active_ccg
     )
 
}


# STP: Spend ---------------------------------------------------------

final_spend_ip  <- format_savings(strats_ip, stp_spend_ip)
final_spend_ae  <- format_savings(strats_ae, stp_spend_ae)
final_spend_op  <- format_savings(strats_op, stp_spend_op)


# STP: Savings Av ----------------------------------------------------

final_av_ip  <- format_savings(strats_ip, stp_avg_ip)
final_av_ae  <- format_savings(strats_ae, stp_avg_ae)
final_av_op  <- format_savings(strats_op, stp_avg_op)

# STP: Savings TQ ----------------------------------------------------

final_top_ip <- format_savings(strats_ip, stp_top_qrt_ip)
final_top_ae <- format_savings(strats_ae, stp_top_qrt_ae)
final_top_op <- format_savings(strats_op, stp_top_qrt_op)


# STP: Rate comparison ------------------------------------------------

final_rates_ip <- format_rates_n_roc(strats_ip, stp_rates_ip)
final_rates_ae <- format_rates_n_roc(strats_ae, stp_rates_ae)
final_rates_op <- format_rates_n_roc(strats_op, stp_rates_op)


# STP: Roc comparison -------------------------------------------------

final_roc_ip <- format_rates_n_roc(strats_ip, stp_roc_ip)
final_roc_ae <- format_rates_n_roc(strats_ae, stp_roc_ae)
final_roc_op <- format_rates_n_roc(strats_op, stp_roc_op)


# FLEXTABLES-------------------------------------------------

flexerize_gold <- function(df){
  
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


# Spend --------------------------------------------------------------

flex_spend_ip <- flexerize_gold(final_spend_ip)
flex_spend_ae <- flexerize_gold(final_spend_ae)
flex_spend_op <- flexerize_gold(final_spend_op)

# IP Tables --------------------------------------------------

flex_av_ip    <- flexerize_gold(final_av_ip)
flex_top_ip   <- flexerize_gold(final_top_ip)

# AE Tables ----------------------------------------------------------

flex_av_ae    <- flexerize_gold(final_av_ae)
flex_top_ae   <- flexerize_gold(final_top_ae)


# OP Tables ----------------------------------------------------------

flex_av_op    <- flexerize_gold(final_av_op)
flex_top_op   <- flexerize_gold(final_top_op)



# RAG Tables ---------------------------------------------------------

myCellProps <- cellProperties()

flex_rates_ip <- conditional_worcs(final_rates_ip)
flex_rates_ae <- conditional_worcs(final_rates_ae)
flex_rates_op <- conditional_worcs(final_rates_op)

flex_roc_ip <- conditional_worcs(final_roc_ip)
flex_roc_ae <- conditional_worcs(final_roc_ae)
flex_roc_op <- conditional_worcs(final_roc_op)


#  Treemap----------------------------------------------------------

tmp_tree <- stp_spend_ip %>% 
  select(Opportunity, `STP total`) %>% 
  mutate(cost = as.numeric(str_replace_all(`STP total`, c("£|,"), ""))) 

tree_wrangle <- bind_cols(strats_ip,
                stp_spend_ip %>%
                  # to sum rows must remove the pound and comma
                  mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                  mutate_all(funs(as.numeric)) %>% 
                  mutate(total = rowSums(.))) %>% 
                  # mutate_all(funs(pound)) %>%  
                  # mutate_all(funs(comma))) %>% 
        `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
        mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ","")), funs(str_replace_all(.,"[:alpha:]", ""))) 





library(treemap)
treemap(tree_wrangle, #Your data frame object
        index = c("Opportunity"),  #A list of your categorical variables
        vSize = "STP total",  #This is your quantitative variable
        type  = "index", #Type sets the organization and color scheme of your treemap
        palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
        title = "", #Customize your title
        fontsize.title = 15 #Change the font size of the title
)

setwd("C:/2017_projects/qipp_extra")
savePlot(filename = "Rplot", type = "png", device = "png")

png('treemap_worcs.png', width = 29.7, height = 21.0, units = "cm", res = 72)
dev.off()
