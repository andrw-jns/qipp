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
stp_choice <- stp_unique[4]
#  ------------------------------------------------------------------------

loop_df <- ccg_stp %>% filter(STP17NM   == stp_choice,
                              !CCG16CDH == "05F") # Remove Hereford.

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

  strats_ip <- summ_ip_cost_out %>% 
    select(Opportunity)
  
  strats_ae <- summ_ae_cost_out %>% 
    select(Opportunity)
  
  strats_op <- summ_op_cost_out %>% 
    select(Opportunity)
  
  
  av_save_ip  <- summ_ip_cost_out %>% select(col = `Total Savings if Average`)
  av_save_ae  <- summ_ae_cost_out %>% select(col = `Total Savings if Average`)
  av_save_op  <- summ_op_cost_out %>% select(col = `Total Savings if Average`)
  
  
  top_save_ip <- summ_ip_cost_out %>% select(col = `Total Savings if Top Quartile`)
  top_save_ae <- summ_ae_cost_out %>% select(col = `Total Savings if Top Quartile`)
  top_save_op <- summ_op_cost_out %>% select(col = `Total Savings if Top Quartile`)
  
  
  stp_avg_ip    <- if(i == 1){av_save_ip}  else {bind_cols(stp_avg_ip, av_save_ip)}
  stp_avg_ae    <- if(i == 1){av_save_ae}  else {bind_cols(stp_avg_ae, av_save_ae)}
  stp_avg_op    <- if(i == 1){av_save_op}  else {bind_cols(stp_avg_op, av_save_op)}
  
  
  stp_top_qrt_ip  <- if(i == 1){top_save_ip} else {bind_cols(stp_top_qrt_ip, top_save_ip)}
  stp_top_qrt_ae  <- if(i == 1){top_save_ae} else {bind_cols(stp_top_qrt_ae, top_save_ae)}
  stp_top_qrt_op  <- if(i == 1){top_save_op} else {bind_cols(stp_top_qrt_op, top_save_op)}
  
  rm(av_save_ip, av_save_ae, av_save_op,
     top_save_ip, top_save_ae, top_save_op,
     active_ccg)
 
}


# STP: Savings Av ----------------------------------------------------

final_av_ip  <- bind_cols(strats_ip, stp_avg_ip %>%
                         # to sum rows must remove the pound and comma
                         mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                         mutate_all(funs(as.numeric)) %>% 
                         mutate(total = rowSums(.)) %>% 
                         mutate_all(funs(pound)) %>%  
                         mutate_all(funs(comma))) %>% 
                         `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                         mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))
                       

final_av_ae  <- bind_cols(strats_ae, stp_avg_ae %>%
                            # to sum rows must remove the pound and comma
                            mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                            mutate_all(funs(as.numeric)) %>% 
                            mutate(total = rowSums(.)) %>% 
                            mutate_all(funs(pound)) %>%  
                            mutate_all(funs(comma))) %>%
                            `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                            mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))




final_av_op  <- bind_cols(strats_op, stp_avg_op %>%
                            # to sum rows must remove the pound and comma
                            mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                            mutate_all(funs(as.numeric)) %>% 
                            mutate(total = rowSums(.)) %>% 
                            mutate_all(funs(pound)) %>%  
                            mutate_all(funs(comma))) %>%
                            `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                            mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))



# STP: Savings TQ ----------------------------------------------------


final_top_ip <- bind_cols(strats_ip, stp_top_qrt_ip %>%
                         # to sum rows must remove the pound and comma
                         mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                         mutate_all(funs(as.numeric)) %>% 
                         mutate(total = rowSums(.)) %>% 
                         mutate_all(funs(pound)) %>%  
                         mutate_all(funs(comma))) %>% 
                         `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                         mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))


final_top_ae <- bind_cols(strats_ae, stp_top_qrt_ae %>%
                            # to sum rows must remove the pound and comma
                            mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                            mutate_all(funs(as.numeric)) %>% 
                            mutate(total = rowSums(.)) %>% 
                            mutate_all(funs(pound)) %>%  
                            mutate_all(funs(comma))) %>% 
                            `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                            mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))



final_top_op <- bind_cols(strats_op, stp_top_qrt_op %>%
                            # to sum rows must remove the pound and comma
                            mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                            mutate_all(funs(as.numeric)) %>% 
                            mutate(total = rowSums(.)) %>% 
                            mutate_all(funs(pound)) %>%  
                            mutate_all(funs(comma))) %>% 
                            `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total")) %>% 
                            mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "STP total"), funs(str_replace_all(.,"[:alpha:]", "")))


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


# IP Tables --------------------------------------------------

flex_av_ip    <- flexerize_gold(final_av_ip)
flex_top_ip   <- flexerize_gold(final_top_ip)

# AE Tables ----------------------------------------------------------

flex_av_ae    <- flexerize_gold(final_av_ae)
flex_top_ae   <- flexerize_gold(final_top_ae)


# OP Tables ----------------------------------------------------------

flex_av_op    <- flexerize_gold(final_av_op)
flex_top_op   <- flexerize_gold(final_top_op)



#  Treemap----------------------------------------------------------

tmp_tree <- final_top_ip %>% 
  select(Opportunity, `STP total`) %>% 
  mutate(cost = as.numeric(str_replace_all(`STP total`, c("£|,"), ""))) 


library(treemap)
treemap(tmp_tree, #Your data frame object
        index=c("Opportunity"),  #A list of your categorical variables
        vSize = "cost",  #This is your quantitative variable
        type="index", #Type sets the organization and color scheme of your treemap
        palette = "Reds",  #Select your color palette from the RColorBrewer presets or make your own.
        title="Worcester CCGs Total Spend", #Customize your title
        fontsize.title = 12 #Change the font size of the title
)

getwd()
savePlot(filename = "Rplot", type = "png", device = "png")

png('rplot.png', width = 1024, height = 768)

dev.off()
