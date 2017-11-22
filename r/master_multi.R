############################################################################
" RUN QIPP FOR STP SUMMARY TABLES"
###########################################################################
setwd("C:/2017_projects/qipp")

source_here <- function(name){
  source(here::here("r", name))
}

# 1. Load data (minus plot funs) -------------------------------------
source_here("1_init_master.R") # it loads the data (needed only once)
source_here("theme_strategy.R")


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
    `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "Worcs total")) %>% 
    mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ",""), "Worcs total"), funs(str_replace_all(.,"[:alpha:]", "")))
}



wrangle_saveif <- function(df_av, df_tquart){
  
  bind_cols(df_av %>% select(Opportunity, `Worcs total`),
            df_tquart %>% select(`Worcs total`)) %>%
    rename(opportunity = Opportunity,
           average     = `Worcs total`,
           t_quartile  = `Worcs total1`) %>%
    mutate_at(vars(c("average", "t_quartile")), funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
    mutate_at(vars(c("average", "t_quartile")), funs(as.numeric))
}


plot_saveif <- function(df, opportunity, average, t_quartile, pod_colour){
  
  ggplot(df, aes(reorder(opportunity, average), average))+
    geom_bar(stat = "identity", colour = "black", aes(fill = "myline1"))+
    geom_bar(aes(opportunity, t_quartile, fill = "myline2"), stat = "identity", alpha = 0.4)+
    # geom_bar(stat = "identity", position = "stack") +
    coord_flip()+
    theme_strategy_large()+
    scale_y_continuous(labels = scales::unit_format(unit = "", scale = 1e-3, digits = 1)
                       , position = "top"
                       , expand = c(0,0))+
    # scale_y_continuous(labels = scales::comma_format())
    theme(legend.title = element_blank(),
          axis.title.y = element_blank(),
          legend.position = c(0.82, 0.18),
          legend.background = element_rect(fill = "white"))+
    scale_fill_manual(
      name = "line Colour",
      values = c(myline1 = pod_colour, myline2 = pod_colour),
      labels = c("Savings if average", "Additional savings if top quartile"))+
    # scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9")
    #                   , labels=c("Savings if Top Decile", "Savings if Top Quartile", "Savings if Average"))+
    ylab("Potential savings (£ millions)")+
    guides(fill = guide_legend(override.aes = list(alpha = c(1, 0.2))))+ # the second alpha does not have to relate
    theme(# panel.grid.major = element_blank(),
      panel.background = element_blank())+
    scale_x_discrete(expand = c(0,0))
    
    }


conditional_worcs <- function(df){
  
  rag_colours <- RColorBrewer::brewer.pal(9, "RdYlGn")
  
  data <- flexerize_stp(df)
  
  data[df$`Redditch and Bromsgrove CCG` == "Low", 2] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`Redditch and Bromsgrove CCG` == "Not Significant", 2] = chprop(myCellProps, background.color = alpha(rag_colours[4], 0.2))
  data[df$`Redditch and Bromsgrove CCG` == "High", 2] = chprop(myCellProps, background.color = rag_colours[4])
  
  
  data[df$`South Worcestershire CCG` == "Low", 3] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`South Worcestershire CCG` == "Not Significant", 3] = chprop(myCellProps, background.color = alpha(rag_colours[4], 0.2))
  data[df$`South Worcestershire CCG` == "High", 3] = chprop(myCellProps, background.color = rag_colours[4])
  
  
  data[df$`Wyre Forest CCG` == "Low", 4] = chprop(myCellProps, background.color = rag_colours[6])
  data[df$`Wyre Forest CCG` == "Not Significant", 4] = chprop(myCellProps, background.color = alpha(rag_colours[4], 0.2))
  data[df$`Wyre Forest CCG` == "High", 4] = chprop(myCellProps, background.color = rag_colours[4])
  
  
  
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


# *** ----------------------------------------------------------------


# 1.2. BEGIN loop ---------------------------------------------------------

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
  
  spend_ip  <- summ_ip_cost_out %>% select(col = `2016-17 Spend (000s)`)
  spend_ae  <- summ_ae_cost_out %>% select(col = `2016-17 Spend (000s)`)
  spend_op  <- summ_op_cost_out %>% select(col = `2016-17 Spend (000s)`)
  
  av_save_ip  <- summ_ip_cost_out %>% select(col = `Savings if Average (000s)`)
  av_save_ae  <- summ_ae_cost_out %>% select(col = `Savings if Average (000s)`)
  av_save_op  <- summ_op_cost_out %>% select(col = `Savings if Average (000s)`)
  
  
  top_save_ip <- summ_ip_cost_out %>% select(col = `Savings if Top Quartile (000s)`)
  top_save_ae <- summ_ae_cost_out %>% select(col = `Savings if Top Quartile (000s)`)
  top_save_op <- summ_op_cost_out %>% select(col = `Savings if Top Quartile (000s)`)
  
  
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


# *** ----------------------------------------------------------------

flexerize_stp <- function(df){
  
  table <- vanilla.table(df)
  
  table[,] <- textProperties(font.family = "Segoe UI", font.size = 10)
  table[to = "header"]      <-  textProperties(font.size = 10, font.family = "Segoe UI", font.weight = "bold")
  
  table[, 1]                <- parLeft()
  table[, 1, to = "header"] <- parLeft()
  
  # data[, 4:5]                <- parLeft()
  # data[, 4:5, to = "header"] <- parLeft()
  
  
  table <- setFlexTableBorders(table
                               , inner.vertical = borderProperties( style = "dashed", color = "white" )
                               , inner.horizontal = borderProperties( style = "solid", color = "grey80"  )
                               , outer.vertical = borderProperties( width = 2, color = "white"  )
                               , outer.horizontal = borderProperties( width = 1, color = "grey30"  )
  )
  
  table
}


# STP: Spend ---------------------------------------------------------

final_spend_ip  <- format_savings(strats_ip, stp_spend_ip)
final_spend_ae  <- format_savings(strats_ae, stp_spend_ae)
final_spend_op  <- format_savings(strats_op, stp_spend_op)

# flextables:

flex_spend_ip <- flexerize_stp(final_spend_ip)
flex_spend_ae <- flexerize_stp(final_spend_ae)
flex_spend_op <- flexerize_stp(final_spend_op)


# STP: Savings Av ----------------------------------------------------

final_av_ip  <- format_savings(strats_ip, stp_avg_ip)
final_av_ae  <- format_savings(strats_ae, stp_avg_ae)
final_av_op  <- format_savings(strats_op, stp_avg_op)

# flextables:

flex_av_ip    <- flexerize_stp(final_av_ip)
flex_av_ae    <- flexerize_stp(final_av_ae)
flex_av_op    <- flexerize_stp(final_av_op)


# STP: Savings TQ ----------------------------------------------------

final_top_ip <- format_savings(strats_ip, stp_top_qrt_ip)
final_top_ae <- format_savings(strats_ae, stp_top_qrt_ae)
final_top_op <- format_savings(strats_op, stp_top_qrt_op)

# flextables:

flex_top_ip   <- flexerize_stp(final_top_ip)
flex_top_ae   <- flexerize_stp(final_top_ae)
flex_top_op   <- flexerize_stp(final_top_op)



# STP: Potential savings plots ---------------------------------------

final_saveif_ip <- wrangle_saveif(final_av_ip, final_top_ip)
final_saveif_ae <- wrangle_saveif(final_av_ae, final_top_ae)
final_saveif_op <- wrangle_saveif(final_av_op, final_top_op)

plot_saveif_ip <- plot_saveif(final_saveif_ip, opportunity, average, t_quartile, ip_colour)
plot_saveif_ae <- plot_saveif(final_saveif_ae, opportunity, average, t_quartile, ae_colour)
plot_saveif_op <- plot_saveif(final_saveif_op, opportunity, average, t_quartile, op_colour)


# *** ----------------------------------------------------------------


# RAG Tables ---------------------------------------------------------

# Rate comparison 

final_rates_ip <- format_rates_n_roc(strats_ip, stp_rates_ip)
final_rates_ae <- format_rates_n_roc(strats_ae, stp_rates_ae)
final_rates_op <- format_rates_n_roc(strats_op, stp_rates_op)

# summ_op_summ_out[4:5][summ_op_summ_out[4:5] == "Not Significant"] <- "-"

# STP: Roc comparison 

final_roc_ip <- format_rates_n_roc(strats_ip, stp_roc_ip)
final_roc_ae <- format_rates_n_roc(strats_ae, stp_roc_ae)
final_roc_op <- format_rates_n_roc(strats_op, stp_roc_op)


# flextables:

myCellProps <- cellProperties()

flex_rates_ip <- conditional_worcs(final_rates_ip)
flex_rates_ae <- conditional_worcs(final_rates_ae)
flex_rates_op <- conditional_worcs(final_rates_op)

flex_roc_ip <- conditional_worcs(final_roc_ip)
flex_roc_ae <- conditional_worcs(final_roc_ae)
flex_roc_op <- conditional_worcs(final_roc_op)

# *** ----------------------------------------------------------------


#  Treemap----------------------------------------------------------

tmp_tree <- stp_spend_ip %>% 
  select(Opportunity, `Worcs total`) %>% 
  mutate(cost = as.numeric(str_replace_all(`Worcs total`, c("£|,"), ""))) 

tree_wrangle <- bind_cols(strats_ip,
                stp_spend_ip %>%
                  # to sum rows must remove the pound and comma
                  mutate_all(funs(str_replace_all(.,"[:alpha:]|,|£", ""))) %>%
                  mutate_all(funs(as.numeric)) %>% 
                  mutate(total = rowSums(.))) %>% 
                  # mutate_all(funs(pound)) %>%  
                  # mutate_all(funs(comma))) %>% 
        `colnames<-`(c("Opportunity", str_replace_all(loop_df$CCG16NM, "NHS ",""), "Worcs total")) %>% 
        mutate_at(vars(str_replace_all(loop_df$CCG16NM, "NHS ","")), funs(str_replace_all(.,"[:alpha:]", ""))) 





library(treemap)
treemap(tree_wrangle, #Your data frame object
        index = c("Opportunity"),  #A list of your categorical variables
        vSize = "Worcs total",  #This is your quantitative variable
        type  = "index", #Type sets the organization and color scheme of your treemap
        palette = "Set3",  #Select your color palette from the RColorBrewer presets or make your own.
        title = "", #Customize your title
        fontsize.title = 15 #Change the font size of the title
)

setwd("C:/2017_projects/qipp_extra")
savePlot(filename = "Rplot", type = "png", device = "png")

png('treemap_worcs.png', width = 29.7, height = 21.0, units = "cm", res = 72)
dev.off()
