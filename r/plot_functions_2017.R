

# trend --------------------------------------------------------------


plot_trend <- function(active_df, comparator_df, quote_y, active_y, comparator_y, colour_block, comparator = T){
  
  p <- ggplot()+
    geom_area(data = active_df,
              aes(
                FYearIntToChar(FYear),
                get(quote_y),
                group = 1
              )
              , alpha = 0.2
              , fill = colour_block # "#c52828" # SWB Red: # '#c52828' - original red
    )+
    geom_line(data = active_df,
              aes(
                FYearIntToChar(FYear), 
                get(quote_y),
                group = 1
              ),
              alpha = 0.8
              , colour = colour_block
              , size = 1
    )+
    # Semi transparent points:
    geom_point(data = active_df,
               aes(
                 FYearIntToChar(FYear), 
                 get(quote_y),
                 group = 1
               ),
               alpha = 0.2
               , colour = colour_block
               , size = 3
    )+
    # Highlighted points:
    geom_point(data = active_df %>% 
                 filter(FYear == f_year),
               aes(
                 FYearIntToChar(FYear), 
                 get(quote_y),
                 group = 1
               )
               , colour = colour_block
               , size = 3
    )+
    ylim(0, 1.2*max(active_y, comparator_y))+
    theme_strategy()+
    theme(panel.background = element_rect(fill = "white"),
          # plot.subtitle = element_text(face = "italic"),
          axis.title = element_blank())+
    labs(y = paste0("DSR per ",
                    scales::comma(funnelParameters$RatePerPeople)," population"),
         title = paste0("Trend in Directly Standardised Rate, ",
                        str_sub(rocParameters$From ,1, 4), "-",
                        str_sub(rocParameters$From ,5, 6), " to "
                        , str_sub(rocParameters$To ,1, 4), "-",
                        str_sub(rocParameters$To ,5, 6))
         , subtitle = "DSR per 100k population [Vertical Axis]")+
    scale_x_discrete(expand = c(0.02,0.0))
  
  if(comparator == T){
    
    p + geom_line(data = comparator_df %>% 
                    filter(Type == "Average"),
                  aes(
                    FYearIntToChar(FYear), 
                    get(quote_y),
                    group = 1
                  )
                  # ,linetype = "longdash"
                  , alpha = 0.8
    )+ 
      geom_segment(data = comparator_df,
                   aes(
                     y = 0.1*max(comparator_y), yend = 0.1*max(comparator_y),
                     x = 4.05, xend = 4.25
                   )
      )+
      geom_text(data = comparator_df,
                aes(x = 4.6, y = 0.1*max(comparator_y),
                    label ="W.M. average",
                    family = "Segoe UI",
                    fontface = "plain"),
                size = 2.5)
    
    
  } else {
    p  + geom_line(data = comparator_df,
                   aes(
                     FYearIntToChar(FYear), 
                     get(quote_y),
                     group = 1
                   )
                   # ,linetype = "longdash"
                   , alpha = 0.8
    )
  }
}

# plot_trend(plotTrendActive,
#            plotTrendComparators,
#            "DSRate",
#            plotTrendActive$DSRate,
#            plotTrendComparators$DSRate,
#            ip_colour)


# funnel rate --------------------------------------------------------



plot_fun   <- function(df_funnels, df_units, colour_block){
  
  ggplot(df_funnels) +
    geom_line(aes(x = n, y = fnlLow, group = fnlLimit), linetype = "44") +
    geom_line(aes(x = n, y = fnlHigh, group = fnlLimit), linetype = "44") +
    geom_segment(aes(  x    = min(n)
                       , xend = max(n)
                       , y    = target
                       , yend = target))+
    #geom_hline(aes(yintercept = target)) +
    geom_point(data = df_units, aes(x = DerivedPopulation, y = DSRate, colour = IsActiveCCG), size = 3)+
    geom_text(data = df_funnels
              , aes(x = 0.75*max(n), y = min(fnlLow, df_units$DSRate), label = "Standardised population 2016-17")
              # , vjust = "bottom"
              # , hjust = "right"
              , family = "Segoe UI"
              , size  = 2.5
              , fontface  = "plain"
              , color = "grey30"
    )+
    geom_text_repel(data = df_units
                    , aes(x = DerivedPopulation
                          , y = DSRate
                          , label = ccg_label
                    )
                    , alpha = 0.5
                    
                    #, fontface = 'bold'
                    , size = 3
                    # box.padding = unit(0.25, "lines"),
                    # point.padding = unit(0.5, "lines")
                    # , nudge_y = -0.00050
    ) +
    scale_x_continuous(labels = scales::comma
                       #, limits = c(0, 1000000) # forced x to 1M
    )+
    scale_y_continuous(labels = scales::comma)+
    theme_strategy()+
    theme(legend.position = "none"
          , axis.title = element_blank()
          #, plot.subtitle = element_text(face = "italic")
    )+
    labs(
      x = paste0("Standardised population ", FYearIntToChar(f_year))
      , subtitle = paste0("DSR per 100k population [Vertical Axis]")  # , scales::comma(funnelParameters$RatePerPeople)," population")
      , title = paste0("Directly Standardised Rate, ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", colour_block))
  
}

# x <- plot_fun(plotFunnels, plotUnits)


# roc ----------------------------------------------------------------



plot_roc <- function(funnel_df, points_df, summary_df, colour_block){
  
  ggplot(data = funnel_df) +
    geom_line(aes(x = Denominator, y = ThreeSigmaLower ), colour = "black", linetype =  44) +
    # geom_line(aes(x = Denominator, y = TwoSigmaLower   ), colour = "black" , linetype = 44) +
    # geom_line(aes(x = Denominator, y = TwoSigmaHigher  ), colour = "black" , linetype = 44) +
    geom_line(aes(x = Denominator, y = ThreeSigmaHigher), colour = "black", linetype =  44) +
    geom_segment(aes(x      = min(Denominator)
                     , xend = max(Denominator)
                     , y    = AverageRateOfChange
                     , yend = AverageRateOfChange))+
    geom_point(
      data = points_df
      , aes(x = SpellsInBaseYear,
            y = RateOfChange, colour = IsActiveCCG)
      , size = 3
    )+
    geom_text(data = summary_df
              , aes(x = max(NewMaxSpells), y = min(NewMinRateOfChange), label = "Related activity 2012-13")
              , vjust = "bottom"
              , hjust = "right"
              , family = "Segoe UI"
              , size  = 2.5
              , fontface  = "plain"
              , color = "grey30"
    )+
    geom_text_repel(data = points_df
                    , aes(x = SpellsInBaseYear,
                          y = RateOfChange
                          , label = ccg_label
                    )
                    , alpha = 0.5
                    #, fontface = 'bold'
                    , size = 3
                    # box.padding = unit(0.25, "lines"),
                    # point.padding = unit(0.5, "lines")
                    # , nudge_y = -0.00050
    ) +
    scale_x_continuous(
      labels = scales::comma
      , limits = c(summary_df$NewMinSpells, summary_df$NewMaxSpells)) +
    scale_y_continuous(
      labels = scales::percent
      , limits = c(summary_df$NewMinRateOfChange, summary_df$NewMaxRateOfChange)) +
    labs(
      x = paste0("Related spells "
                 , points_df %>% 
                   filter(IsActiveCCG) %>% 
                   ungroup() %>% 
                   select(From) %>% 
                   unlist %>% unname %>% 
                   FYearIntToChar)
      , y = paste0("Percentage change")
      , title = 
        paste0("Percentage Change in Directly Standardised Rate, "
               , points_df %>% 
                 filter(IsActiveCCG) %>% 
                 ungroup() %>% 
                 select(From) %>% 
                 unlist %>% unname %>% 
                 FYearIntToChar
               , " to "
               , points_df %>% 
                 filter(IsActiveCCG) %>% 
                 ungroup() %>% 
                 select(FYear) %>% 
                 unlist %>% unname %>% 
                 FYearIntToChar)
    ) +
    # scale_y_continuous(limits = c(0.8*min(plotRocPoints$RateOfChange)
    #                               , 1.2*max(plotRocPoints$RateOfChange)
    # )
    # , labels = scales::comma)+
    theme_strategy()+
    theme(legend.position = "none",
          axis.title = element_blank()
          # plot.subtitle = element_text(face = "italic"),
          # panel.background = element_rect(fill = "#E6E6FA")
    )+
    labs(
      # x = paste0("Rate of Change between ", FYearIntToChar(f_year))
      subtitle = "Percentage change in DSR [Vertical Axis]"
      #, title = paste0("Ratio of Follow-ups to First Appointments ", FYearIntToChar(f_year))
    )+
    scale_color_manual(values = c("grey70", colour_block))
  
  #69D2E7,#A7DBD8,#E0E4CC,#F38630,#FA6900,#69D2E7,#A7DBD8,#E0E4CC
  #"grey70", '#c52828'
  #"#69D2E7", '#FA6900'
}

# plot_roc(plotRocFunnels, plotRocPoints, plotRocSummary, "grey70")


# Obselete: ----------------------------------------------------------

# 
# 
# plot_cost  <- function(df){
#   ggplot(df) +
#     geom_bar(aes(x = ShortName, y = DSCostsPerHead, fill = IsActiveCCG), stat = "identity") +
#     geom_text(
#       aes(
#         x = ShortName
#         , y = 1.01 * DSCostsPerHead # for label
#         , family = "Segoe UI Light"
#         , label = stringr::str_c("£", df$for_label)
#         , hjust = 0
#       )
#       , size = 3) +
#     coord_flip() +
#     # scale_fill_manual(values = colourBlindPalette[c("green", "red")] %>% unname) +
#     scale_y_continuous(labels = pound, limits = c(0,101)) +
#     expand_limits(y = c(min(pretty(df$DSCostsPerHead)), max(pretty(df$DSCostsPerHead))*1.05)) +
#     labs(x = NULL, y = NULL, title = "Directly Standardised Costs per head of Population") +
#     theme_strategy()+
#     theme(legend.position = "none")+
#     # theme(panel.grid.major = element_blank())+
#     scale_fill_grey(start = 0.6)
# }
