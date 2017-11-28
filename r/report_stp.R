# library(ReporteRs)
# library(stringr)


# source_here_extra <- function(name){
#   source(here::here("r", name))
# }
# setwd("C:/2017_projects/qipp_extra")

qipp_report_stp <- pptx(title = "qipp_two", template = "C:/2017_projects/qipp/reporting/su_brand5.pptx")

filename <- "stp_reporters_out.pptx" # the document to produce

# 0: TITLE --------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "title" ) %>%
  addTitle(value = "Identifying Opportunities to Reduce Acute Hospital Activity") %>%
  addSubtitle(str_c("Summary Report for ", stp_choice, " STP", "                                                         November 2017"))


# 3: IP  TITLE ---------------------------------------------------------

# qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
#   addImage("qipp_photo_inpatient.png") %>%
#   addTitle("Inpatients")


# IP -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Inpatient Spend (000s) by Opportunity, 2016-17") %>%
  addFlexTable(flex_spend_ip)


qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Average") %>%
  addFlexTable(flex_av_ip) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Top Quartile") %>%
  addFlexTable(flex_top_ip) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle(str_c("Potential Savings for ", stp_choice," STP" )) %>%
  addPlot(function() plot(plot_saveif_ip)) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Utilisation Rates Compared With Other West Midlands CCGs") %>%
  addFlexTable(flex_rates_ip )%>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Change in Utilisation Rates (2012-13 to 2016-17)") %>%
  addFlexTable(flex_roc_ip) %>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")


# 3: ED  TITLE ---------------------------------------------------------
# 
# qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
#   addImage("qipp_photo_inpatient.png") %>%
#   addTitle("ED")



# AE -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Emergency Department Spend (000s) by Opportunity, 2016-17") %>%
  addFlexTable(flex_spend_ae)


qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Average") %>%
  addFlexTable(flex_av_ae) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")


qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Top Quartile ") %>%
  addFlexTable(flex_top_ae) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle(str_c("Potential Savings for ", stp_choice," STP" )) %>%
  addPlot(function() plot(plot_saveif_ae)) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Utilisation Rates Compared With Other West Midlands CCGs") %>%
  addFlexTable(flex_rates_ae) %>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Change in Utilisation Rates (2012-13 to 2016-17)") %>%
  addFlexTable(flex_roc_ae) %>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")



# 3: OP  TITLE ---------------------------------------------------------
# 
# qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
#   addImage("qipp_photo_inpatient.png") %>%
#   addTitle("Outpatients")


# OP -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Outpatient Spend (000s) by Opportunity, 2016-17") %>%
  addFlexTable(flex_spend_op)


qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Average") %>%
  addFlexTable(flex_av_op) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle("Potential Savings (000s) by Matching Top Quartile") %>%
  addFlexTable(flex_top_op) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer_2") %>%
  addTitle(str_c("Potential Savings for ", stp_choice," STP" )) %>%
  addPlot(function() plot(plot_saveif_op)) %>% 
  addFooter("Notes: Savings estimates are the total savings achievable if activity for a particular sub-group was reduced from its current level to the average or top quartile of other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Utilisation Rates Compared With Other West Midlands CCGs") %>%
  addFlexTable(flex_rates_op) %>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")

qipp_report_stp <- addSlide(qipp_report_stp, "content_footer") %>%
  addTitle("Change in Utilisation Rates (2012-13 to 2016-17)") %>%
  addFlexTable(flex_roc_op) %>% 
  addFooter("Notes: Rate and rate of change comparisons are with other West Midlands CCGs.")


#  -------------------------------------------------------------------


# # TEST write qipp_report 
writeDoc(qipp_report_stp, paste0(getwd(), "/reporting/", filename))



