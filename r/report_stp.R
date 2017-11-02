# library(ReporteRs)
# library(stringr)


# source_here_extra <- function(name){
#   source(here::here("r", name))
# }
setwd("C:/2017_projects/qipp_extra")

qipp_report_stp <- pptx(title = "qipp_two", template = "su_brand5.pptx")

filename <- "qipp_test_stp_4.pptx" # the document to produce

# 0: TITLE --------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "title" ) %>%
  addTitle(value = "Identifying potential QIPP opportunities") %>%
  addSubtitle(str_c("Summary pack for ", stp_choice, " STP"))


# 3: IP  TITLE ---------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>%
  addTitle("Inpatients")


# IP -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Inpatient Spend by Opportunity [2016-17]") %>%
  addFlexTable(flex_spend_ip)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Average Performers") %>%
  addFlexTable(flex_av_ip)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Top-Quartile Performers") %>%
  addFlexTable(flex_top_ip)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings for Combined Worcester CCGs") %>%
  addPlot(function() plot(plot_saveif_ip))

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Rates vs. Comparator CCGs") %>%
  addFlexTable(flex_rates_ip)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Change in Rate vs. Comparator CCGs") %>%
  addFlexTable(flex_roc_ip)


# 3: ED  TITLE ---------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>%
  addTitle("ED")



# AE -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("ED Spend by Opportunity [2016-17]") %>%
  addFlexTable(flex_spend_ae)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Average Performers") %>%
  addFlexTable(flex_av_ae)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Top-Quartile Performers") %>%
  addFlexTable(flex_top_ae)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings for Combined Worcester CCGs") %>%
  addPlot(function() plot(plot_saveif_ae))

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Rate vs. Comparator CCGs") %>%
  addFlexTable(flex_rates_ae)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Change in Rate vs. Comparator CCGs") %>%
  addFlexTable(flex_roc_ae)



# 3: OP  TITLE ---------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>%
  addTitle("Outpatients")


# OP -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Outpatient Spend by Opportunity [2016-17]") %>%
  addFlexTable(flex_spend_op)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Average Performers*") %>%
  addFlexTable(flex_av_op)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings by Matching Top-Quartile Performers") %>%
  addFlexTable(flex_top_op)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential Savings for Combined Worcester CCGs") %>%
  addPlot(function() plot(plot_saveif_op))

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Rate vs. Comparator CCGs") %>%
  addFlexTable(flex_rates_op)

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Change in Rate vs. Comparator CCGs") %>%
  addFlexTable(flex_roc_op)


#  -------------------------------------------------------------------


# # TEST write qipp_report 
writeDoc(qipp_report_stp, filename)


