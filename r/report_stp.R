library(ReporteRs)
library(stringr)

setwd("C:/2017_projects/qipp_extra")

qipp_report_stp <- pptx(title = "qipp_two", template = "su_brand2.pptx")


# 0: TITLE --------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "title" ) %>%
  addTitle(value = "Identifying potential QIPP opportunities") %>%
  addSubtitle(str_c("Summary pack for ", stp_choice, " STP"))


# TABLES ------------------------------------------------------------------


# IP -----------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching average performers*") %>%
  addFlexTable(flex_av_ip)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching top quartile performers*") %>%
  addFlexTable(flex_top_ip)


# AE -----------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching average performers*") %>%
  addFlexTable(flex_av_ae)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching top quartile performers*") %>%
  addFlexTable(flex_top_ae)



# OP -----------------------------------------------------------------

qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching average performers*") %>%
  addFlexTable(flex_av_op)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Potential savings by matching top quartile performers*") %>%
  addFlexTable(flex_top_op)


#  -------------------------------------------------------------------



filename <- "qipp_test_stp_2.pptx" # the document to produce
# # TEST write qipp_report 
writeDoc(qipp_report_stp, filename)


