library(ReporteRs)
library(stringr)

setwd("C:/2017_projects/qipp_extra")

qipp_report_stp <- pptx(title = "qipp_two", template = "su_brand2.pptx")


# 0: TITLE --------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "title" ) %>%
  addTitle(value = "Identifying potential QIPP opportunities") %>%
  addSubtitle(str_c("Summary pack for ", stp_choice, " STP"))


# TABLES ------------------------------------------------------------------


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Flextable: Proof of Concept") %>%
  addFlexTable(flex_av)


qipp_report_stp <- addSlide(qipp_report_stp, "contentA") %>%
  addTitle("Flextable: Proof of Concept") %>%
  addFlexTable(flex_top_q)


filename <- "qipp_test_stp.pptx" # the document to produce
# # TEST write qipp_report 
writeDoc(qipp_report_stp, filename)
