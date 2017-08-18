# 8888888b.  8888888888 8888888b.   .d88888b.  8888888b. 88888888888 
# 888   Y88b 888        888   Y88b d88P" "Y88b 888   Y88b    888     
# 888    888 888        888    888 888     888 888    888    888     
# 888   d88P 8888888    888   d88P 888     888 888   d88P    888     
# 8888888P"  888        8888888P"  888     888 8888888P"     888     
# 888 T88b   888        888        888     888 888 T88b      888     
# 888  T88b  888        888        Y88b. .d88P 888  T88b     888     
# 888   T88b 8888888888 888         "Y88888P"  888   T88b    888  



# notes --------------------------------------------------------------

# MAY OCCASIONALLY BE USEFUL TO SOFT WRAP TEXT IN THIS SCRIPT: 
# TOOLS -> GLOBAL OPTIONS -> CODE 

# EXTRA NOTES:
# http://www.sthda.com/english/wiki/create-and-format-word-documents-using-r-software-and-reporters-package#add-plots-and-images

# setup --------------------------------------------------------------
# install.packages("ReporteRs")
# library(tidyverse)
library(ReporteRs)
library(stringr)

setwd("//clients.its.local/csu/users01/andrew.jones/Desktop")

qipp_report <- pptx(title = "qipp_one", template = "su_brand3.pptx")

# slide.layouts(qipp_report)
# slide.layouts(qipp_report, "qipp_body")

# 0: TITLE --------------------------------------------------------------


qipp_report <- addSlide(qipp_report, "title" ) %>% 
  addTitle(value = "Indentifying potential QIPP opportunities") %>% 
  addSubtitle(str_c("Prepared for ", activeCCGInfo$CCGNameMinusCCG, " Clinical Commissioning Group"))

# 1: ABOUT -----------------------------------------------------

qipp_report <- addSlide(qipp_report, "contentA") %>%
  addTitle("About the Strategy Unit") %>% 
  addParagraph("The Strategy Unit is a team of experts who are committed to helping you to improve health and care in ever more challenging circumstances. Hosted by the Midlands and Lancashire Commissioning Support Unit, we operate autonomously as a free-standing health and care consultancy business.\n\nOur team offers advanced technical skills combined with practically grounded strategic and operational experience. We specialise in analysis; evidence review; strategic financial planning; policy and strategy development; consensus building; programme design, assurance and implementation; capacity building; evaluation; and trusted advisor support for senior leaders.\n\nWe welcome the opportunity to discuss your needs and challenges at any time. If we think we can help you, we will gladly develop a detailed proposal. If we think we can't, then we will tell you, and explain why, helping you to find an alternative if needed.")
  # adding a new paragraph here creates block 2

# 2: CONTENTS --------------------------------------------------

qipp_report <- addSlide(qipp_report, "table_of_contents") %>% 
  addTitle("Contents") %>% 
  addParagraph("Purpose of this report\nSubsets of Activity\nData Sources\nInterpreting Funnel Plots") %>% 
  # adding a new paragraph here creates block 2
  addParagraph("1\n7\n11\n24", par.properties = parProperties(text.align = "right")) %>% 
  addParagraph("Purpose of this report\nSubsets of Activity\nData Sources\nInterpreting Funnel Plots") %>% 
  addParagraph("1\n7\n11\n24")

# 3: TITLE IP ---------------------------------------------------------

qipp_report <- addSlide(qipp_report, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>% 
  addTitle("Inpatients") 

# 3: BODY IP ---------------------------------------------------------

for(i in seq(ipPlottableStrategies$Strategy)){
  
  qipp_report <- addSlide(qipp_report, "qipp_body") %>% 
    addTitle(noquote(str_replace_all(ipPlottableStrategies$StrategyDescription[i],"[^[:alnum:][:space:]-]", ""))) %>%  # "ACS Vaccine Preventable Conditions"
    addPlot(function() plot(plot_ip_fun[[i]])) %>% 
    addPlot(function() plot(plot_ip_trend[[i]])) %>% 
    #addPlot(function() plot(plot_ip_cost[[i]]))
    # %>%
    addPlot(function() plot(plot_ip_roc[[i]])) # %>% 
    # addParagraph(stringr::str_c("?", format(round(summaryOutputIP$Costs[i], -3), big.mark = ","),
    #                             " spent", "  ",
    #                             format(round(summaryOutputIP$Spells[i], -1), big.mark = ","),
    #                             " spells", "  ",
    #                             round(summaryOutputIP$propSpells[i]*100, 1),
    #                             "% of all IP spells"
    #                            )
    #             )
}


# TEST write document -----------------------------------------------------
filename <- "qipp_example_v5.pptx" # the document to produce
# TEST write qipp_report 
writeDoc(qipp_report, filename)



# 4: TITLE AE ---------------------------------------------------------

qipp_report <- addSlide(qipp_report, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>% 
  addTitle("A&E") 


# 4: BODY AE ---------------------------------------------------------

for(i in seq(aePlottableStrategies$Strategy)){
  
  qipp_report <- addSlide(qipp_report, "qipp_body") %>% 
    addTitle(noquote(str_replace_all(aePlottableStrategies$StrategyDescription[i],"[:punct:]", ""))) %>%  # "ACS Vaccine Preventable Conditions"
    addPlot(function() plot(plot_ae_fun[[i]])) %>% 
    addPlot(function() plot(plot_ae_trend[[i]])) %>% 
    addPlot(function() plot(plot_ae_roc[[i]])) 
}


# 5: TITLE OP ---------------------------------------------------------

qipp_report <- addSlide(qipp_report, "poster") %>%
  addImage("qipp_photo_inpatient.png") %>% 
  addTitle("Outpatients") 


# 5: BODY OP ---------------------------------------------------------

for(i in seq(opPlottableStrategies$Strategy)){
  
  qipp_report <- addSlide(qipp_report, "qipp_body") %>% 
    addTitle(noquote(str_replace_all(opPlottableStrategies$StrategyDescription[i],"[:punct:]", ""))) %>%  # "ACS Vaccine Preventable Conditions"
    addPlot(function() plot(plot_op_fun[[i]])) %>% 
    addPlot(function() plot(plot_op_trend[[i]])) %>% 
    addPlot(function() plot(plot_op_roc[[i]]))
}

# for(i in seq(opPlottableFUFStrategies$Strategy)){
#   
#   qipp_report <- addSlide(qipp_report, "qipp_body") %>% 
#     addTitle(noquote(str_replace_all(opPlottableFUFStrategies$StrategyDescription[i],"[:punct:]", ""))) %>%  # "ACS Vaccine Preventable Conditions"
#     addPlot(function() plot(plot_fuf_fun[[i]])) %>% 
#     addPlot(function() plot(plot_fuf_trend[[i]])) %>%  
#     addPlot(function() plot(plot_fuf_cost[[i]])) 
#  }



# 6. EXAMPLE TABLE ---------------------------------------------------
# 
# test_summary <- summaryOutputIP %>% 
#   group_by() %>% 
#   na.omit() %>% 
#   slice(1:10)
# 
# test_table <- tibble(colour       = rep("", length(test_summary$Significance)),
#                      activ_subset = test_summary$StrategyDescription,
#                      spells       = test_summary$Spells,
#                      spend        = test_summary$Costs,
#                      rate         = test_summary$Significance,
#                      roc          = test_summary$RocSignificance
#                      )
# 
# test_flex <- FlexTable(test_table) %>% 
#   setFlexTableBackgroundColors(j = 1, 
#                                colors = ifelse(test_summary$RocSignificance == "Not Significant", "hotpink1", "grey"))
# 
# 
# qipp_report <- addSlide(qipp_report, "contentA") %>%
#   addTitle("Flextable: Proof of Concept") %>% 
#   addFlexTable(test_flex)
#   

# slidex -------------------------------------------------------------
qipp_report <- addSlide(qipp_report, "contentB") %>% 
  addTitle("Genesis") %>% 
  addParagraph("And the earth was without form and void;\nand darkness was upon the face of the deep.") %>% 
  # adding a new paragraph here creates block 2
  addParagraph("And the earth was without form and void;")

# contact -------------------------------------------------------------

# \n is perhaps the best way to string paragraphs of regular text together.
# Can also create a string of paragraphs using pot() and set_of_paragraphs:
# Useful for formatted text
# Change the default font size and font family:
 options('ReporteRs-fontsize'= 20, 'ReporteRs-default-font'='Segoe UI Light')

 contact_name  <- pot("aj", format = textBold())
 contact_role  <- pot("ha")
 contact_email <- pot("ad", format = textNormal(color = "blue", underline = T))
 contact_tel   <- pot("01")
 
# parProperties() : Look at default options

qipp_report <- addSlide(qipp_report, "contact") %>% 
  addTitle("Contact") %>% 
  addParagraph(value = set_of_paragraphs(contact_name,
                                         contact_role,
                                         contact_email,
                                         contact_tel),
              par.properties = parProperties(padding = 0)
              )

# qipp_report <- addFlexTable( qipp_report, vanilla.table(iris[1:10,] ) )


# write document -----------------------------------------------------
filename <- "qipp_example_v5.pptx" # the document to produce
# write qipp_report 
writeDoc(qipp_report, filename)



