############################################################################
" AAF"
" UPDATE TO FOLLOW NATIONAL INDICATOR METHOD* AND CONVENTIONAL GROUPS"
# *SOURCE: Local Alcohol Profiles for England 2015 user guide
# "2017-11-10 16:13:35 GMT"
###########################################################################



library(DBI)
library(odbc)
library(dbplyr) 
library(stringr)
library(tidyverse)



# SQL code -----------------------------------------------------------

connect <- dbConnect(odbc::odbc(),
                 driver = "SQL Server",
                 server = "CSU-SQL-03",
                 database = "StrategicAnalytics_QIPP",
                 #uid    = "CLIENTS\\andrew.jones",
                 port = 1433
)

sus_table <- dbGetQuery(connect,
                        " 
SELECT *
FROM Working.tbSUSIPAAF1617_grps
"
                        )

ref_table <- dbGetQuery(connect,
                        "
SELECT * 
FROM Working.tbAAF2014_FullDiagnosisCodes_aj
                        "
                        ) %>% 
  select(-DeathFlag) %>% 
  # Why is there a death flag in this table?
  filter(AAF >= 0)
# Only positive 


# Divide reference table ---------------------------------------------

# really this should be a function (revise tidyeval)

ref_wh <- ref_table %>%
  filter(alc_group == "wh") %>% 
  select(-alc_group) %>% 
  rename(alc_wholly = AAF)

ref_pa <- ref_table %>%
  filter(alc_group == "pa") %>% 
  select(-alc_group) %>% 
  rename(alc_acute = AAF)

ref_pc <- ref_table %>% 
  filter(alc_group == "pc") %>% 
  select(-alc_group) %>% 
  rename(alc_chronic = AAF)

rm(ref_table)
gc()
# sus_table2 <- dbGetQuery(connect,
#                         "
#                         SELECT TOP 100 * FROM Working.tbSUSIPAAF1617
#                         ")

# sus_dummy <- sus_table %>% 
#   select(-1, -2, - c(11:19)) %>% 
#   mutate(id = row_number()) %>%
#   # bare essentials:
#   select(id, everything(), DeathFlag , -aaf_wholly, -aaf_chronic, -aaf_acute)

# saveRDS(sus_table_dummy, "main_table.RDS")
# saveRDS(ref_table, "ref_table.RDS")


# Add KEY column, copy and truncate ----------------------------------

sus_table <- sus_table %>% 
  mutate(key = row_number())

sus_truncate <- sus_table %>% 
  # bare essentials:
  select(key, everything(), -DeathFlag , -EpisodeId, -SUSGeneratedIdentifier, -aaf_wholly, -aaf_chronic, -aaf_acute)

tmp_long <- sus_truncate %>% 
  gather(diagnosis, code, 4:17) %>% 
  filter(!is.na(code)) %>% # optional
  left_join(ref_wh, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) %>% 
  left_join(ref_pa, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) %>% 
  left_join(ref_pc, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) 

# If you wanted mutually exclusive opportunites, then max() would be done
# before grouping(?)


# Take the maximum value (this takes a while):
tmp_long <- tmp_long %>% 
  group_by(key) %>% 
  summarise_at(vars(c("alc_wholly", "alc_acute", "alc_chronic")),
               funs(max(., na.rm = T)))



tmp_long[2:4][tmp_long[2:4] == "-Inf"|tmp_long[2:4] == 0] <- NA


# START HERE (if run prev) -------------------------------------------



# saveRDS(tmp_long, "aaf_patients.RDS")
tmp_long <- read_rds("aaf_patients.RDS")

# Produces table with ids and aaf (by group) (which are largely exclusive)

# Now join back to original set:
"CHECK THIS MARRIES TOMORROW!"

sus_table <- dbGetQuery(connect,
                        " 
                        SELECT *
                        FROM Working.tbSUSIPAAF1617_grps
                        "
)

sus_table <- sus_table %>% 
  mutate(key = row_number())



final_episode_lookup <- left_join(sus_table %>% select(key, 1 , 2), tmp_long, by = "key") %>% 
  select(-key)

"SHOULD SAVE THE RDS HERE"

# Specify schema for new table to be written to SQL:
# https://github.com/r-dbi/odbc/issues/91

install.packages("Rcpp")
library(Rcpp)
devtools::install_github("rstats-db/odbc@SQLTable")
# devtools::install_github("rstats-db/odbc")
# ASLo useful :https://rdrr.io/cran/RSQLite/man/dbWriteTable.html


library(DBI)
library(odbc)

# Need this to specify schema
tabowl <- dbId(con = connect, 
               name = "tbSUSIPAAF1617_new_groups_2",
               schema = "Working")

# This seems to take a while:
# dbWriteTable(connect, "Working.tbSUSIPAAF1617_new_groups", final_episode_lookup[1:10,])

# test:
dbWriteTable(connect, tabowl, mtcars[1:10,])

# This workflow would save many tables being created (most of which cannot be linked
# to each other)