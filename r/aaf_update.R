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

# *** ----------------------------------------------------------------
# Connection --------------------------------------------------------

connect <- dbConnect(odbc::odbc(),
                 driver = "SQL Server",
                 server = "CSU-SQL-03",
                 database = "StrategicAnalytics_QIPP",
                 #uid    = "CLIENTS\\andrew.jones",
                 port = 1433
)


# SQL  --------------------------------------------------------------

# 1. Create and populate table of SUS episodes with diagnoses:

# Note: already configured for 1617
years <- c("1213", "1314", "1415", "1516")

for(i in years){
  dbGetQuery(connect,
  # adapted from JS aaf_sus.sql:           
  str_c("
  CREATE TABLE [StrategicAnalytics_QIPP].Working.tbSUSIPAAF", i, "_grps	
  (
    [EpisodeId] bigint NOT NULL
    , SUSGeneratedIdentifier varchar(38) 
    , [AgeGroup] varchar(6)
    , [Gender] varchar(1)
    , [DeathFlag] int
    , [Diagnosis1] varchar(5)
    , [Diagnosis2] varchar(5)
    , [Diagnosis3] varchar(5)
    , [Diagnosis4] varchar(5)
    , [Diagnosis5] varchar(5)
    , [Diagnosis6] varchar(5)
    , [Diagnosis7] varchar(5)
    , [Diagnosis8] varchar(5)
    , [Diagnosis9] varchar(5)
    , [Diagnosis10] varchar(5)
    , [Diagnosis11] varchar(5)
    , [Diagnosis12] varchar(5)
    , [Diagnosis13] varchar(5)
    , [Diagnosis14] varchar(5)
  )"
  ))}


for(i in years){
  dbGetQuery(connect,
             str_c("
             --- Set up the table
             Insert into
             [StrategicAnalytics_QIPP].Working.tbSUSIPAAF", i, "_grps
             
             SELECT 
             a.EpisodeId
             , a.SUSGeneratedIdentifier
             , case 
             when AgeOnAdmission >= 0 and AgeOnAdmission < 16 then '0to15' 
             when AgeOnAdmission >= 16 and AgeOnAdmission < 25 then '16to24' 
             when AgeOnAdmission >= 25 and AgeOnAdmission < 75 then
             cast(FLOOR(AgeOnAdmission/5)*5 -5*(1-FLOOR(AgeOnAdmission/5) % 2) as nvarchar(2)) + 'to' + 
             cast(FLOOR(AgeOnAdmission/5)*5 -5*(1-FLOOR(AgeOnAdmission/5) % 2) + 9 as nvarchar(2))
             when AgeOnAdmission >= 75 and AgeOnAdmission < 120 then '75plus'
             else null
             end as AgeGroup
             , LEFT(a.GenderDescription,1) as Gender
             , case 
             when a.DischargeMethodCode = '4' then 1  
             when a.DischargeMethodCode is not null then 0 
             else NULL  
             end as DeathFlag
             , Diagnosis1
             , Diagnosis2
             , Diagnosis3
             , Diagnosis4
             , Diagnosis5
             , Diagnosis6
             , Diagnosis7
             , Diagnosis8
             , Diagnosis9
             , Diagnosis10
             , Diagnosis11
             , Diagnosis12
             , Diagnosis13
             , Diagnosis14
             
             
             FROM [AcuteDW].dbo.tbInpatientEpisodes", i, " a
             LEFT OUTER JOIN [AcuteDW].dbo.tbIPDiagnosis", i,  " b
             on a.EpisodeId = b.EpisodeId
             
             WHERE 
             IsCosted = '1' 
             and isdominant ='1' 
             and gendercode in ('1','2')
             and ageonadmission is not null 
             
  "
))
}

# 2. Pull the above into R

# for(i in years){
# assign(paste0("sus_table_", i), 
#        dbGetQuery(connect,
#                   str_c(" 
#                   SELECT *
#                   FROM Working.tbSUSIPAAF", i,"_grps
#                   "
#        )))
# }

sus_tables <- list()

for(i in seq_along(years)){
  sus_tables[[i]] <- dbGetQuery(connect, str_c(
    "
    SELECT * 
    FROM Working.tbSUSIPAAF", years[i],"_grps"
    ))
}

# 3. Pull AAF reference table (created with aaf_investigations.R)

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



# ***  ---------------------------------------------------------------
# R ------------------------------------------------------------------
# *** ----------------------------------------------------------------


# Divide reference table ---------------------------------------------

# This should be a function (revise tidyeval):

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


# Add KEY column, copy and truncate ----------------------------------

sus_tables <- map(sus_tables, function(df) df %>%
                    mutate(key = row_number()))

sus_truncates <- map(sus_tables, function(df) df %>%
                       # bare essentials to pivot (which will produce some behemoths)
                       select(key, everything(), -DeathFlag , -EpisodeId, -SUSGeneratedIdentifier))  # 1617:, -aaf_wholly, -aaf_chronic, -aaf_acute))


# Perhaps do these one at a time:
tmp_long <- sus_truncates[[1]] %>% 
  gather(diagnosis, code, matches("Diagnosis*")) %>%  # 4:17) %>% 
  filter(!is.na(code)) %>% # optional
  left_join(ref_wh, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) %>% 
  left_join(ref_pa, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) %>% 
  left_join(ref_pc, by = c("code" = "DiagnosisCode", "AgeGroup", "Gender")) 

# If you wanted mutually exclusive opportunites, then max() would be done
# before grouping(?)



# Take the maximum value (this takes a while ~ 11 mins):
Sys.time()
tmp_long <- tmp_long %>% 
  group_by(key) %>% 
  summarise_at(vars(c("alc_wholly", "alc_acute", "alc_chronic")),
               funs(max(., na.rm = T)))
Sys.time()



tmp_long[2:4][tmp_long[2:4] == "-Inf"|tmp_long[2:4] == 0] <- NA

final_episode_lookup_1213 <- left_join(sus_tables[[1]] %>% select(key, 1 , 2), tmp_long, by = "key") %>% 
  select(-key)

"SHOULD SAVE THE RDS HERE"
# saveRDS(final_episode_lookup_1617, "aaf_1617.RDS")
# saveRDS(final_episode_lookup_1213, "aaf_1213.RDS")
final_episode_lookup_1617 <- readRDS("aaf_1617.RDS")
# *** ----------------------------------------------------------------


# START HERE (if run previously) -------------------------------------------


# Specify schema for new table to be written to SQL:
# https://github.com/r-dbi/odbc/issues/91

# install.packages("Rcpp")
library(Rcpp)
# library(DBI)
# library(odbc)
# devtools::install_github("rstats-db/odbc@SQLTable")
## NOT this one: devtools::install_github("rstats-db/odbc")
# ASLo useful reading :https://rdrr.io/cran/RSQLite/man/dbWriteTable.html



# Need this to specify schema
schema <- dbId(con = connect, 
               name = "tbSUSIPAAF1617_NEW_GROUPS",
               schema = "Working")

# This seems to take a while:
dbWriteTable(connect, schema, final_episode_lookup_1617)
# dbWriteTable(connect, "Working.tbSUSIPAAF1213_NEW_GROUPS", final_episode_lookup_1213)
rm(tmp_long, final_episode_lookup_1213)
gc()
# test:
# dbWriteTable(connect, tabowl, mtcars[1:10,])

# This workflow would save many tables being created (most of which cannot be linked
# to each other)