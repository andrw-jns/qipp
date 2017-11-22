---------------------------------------
--- RE-WORKING AAF
---------------------------------------

select *
from Working.tbPbRIPAAF1516

-- JS lookup diag, condition, group (wholly/partial(a/c)), subgroup.
select top 10 *
from Working.tbAAF2014_groups

-- JS: AAF by diagnosis, age, gender (diag, age, gender, death, AAF)
select top 10 *
from Working.tbAAF2014_FullDiagnosisCodes

-- AJ: lookup coming from R script. (Almost a duplication of JS work, but a little more detail)
-- Removed ~2000 rows of erroneous diagnoses.
select top 10 *
from Working.tb_aaf_grp_lookup_aj

--------------------

CREATE TABLE [StrategicAnalytics_QIPP].[Working].[tbAAF2014_FullDiagnosisCodes_aj] 
(
	[DiagnosisCode] nvarchar(5)
	, [AgeGroup] varchar(10)
	, [Gender] nchar(1)
    , [DeathFlag] bit
    , [AAF] float
    , [alc_group] VARCHAR(2)
)

-----------

INSERT INTO [StrategicAnalytics_QIPP].[Working].[tbAAF2014_FullDiagnosisCodes_aj] 
    SELECT base.*,
    lkp.[group]
from Working.tbAAF2014_FullDiagnosisCodes base
    LEFT JOIN Working.tb_aaf_grp_lookup_aj lkp
    ON base.DiagnosisCode = lkp.DiagnosisCode
    WHERE [group] != 'NULL'

------

--- NOW NEED TO UPDATE THE AAF TABLES FOR EACH YR

--- We use the script JS (from his staffs moonlighting) aaf_sus.sql in qipp/sql to build the table. Then

--- SEE UPDATING - aaf_sus_test.sql

