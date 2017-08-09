-- Applying alcohol attributable fractions to SUS PbR data
-- NOTES
-- PLEASE READ NOTES BEFORE RUNNING.

-- Note that this query does not necessarily do everything that you might want it to do:
--
-- It does not archive the previous version of the table (if it exists), it will just delete it.
-- It assumes that the progress table [StrategicAnalytics_QIPP].[Working].[tbAAFProgress] is ready to go. 
--	You might want to truncate it first, but it's only used for you to check the query's progress. If you do want to truncate, use this:
-- 		TRUNCATE TABLE [StrategicAnalytics_QIPP].[Working].[tbAAFProgress]
--
-- To check progress run:
--
--SELECT [CurrentPosition]
--      ,[DateTime]
--  FROM [StrategicAnalytics_QIPP].[Working].[tbAAFProgress]
--
-- Notes on speed 11:58 19/06/2015
-- I want to try adding some indexes on DiagCount, AgeGroup, Gender, and DeathFlag. 
-- This query is not going to scale nationally without that.

-- AJ: LOOKING AT JS 2017 CODE FOR STAFFS MODELLING, IT APPEARS: [StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
-- IS USED WHATEVER THE YEAR.
--------------------------------------------------------------------------------------------------

-- SET @SQL = REPLACE(@SQL, N'ADMISSION_DATE'			, N'pbrS.ADMISSION_DATE'			)
-- SET @SQL = REPLACE(@SQL, N'ADMISSION_METHOD'		, N'pbrS.ADMISSION_METHOD'			)
-- SET @SQL = REPLACE(@SQL, N'AGE_ON_ADMISSION'		, N'pbrS.AGE_ON_ADMISSION'			)
-- SET @SQL = REPLACE(@SQL, N'DISCHARGE_DATE'			, N'pbrS.DISCHARGE_DATE'			)
-- SET @SQL = REPLACE(@SQL, N'DISCHARGE_METHOD'		, N'pbrS.DISCHARGE_METHOD'			)
-- SET @SQL = REPLACE(@SQL, N'PATIENT_CLASSIFICATION'	, N'pbrS.PATIENT_CLASSIFICATION'	)
-- SET @SQL = REPLACE(@SQL, N'PRIMARY_PROCEDURE_CODE'	, N'pbrS.PRIMARY_PROCEDURE_CODE'	)
-- SET @SQL = REPLACE(@SQL, N'SEX'						, N'pbrS.SEX'						)

Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Start - removing current table, if it exists.', GETDATE()
GO

IF Object_id('[StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516') IS NOT NULL 
BEGIN
DROP TABLE [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516
END

GO
Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Creating table', GETDATE()
GO
-- set up
CREATE TABLE [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516	
(
	[SPELL_ID] bigint NOT NULL
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
	, [AAF] float
)

-- Set up the table
Insert into
	[StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516
	
SELECT 
	pbrS.SPELL_ID
	, case 
		when pbrS.AGE_ON_ADMISSION >= 0 and pbrS.AGE_ON_ADMISSION < 16 then '0to15' 
		when pbrS.AGE_ON_ADMISSION >= 16 and pbrS.AGE_ON_ADMISSION < 25 then '16to24' 
		when pbrS.AGE_ON_ADMISSION >= 25 and pbrS.AGE_ON_ADMISSION < 75 then
			cast(FLOOR(pbrS.AGE_ON_ADMISSION/5)*5 -5*(1-FLOOR(pbrS.AGE_ON_ADMISSION/5) % 2) as nvarchar(2)) + 'to' + 
			cast(FLOOR(pbrS.AGE_ON_ADMISSION/5)*5 -5*(1-FLOOR(pbrS.AGE_ON_ADMISSION/5) % 2) + 9 as nvarchar(2))
		when pbrS.AGE_ON_ADMISSION >= 75 and pbrS.AGE_ON_ADMISSION <= 110 then '75plus'
		else null
		end as AgeGroup
	, CASE pbrS.SEX
		WHEN '1' THEN 'M' 
		WHEN '2' THEN 'F'
		ELSE NULL
		END as Gender
	, case 
		when pbrS.DISCHARGE_METHOD = '4' then 1  
		when pbrS.DISCHARGE_METHOD is not null then 0 
		else NULL  
		end as DeathFlag
	, CAST(PRIMARY_DIAGNOSIS_CODE_CLND AS NVARCHAR(5))      [Diagnosis1] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_1_CLND AS NVARCHAR(5))  [Diagnosis2] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_2_CLND AS NVARCHAR(5))  [Diagnosis3] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_3_CLND AS NVARCHAR(5))  [Diagnosis4] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_4_CLND AS NVARCHAR(5))  [Diagnosis5] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_5_CLND AS NVARCHAR(5))  [Diagnosis6] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_6_CLND AS NVARCHAR(5))  [Diagnosis7] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_7_CLND AS NVARCHAR(5))  [Diagnosis8] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_8_CLND AS NVARCHAR(5))  [Diagnosis9] 
	, CAST(SECONDARY_DIAGNOSIS_CODE_9_CLND AS NVARCHAR(5))  [Diagnosis10]
	, CAST(SECONDARY_DIAGNOSIS_CODE_10_CLND AS NVARCHAR(5)) [Diagnosis11]
	, CAST(SECONDARY_DIAGNOSIS_CODE_11_CLND AS NVARCHAR(5)) [Diagnosis12]
	, CAST(SECONDARY_DIAGNOSIS_CODE_12_CLND AS NVARCHAR(5)) [Diagnosis13]
	, CAST(1 as float) [AAF]
	
FROM 
	[HESData].dbo.tbSUSPbRInpatientSpells1516 pbrS
	left outer join [HESData].dbo.tbSUSPbRInpatientEpisodes1516 pbrE
		on  pbrS.SPELL_ID = pbrE.SPELL_ID 
WHERE 
	pbrS.SEX LIKE '[12]'
	and pbrS.AGE_ON_ADMISSION is not null 
	and CAST(pbrS.AGE_ON_ADMISSION AS INT) between 0 and 110 
	AND pbrS.EXCLUSION_REASON_SPELL IS NULL
	AND pbrE.IC_PBR_QUALIFIED = 'Y'
	AND pbrE.DOMINANT_EPISODE_IND = '1'
	AND pbrE.PBR_EXCLUDED_INDICATOR = '0'

GO		

	-- CREATE INDEX	
	--AgeGroup		nvarchar(7)		19 categories
	--Gender		nchar(1)		F / M
	--DeathFlag		bit				0 or 1
Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Creating Indexes', GETDATE()	
	
CREATE INDEX IndexAgeGroup
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (AgeGroup)
	
CREATE INDEX IndexGender
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Gender)
	
CREATE INDEX IndexDeathFlag
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (DeathFlag)
	
CREATE INDEX IndexDiagnosis1
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis1)
	
CREATE INDEX IndexDiagnosis2
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis2)
	
CREATE INDEX IndexDiagnosis3
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis3)
	
CREATE INDEX IndexDiagnosis4
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis4)
	
CREATE INDEX IndexDiagnosis5
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis5)
	
CREATE INDEX IndexDiagnosis6
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis6)
	
CREATE INDEX IndexDiagnosis7
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis7)
	
CREATE INDEX IndexDiagnosis8
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis8)
	
CREATE INDEX IndexDiagnosis9
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis9)
	
CREATE INDEX IndexDiagnosis10
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis10)
	
CREATE INDEX IndexDiagnosis11
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis11)
	
CREATE INDEX IndexDiagnosis12
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis12)
	
CREATE INDEX IndexDiagnosis13
	ON [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 (Diagnosis13)
	
-------------------------------------------------------------------------
GO
-------------------------------------------------------------------------
Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Diagnosis1', GETDATE()


UPDATE a
SET a.AAF = ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis1 = aaf.DiagnosisCode
	
WHERE 
	Diagnosis1 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag


INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis2', GETDATE()


UPDATE a
Set a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis2 = aaf.DiagnosisCode
WHERE 
	Diagnosis2 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis3', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis3 = aaf.DiagnosisCode
WHERE 
	Diagnosis3 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis4', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis4 = aaf.DiagnosisCode
WHERE 
	Diagnosis4 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis5', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis5 = aaf.DiagnosisCode
WHERE 
	Diagnosis5 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis6', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis6 = aaf.DiagnosisCode
WHERE 
	Diagnosis6 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis7', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis7 = aaf.DiagnosisCode
WHERE 
	Diagnosis7 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis8', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis8 = aaf.DiagnosisCode
WHERE 
	Diagnosis8 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis9', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis9 = aaf.DiagnosisCode
WHERE 
	Diagnosis9 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis10', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis10 = aaf.DiagnosisCode
WHERE 
	Diagnosis10 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis11', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis11 = aaf.DiagnosisCode
WHERE 
	Diagnosis11 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis12', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis12 = aaf.DiagnosisCode
WHERE 
	Diagnosis12 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
			
INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
SELECT 'Diagnosis13', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes aaf
		ON a.Diagnosis13 = aaf.DiagnosisCode
WHERE 
	Diagnosis13 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
	
	

Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Final', GETDATE()


Update a
Set a.AAF = (Select MAX(v) FROM (VALUES (1 - AAF), (0)) as value(v))
from [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516 a


Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Ready to drop columns', GETDATE()

DROP INDEX IndexAgeGroup on Working.tbPbRIPAAF1516
DROP INDEX IndexGender on Working.tbPbRIPAAF1516
DROP INDEX IndexDeathFlag on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis1 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis2 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis3 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis4 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis5 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis6 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis7 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis8 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis9 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis10 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis11 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis12 on Working.tbPbRIPAAF1516
DROP INDEX IndexDiagnosis13 on Working.tbPbRIPAAF1516
GO
ALTER TABLE [StrategicAnalytics_QIPP].Working.tbPbRIPAAF1516
DROP COLUMN AgeGroup, Gender, DeathFlag, Diagnosis1, Diagnosis2, Diagnosis3, Diagnosis4, Diagnosis5, Diagnosis6, Diagnosis7, Diagnosis8, Diagnosis9, Diagnosis10, Diagnosis11, Diagnosis12, Diagnosis13


-- GO
-- Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- select 'Finished', GETDATE()
GO

-- To check progress run:

--SELECT [CurrentPosition]
--      ,[DateTime]
--  FROM [StrategicAnalytics_QIPP].[Working].[tbAAFProgress]
