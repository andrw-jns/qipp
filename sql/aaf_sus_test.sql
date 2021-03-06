-- Applying alcohol attributable fractions to SUS IP data
--NOTES
-- PLEASE READ NOTES BEFORE RUNNING.
--
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
-- 
-- 12/03/2017 - changed episodeid to be bigint for financial year >= 201617
--------------------------------------------------------------------------------------------------

Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Start - removing current table, if it exists.', GETDATE()
GO

IF Object_id('[StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617') IS NOT NULL 
BEGIN
DROP TABLE [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617
END

GO
Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Creating table', GETDATE()
GO
-- set up
CREATE TABLE [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps	
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
	, [AAF] float
    , [alc_group] varchar(2)
)

-- Set up the table
Insert into
	[StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps
	
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
	, CAST(1 as float) [AAF]
    , [alc_group] = NULL
	
FROM [AcuteDW].dbo.tbInpatientEpisodes1617 a
	LEFT OUTER JOIN [AcuteDW].dbo.tbIPDiagnosis1617 b
		on a.EpisodeId = b.EpisodeId

WHERE 
	IsCosted = '1' 
	and isdominant ='1' 
	and gendercode in ('1','2')
	and ageonadmission is not null 

GO		

	-- CREATE INDEX	
	--AgeGroup		nvarchar(7)		19 categories
	--Gender		nchar(1)		F / M
	--DeathFlag		bit				0 or 1
Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Creating Indexes', GETDATE()	
	
CREATE INDEX IndexAgeGroup
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (AgeGroup)
	
CREATE INDEX IndexGender
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Gender)
	
CREATE INDEX IndexDeathFlag
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (DeathFlag)
	
CREATE INDEX IndexDiagnosis1
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis1)
	
CREATE INDEX IndexDiagnosis2
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis2)
	
CREATE INDEX IndexDiagnosis3
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis3)
	
CREATE INDEX IndexDiagnosis4
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis4)
	
CREATE INDEX IndexDiagnosis5
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis5)
	
CREATE INDEX IndexDiagnosis6
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis6)
	
CREATE INDEX IndexDiagnosis7
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis7)
	
CREATE INDEX IndexDiagnosis8
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis8)
	
CREATE INDEX IndexDiagnosis9
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis9)
	
CREATE INDEX IndexDiagnosis10
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis10)
	
CREATE INDEX IndexDiagnosis11
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis11)
	
CREATE INDEX IndexDiagnosis12
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis12)
	
CREATE INDEX IndexDiagnosis13
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis13)
	
CREATE INDEX IndexDiagnosis14
	ON [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps (Diagnosis14)
-------------------------------------------------------------------------
GO
-------------------------------------------------------------------------
-- Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- select 'Diagnosis1', GETDATE()


UPDATE a
SET a.AAF = ISNULL(1 - aaf.AAF, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis1 = aaf.DiagnosisCode
	
WHERE 
	Diagnosis1 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag


-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis2', GETDATE()


UPDATE a
Set a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis2 = aaf.DiagnosisCode
WHERE 
	Diagnosis2 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis3', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis3 = aaf.DiagnosisCode
WHERE 
	Diagnosis3 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis4', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis4 = aaf.DiagnosisCode
WHERE 
	Diagnosis4 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis5', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis5 = aaf.DiagnosisCode
WHERE 
	Diagnosis5 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis6', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis6 = aaf.DiagnosisCode
WHERE 
	Diagnosis6 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis7', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis7 = aaf.DiagnosisCode
WHERE 
	Diagnosis7 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis8', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis8 = aaf.DiagnosisCode
WHERE 
	Diagnosis8 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag

-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis9', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis9 = aaf.DiagnosisCode
WHERE 
	Diagnosis9 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis10', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis10 = aaf.DiagnosisCode
WHERE 
	Diagnosis10 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis11', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis11 = aaf.DiagnosisCode
WHERE 
	Diagnosis11 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis12', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis12 = aaf.DiagnosisCode
WHERE 
	Diagnosis12 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
			
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis13', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis13 = aaf.DiagnosisCode
WHERE 
	Diagnosis13 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag
	
			
-- INSERT INTO [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- SELECT 'Diagnosis14', GETDATE()

UPDATE a
SET a.AAF = a.AAF * ISNULL(1 - aaf.aaf, 1)
FROM [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a
	LEFT OUTER JOIN	[StrategicAnalytics_QIPP].Working.tbAAF2014_FullDiagnosisCodes_aj aaf
		ON a.Diagnosis14 = aaf.DiagnosisCode
WHERE 
	Diagnosis14 is not null
	and a.AgeGroup = aaf.AgeGroup
	and a.Gender = aaf.Gender
	and ISNULL(aaf.Deathflag, a.Deathflag) = a.DeathFlag	
	

-- Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- select 'Final', GETDATE()


Update a
Set a.AAF = (Select MAX(v) FROM (VALUES (1 - AAF), (0)) as value(v))
from [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617_grps a


Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
select 'Ready to drop columns', GETDATE()

DROP INDEX IndexAgeGroup on Working.tbSUSIPAAF1617
DROP INDEX IndexGender on Working.tbSUSIPAAF1617
DROP INDEX IndexDeathFlag on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis1 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis2 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis3 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis4 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis5 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis6 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis7 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis8 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis9 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis10 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis11 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis12 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis13 on Working.tbSUSIPAAF1617
DROP INDEX IndexDiagnosis14 on Working.tbSUSIPAAF1617
GO
ALTER TABLE [StrategicAnalytics_QIPP].Working.tbSUSIPAAF1617
DROP COLUMN AgeGroup, Gender, DeathFlag, Diagnosis1, Diagnosis2, Diagnosis3, Diagnosis4, Diagnosis5, Diagnosis6, Diagnosis7, Diagnosis8, Diagnosis9, Diagnosis10, Diagnosis11, Diagnosis12, Diagnosis13, Diagnosis14


-- GO
-- Insert into [StrategicAnalytics_QIPP].Working.tbAAFProgress
-- select 'Finished', GETDATE()
GO

-- To check progress run:

--SELECT [CurrentPosition]
--      ,[DateTime]
--  FROM [StrategicAnalytics_QIPP].[Working].[tbAAFProgress]
