-----------------------------------------------------------------------------------
-- Generate Views for AE Manually
-----------------------------------------------------------------------------------

CREATE VIEW Working.vwQIPP_SUS_AE1213 AS 

SELECT 
'201213' as FYear 
	,  ae.EpisodeId AS [encrypted_hesid]
	, ae.SUSGeneratedIdentifier AS [aekey]
    , ae.[Cost] 
    , CASE
		 WHEN ae.[Age] > 89
			 THEN '90plus'
		 ELSE RIGHT('00' + ISNULL(CAST(FLOOR(ae.[Age] / 5) * 5 as nvarchar(2)), ''), 2) + 
			 'to' + RIGHT('00' + ISNULL(CAST((FLOOR(ae.[Age] / 5) * 5) + 4 as nvarchar(2)), ''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(ae.GenderDescription, 1) as [Gender]

, CASE
WHEN 
AmbNoInvNoTreat.EpisodeId is not null 
and aeattendancedisposalcode = '03'
and aearrivalmodecode = '1' 

THEN 1
ELSE 0 
END as [AmbNoInvNoTreat_v1] 
, CASE
WHEN 
FrequentFlyers.NHSNumber is not null
and FrequentFlyers.OriginalProviderCode is not null

THEN 1
ELSE 0 
END as [FrequentFlyers_v1] 
, CASE
WHEN 
AEAttendanceDisposalCode = '12'

THEN 1
ELSE 0 
END as [LeftBeforeTreatment_v1] 
, CASE
WHEN 
(
CBSADerivedHRGCode LIKE 'VB0[69]Z'
OR CBSADerivedHRGCode LIKE 'VB1[01]Z'
OR CBSADerivedHRGCode LIKE 'V0[5-8]'
)
and AEAttendanceDisposalCode LIKE '0[23]' 

THEN 1
ELSE 0 
END as [LowCostReferredDischarged_v1] 
FROM 
[AcuteDW].dbo.tbAE1213 ae 
left outer join [StrategicAnalytics_QIPP].Working.SUSAEAmbNoInvNoTreat1213 AmbNoInvNoTreat
on ae.EpisodeId = AmbNoInvNoTreat.EpisodeId 
left outer join [StrategicAnalytics_QIPP].Working.SUSAEFrequentFlyers1213 FrequentFlyers
on ae.nhsnumber = FrequentFlyers.nhsnumber 
and ae.OriginalProviderCode = FrequentFlyers.OriginalProviderCode
WHERE 
1 = 1 
and GenderCode LIKE '[12]'
and Age is not null
and Age between 0 and 110
and IsCosted = '1'
and ae.ErrorFlags is null 
and (AEDepartmentTypeCode is null or AEDepartmentTypeCode != '04') 