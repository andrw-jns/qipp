SET NOCOUNT ON
GO
SELECT
	(COUNT([aekey]) * [ESP2013OverSubjectPop]) / 2 as [DSRate]
	, (COUNT([aekey]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSRateVar]
	, (SUM([Cost]) * [ESP2013OverSubjectPop]) / 2 as [DSCosts]
	, (SUM([Cost]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSCostsVar]
	, COUNT([aekey]) [Attendances]
    , SUM([Cost]) [Costs]
	, [FYear]
	, qipp.[CCGCode]
	, [AmbNoInvNoTreat_v1]
	, [FrequentFlyers_v1]
	, [LeftBeforeTreatment_v1]
	, [LowCostReferredDischarged_v1]
	
FROM [StrategicAnalytics_QIPP].[Working].[vwQIPP_SUS_AE1516] qipp
	left outer join (
		select [CCGCode], [AgeGroup], [Gender], [ESP2013OverSubjectPop]
		from [StrategicAnalytics_QIPP].[Working].[vwESPOverSubjectPop]
		where 
			[PopulationType] = 'Resident'
			and [Year] = 2015
	) pop
		on qipp.AgeGroup5 = pop.AgeGroup
			and qipp.Gender = pop.Gender
			and qipp.CCGCode = pop.CCGCode 
			
GROUP BY
	[FYear]
    , qipp.[CCGCode]
	, pop.[ESP2013OverSubjectPop]
	, [AmbNoInvNoTreat_v1]
	, [FrequentFlyers_v1]
	, [LeftBeforeTreatment_v1]
	, [LowCostReferredDischarged_v1]