SET NOCOUNT ON
GO
SELECT
	(COUNT([attendkey]) * [ESP2013OverSubjectPop]) / 2 as [DSRate]
	, (COUNT([attendkey]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSRateVar]
	, (SUM([Cost]) * [ESP2013OverSubjectPop]) / 2 as [DSCosts]
	, (SUM([Cost]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSCostsVar]
	, COUNT([attendkey]) [Attendances]
    , SUM([Cost]) [Costs]
	, [FYear]
	, qipp.[CCGCode]
	, [Cons_Ref_v1]
	, [FUF_NonSurgAdult_v2]
	, [FUF_NonSurgChild_v2]
	, [FUF_SurgAdult_v2]
	, [FUF_SurgChild_v2]
	, [GPRef_NonSurgAdult_v1]
	, [GPRef_NonSurgChild_v1]
	, [GPRef_SurgAdult_v1]
	, [GPRef_SurgChild_v1]
	, [PLCV_v1]

FROM [StrategicAnalytics_QIPP].[Working].[vwQIPP_SUS_OP1516] qipp
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
	, [Cons_Ref_v1]
	, [FUF_NonSurgAdult_v2]
	, [FUF_NonSurgChild_v2]
	, [FUF_SurgAdult_v2]
	, [FUF_SurgChild_v2]
	, [GPRef_NonSurgAdult_v1]
	, [GPRef_NonSurgChild_v1]
	, [GPRef_SurgAdult_v1]
	, [GPRef_SurgChild_v1]
	, [PLCV_v1]