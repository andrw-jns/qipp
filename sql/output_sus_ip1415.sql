SET NOCOUNT ON
GO
SELECT (COUNT([SPELL_ID])*[ESP2013OverSubjectPop]) / 2 as [DSRate]
	, (COUNT([SPELL_ID]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSRateVar]
	, (SUM([Cost]) * [ESP2013OverSubjectPop]) / 2 as [DSCosts]
	, (SUM([Cost]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSCostsVar]
    , COUNT([SPELL_ID]) [Spells]
    , SUM([Cost]) [Costs]
	, [FYear]
    , qipp.[CCGCode]
	, qipp.[ACS_Acute_v3]
	, qipp.[ACS_Chronic_v3]
	, qipp.[ACS_Vaccine_v3]
	, qipp.[Alcohol_25pcto75pc_v3]
	, qipp.[Alcohol_5pcto25pc_v3]
	, qipp.[Alcohol_75pcto100pc_v3]
	, qipp.[Canc_Op_v1]
	--, qipp.[EOLC_Long_v2]
	--, qipp.[EOLC_Short_v2]
	, qipp.[Falls_v1]
	, qipp.[FrailElderly_Occasional_v1]
	, qipp.[FrailElderly_Usual_v1]
	, qipp.[Med_Unexplained_v1]
	, qipp.[Meds_Explicit_v1]
	, qipp.[Meds_Implicit_AntiDiabetics_v1]
	, qipp.[Meds_Implicit_Benzo_v1]
	, qipp.[Meds_Implicit_Diuretics_v1]
	, qipp.[Meds_Implicit_NSAIDs_v1]
	-- , qipp.[Obesity_Large_v1]
	-- , qipp.[Obesity_Marginal_v1]
	-- , qipp.[Obesity_Somewhat_v1]
	, qipp.[PLCV_Cosmetic_v1]
	, qipp.[PLCV_Cost_v1]
	, qipp.[PLCV_Ineffective_v1]
	, qipp.[PLCV_Risks_v1]
	-- , qipp.[RAID_ED_v2]
	, qipp.[Readmissions_v1]
	, qipp.[SelfHarm_v2]
	, qipp.[Smoking_Large_v1]
	, qipp.[Smoking_Somewhat_v1]
	--, qipp.[ZeroLoS_NP_Adult_v2]
	--, qipp.[ZeroLoS_NP_Child_v2]
	, qipp.[EOLC_Long_v3]
	, qipp.[EOLC_Short_v3]
	, qipp.[RAID_ED_v3]	
	, qipp.[ZeroLoS_NP_Adult_v3]
	, qipp.[ZeroLoS_NP_Child_v3]
	, qipp.[Obesity_Large_v2]
	, qipp.[Obesity_Marginal_v2]
	, qipp.[Obesity_Somewhat_v2] 

FROM [StrategicAnalytics_QIPP].[Working].[vwQIPP_SUS_IP1415] qipp
	left outer join (
		select [CCGCode], [AgeGroup], [Gender], [ESP2013OverSubjectPop]
		from [StrategicAnalytics_QIPP].[Working].[vwESPOverSubjectPop]
		where 
			[PopulationType] = 'Resident'
			and [Year] = 2014
	) pop
		on qipp.AgeGroup5 = pop.AgeGroup
			and qipp.Gender = pop.Gender
			and qipp.CCGCode = pop.CCGCode 
GROUP BY
	[FYear]
    , qipp.[CCGCode]
	, pop.[ESP2013OverSubjectPop]
	, qipp.[ACS_Acute_v3]
	, qipp.[ACS_Chronic_v3]
	, qipp.[ACS_Vaccine_v3]
	, qipp.[Alcohol_25pcto75pc_v3]
	, qipp.[Alcohol_5pcto25pc_v3]
	, qipp.[Alcohol_75pcto100pc_v3]
	, qipp.[Canc_Op_v1]
	--, qipp.[EOLC_Long_v2]
	--, qipp.[EOLC_Short_v2]
	, qipp.[Falls_v1]
	, qipp.[FrailElderly_Occasional_v1]
	, qipp.[FrailElderly_Usual_v1]
	, qipp.[Med_Unexplained_v1]
	, qipp.[Meds_Explicit_v1]
	, qipp.[Meds_Implicit_AntiDiabetics_v1]
	, qipp.[Meds_Implicit_Benzo_v1]
	, qipp.[Meds_Implicit_Diuretics_v1]
	, qipp.[Meds_Implicit_NSAIDs_v1]
	-- , qipp.[Obesity_Large_v1]
	-- , qipp.[Obesity_Marginal_v1]
	-- , qipp.[Obesity_Somewhat_v1]
	, qipp.[PLCV_Cosmetic_v1]
	, qipp.[PLCV_Cost_v1]
	, qipp.[PLCV_Ineffective_v1]
	, qipp.[PLCV_Risks_v1]
	-- , qipp.[RAID_ED_v2]
	, qipp.[Readmissions_v1]
	, qipp.[SelfHarm_v2]
	, qipp.[Smoking_Large_v1]
	, qipp.[Smoking_Somewhat_v1]
	--, qipp.[ZeroLoS_NP_Adult_v2]
	--, qipp.[ZeroLoS_NP_Child_v2]
	, qipp.[EOLC_Long_v3]
	, qipp.[EOLC_Short_v3]
	, qipp.[RAID_ED_v3]	
	, qipp.[ZeroLoS_NP_Adult_v3]
	, qipp.[ZeroLoS_NP_Child_v3]
	, qipp.[Obesity_Large_v2]
	, qipp.[Obesity_Marginal_v2]
	, qipp.[Obesity_Somewhat_v2] 