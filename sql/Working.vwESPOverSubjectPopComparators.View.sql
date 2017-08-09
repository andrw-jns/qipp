USE [StrategicAnalytics_QIPP]
GO

/****** Object:  View [Working].[vwESPOverSubjectPopComparators]    Script Date: 23/10/2015 16:07:07 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


ALTER VIEW [Working].[vwESPOverSubjectPopComparators] AS
-- Current status 23/10/2015 JS
-- Sums population for a CCG and it's 20 nearest neighbours
-- Assumes latest official estimates are from 2013.
-- Assumes 90 plus is the maximum age band.
-- Assumes 5 year age bands.
-- Only residential at the moment
SELECT 
	ccg.[CCGCode]
	, pop.[AgeGroup]
	, pop.[Gender]
	, pop.[Year]
	, esp.[ESP2013] / SUM(pop.[Population]) [ESP2013OverSubjectPop]
	, 'Resident' [PopulationType]
	, 'Estimate' [EstimateOrProjection]



FROM [BusinessAnalytics_Reference].[dbo].[tbCCGPopEstimates] pop
	left outer join [BusinessAnalytics_Reference].[dbo].tbESP esp
		on esp.AgeGroup = pop.AgeGroup
	left outer join (
		select distinct
			[CCGCode]
			, [CCGCode] [NeighbourCCGCode]
			, 0 [ClosenessRank]
		from 
			[StrategicAnalytics_QIPP].[Working].[vwNearestNeighbourCfV_Tall]
		
		union all
		
		select	
			[CCGCode]
		      ,[NeighbourCCGCode]
		      ,[ClosenessRank]
	
		from [StrategicAnalytics_QIPP].[Working].[vwNearestNeighbourCfV_Tall]
		
		where ClosenessRank <= 20
	) ccg
	on pop.CCGCode = ccg.NeighbourCCGCode
	
WHERE 
	(
		pop.AgeGroup like '[0-8][05]to[0-8][49]'
		or pop.AgeGroup like '90plus'
	)
	and pop.Gender != 'P'
	and pop.CCGCode is not null

GROUP BY
	ccg.[CCGCode]
	, pop.[AgeGroup]
	, pop.[Gender]
	, pop.[Year]
	, esp.ESP2013

UNION ALL


SELECT 
	pop.[CCGCode]
	, pop.[AgeGroup]
	, pop.[Gender]
	, pop.[Year]
	, esp.[ESP2013] / SUM(pop.[Population]) [ESP2013OverSubjectPop]
	, 'Resident' [PopulationType]
	, 'Projection' [EstimateOrProjection]


FROM [BusinessAnalytics_Reference].[dbo].[tbCCGPopProjections0514] pop
	left outer join [BusinessAnalytics_Reference].[dbo].tbESP esp
		on esp.AgeGroup = pop.AgeGroup
	left outer join (
		select distinct
			[CCGCode]
			, [CCGCode] [NeighbourCCGCode]
			, 0 [ClosenessRank]
		from 
			[StrategicAnalytics_QIPP].[Working].[vwNearestNeighbourCfV_Tall]
		
		union all
		
		select	
			[CCGCode]
		      ,[NeighbourCCGCode]
		      ,[ClosenessRank]
	
		from [StrategicAnalytics_QIPP].[Working].[vwNearestNeighbourCfV_Tall]
		
		where ClosenessRank <= 20
	) ccg
	on pop.CCGCode = ccg.NeighbourCCGCode
	
WHERE 
	(
		pop.AgeGroup = '00to04'
		or pop.AgeGroup = '05to09'
		or pop.AgeGroup = '10to14'
		or pop.AgeGroup = '15to19'
		or pop.AgeGroup = '20to24'
		or pop.AgeGroup = '25to29'
		or pop.AgeGroup = '30to34'
		or pop.AgeGroup = '35to39'
		or pop.AgeGroup = '40to44'
		or pop.AgeGroup = '45to49'
		or pop.AgeGroup = '50to54'
		or pop.AgeGroup = '55to59'
		or pop.AgeGroup = '60to64'
		or pop.AgeGroup = '65to69'
		or pop.AgeGroup = '70to74'
		or pop.AgeGroup = '75to79'
		or pop.AgeGroup = '80to84'
		or pop.AgeGroup = '85to89'
		or pop.AgeGroup like '90plus'
	)
	and pop.Gender != 'P'
	and pop.CCGCode is not null
	and pop.[Year] >= 2014

GROUP BY
	pop.[CCGCode]
	, pop.[AgeGroup]
	, pop.[Gender]
	, pop.[Year]
	, esp.ESP2013

GO


