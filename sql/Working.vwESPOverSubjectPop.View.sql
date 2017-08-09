USE [StrategicAnalytics_QIPP]
GO

/****** Object:  View [Working].[vwESPOverSubjectPop]    Script Date: 27/03/2017 12:56:00 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





ALTER VIEW [Working].[vwESPOverSubjectPop] AS
-- Current status 16/03/2017 AJ (BASED ON JS ORIGINAL)
-- Assumes latest official estimates are from 2015.
-- Assumes 90 plus is the maximum age band.
-- Assumes 5 year age bands.
-- Only residential at the moment
		select
			CCGCode 
			, pop.AgeGroup
			, Gender
			, [Year]
			, esp.[ESP2013] / pop.[Population] as [ESP2013OverSubjectPop]
			, 'Resident' [PopulationType]
			, 'Estimate' [EstimateOrProjection]

		from 
			[StrategicReference].[dbo].[tbCCGPopEstimates_UPDATED_2017_03] pop
				inner join [StrategicReference].[dbo].tbESP esp
					on esp.AgeGroup = pop.AgeGroup collate database_default
		where 
		    (
                  pop.AgeGroup like '[0-8][05]to[0-8][49]'
			or pop.AgeGroup like '90plus'
		    )
		    and Gender != 'P'
		    and CCGCode is not null 
		
		union all
		select
			CCGCode COLLATE database_default as CCGCode
			, pop.AgeGroup COLLATE database_default as AgeGroup
			, Gender COLLATE database_default as Gender
			, [Year]
			, esp.[ESP2013] / pop.[Population] as [ESP2013OverSubjectPop]
			, 'Resident' [PopulationType]
			, 'Projection' [EstimateOrProjection] 

		from 
			[StrategicReference].[dbo].[tbCCGPopProjections0516] pop
				inner join [StrategicReference].[dbo].tbESP esp
					on esp.AgeGroup = pop.AgeGroup  COLLATE database_default 
		where 
		    (
                  pop.AgeGroup like '[0-8][05]to[0-8][49]'
			or pop.AgeGroup like '90plus'
		    )
			and [Year] >= 2016
		    and Gender != 'P'
		    and CCGCode is not null 



GO


