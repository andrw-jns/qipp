USE [StrategicAnalytics_QIPP]
GO

/****** Object:  View [Working].[vwNearestNeighbourCfV_Tall_Labels]    Script Date: 26/01/2017 12:56:49 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO


CREATE VIEW [Working].[vwNearestNeighbourCfV_Tall_Labels] AS
SELECT 
	nn.[CCGCode]
	, base.[CCGName]
	, [NeighbourCCGCode]
	, neighbour.[CCGName] [NeighbourCCGName]
	, [ClosenessRank]

FROM [Working].vwNearestNeighbourCfV_Tall nn
	left outer join [StrategicReference].dbo.tbCCGIndex base
		on nn.CCGCode = base.CCGCode
	left outer join [StrategicReference].dbo.tbCCGIndex neighbour
		on nn.NeighbourCCGCode = neighbour.CCGCode

GO


