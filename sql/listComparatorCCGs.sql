SET NOCOUNT ON
GO

SELECT 
	[CCGCode]
    , [NeighbourCCGCode]
    , [ClosenessRank]

FROM [StrategicAnalytics_QIPP].[Working].[vwNearestNeighbourCfV_Tall]
WHERE 
	ClosenessRank <= 20

	