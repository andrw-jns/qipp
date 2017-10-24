SET NOCOUNT ON
GO
SELECT 
	[CCGCode]
    , [Population]

FROM [StrategicReference].[dbo].[tbCCGPopProjections0516]

WHERE
	[Gender] = 'P' 
	and [AgeGroup] = 'AllAges' 
	and [Year] = 2016

ORDER BY CCGCode