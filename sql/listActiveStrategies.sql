-- Get the names and details of all of the active strategies

--Powershell:
--$file = "listActiveStrategies"
--archive $file "Data"
--archive $file "SQL" # if necessary
--runSQL $file
SET NOCOUNT ON
GO

SELECT 
	[StrategyID]
    , REPLACE([Breakdown], '%', 'pc') + + N'_v' + CAST([Version] as nvarchar) 'Strategy'
	, QUOTENAME([StrategyDescription], '"') 'StrategyDescription'
    , [TableType]
    , [StrategyType]

FROM 
	[StrategicAnalytics_QIPP].[Input].[tbStrategies]

WHERE
	IsQIPP = '1'