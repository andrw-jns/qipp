USE [StrategicAnalytics_QIPP]
GO
--Make sure that the right things are active in the descriptors and strategies tables.
update [StrategicAnalytics_QIPP].[Input].[tbDescriptors]
set IsActive = CASE WHEN DescriptorId IN (8, 58) THEN 1 ELSE 0 END -- AgeGroup5, Gender
update [StrategicAnalytics_QIPP].[Input].[tbStrategies]
set IsActive = IsQIPP
GO

-- Remove existing views

--DROP VIEW [Working].[vwQIPP_PbR_IP1415]
--DROP VIEW [Working].[vwQIPP_PbR_IP1314]
--DROP VIEW [Working].[vwQIPP_PbR_IP1213]
--DROP VIEW [Working].[vwQIPP_PbR_IP1112]
--DROP VIEW [Working].[vwQIPP_PbR_IP1011]
--DROP VIEW [Working].[vwQIPP_PbR_IP0910]

GO
--Generate the new views
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP1415'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2016-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2017-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '1'			-- If 0, execute the SQL. If 1, print the SQL to the console.  
	
	
GO
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP1314'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2013-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2014-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  
	
	
GO
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP1213'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2012-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2013-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  

	
GO
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP1112'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2011-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2012-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  
	
	
GO
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP1011'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2010-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2011-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  
	
	
GO
DECLARE @SQL nvarchar(max)

EXECUTE [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_PbR_IP0910'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2009-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2010-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'PbR'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'IP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= 'pbrS.SPELL_ID
	, pbrS.[CCG_CODE_D] [CCGCode]
	, CAST(pbrS.[PBR_FINAL_TARIFF] AS integer) [Cost]'			-- Custom select clause
	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  