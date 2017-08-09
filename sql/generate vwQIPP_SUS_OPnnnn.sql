--generating vwQIPP_HES_OPnnnn

USE [StrategicAnalytics_QIPP]
GO

DECLARE @SQL nvarchar(max)
Execute [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_SUS_OP1617'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2016-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2017-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'SUS'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'OP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= '
	, op.EpisodeId AS [encrypted_hesid]
	, op.SUSGeneratedIdentifier AS [attendkey]
    , op.[Cost] 
    , CASE
		 WHEN op.[Age] > 89
			 THEN ''90plus''
		 ELSE RIGHT(''00'' + ISNULL(CAST(FLOOR(op.[Age] / 5) * 5 as nvarchar(2)), ''''), 2) + 
			 ''to'' + RIGHT(''00'' + ISNULL(CAST((FLOOR(op.[Age] / 5) * 5) + 4 as nvarchar(2)), ''''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(OP.GenderDescription, 1) as [Gender]
	'

	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  



    
DECLARE @SQL nvarchar(max)
Execute [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_SUS_OP1516'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2015-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2016-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'SUS'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'OP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= '
	, op.EpisodeId AS [encrypted_hesid]
	, op.SUSGeneratedIdentifier AS [attendkey]
    , op.[Cost] 
    , CASE
		 WHEN op.[Age] > 89
			 THEN ''90plus''
		 ELSE RIGHT(''00'' + ISNULL(CAST(FLOOR(op.[Age] / 5) * 5 as nvarchar(2)), ''''), 2) + 
			 ''to'' + RIGHT(''00'' + ISNULL(CAST((FLOOR(op.[Age] / 5) * 5) + 4 as nvarchar(2)), ''''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(OP.GenderDescription, 1) as [Gender]
	'

	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  


    
DECLARE @SQL nvarchar(max)
Execute [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_SUS_OP1415'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2014-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2015-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'SUS'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'OP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= '
	, op.EpisodeId AS [encrypted_hesid]
	, op.SUSGeneratedIdentifier AS [attendkey]
    , op.[Cost] 
    , CASE
		 WHEN op.[Age] > 89
			 THEN ''90plus''
		 ELSE RIGHT(''00'' + ISNULL(CAST(FLOOR(op.[Age] / 5) * 5 as nvarchar(2)), ''''), 2) + 
			 ''to'' + RIGHT(''00'' + ISNULL(CAST((FLOOR(op.[Age] / 5) * 5) + 4 as nvarchar(2)), ''''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(OP.GenderDescription, 1) as [Gender]
	'

	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  



    
DECLARE @SQL nvarchar(max)
Execute [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_SUS_OP1314'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2013-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2014-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'SUS'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'OP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= '
	, op.EpisodeId AS [encrypted_hesid]
	, op.SUSGeneratedIdentifier AS [attendkey]
    , op.[Cost] 
    , CASE
		 WHEN op.[Age] > 89
			 THEN ''90plus''
		 ELSE RIGHT(''00'' + ISNULL(CAST(FLOOR(op.[Age] / 5) * 5 as nvarchar(2)), ''''), 2) + 
			 ''to'' + RIGHT(''00'' + ISNULL(CAST((FLOOR(op.[Age] / 5) * 5) + 4 as nvarchar(2)), ''''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(OP.GenderDescription, 1) as [Gender]
	'

	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  



    
DECLARE @SQL nvarchar(max)
Execute [Working].[sp_QuincyAdHoc]
      @SQL 						output 	
	, @objectSchema				= 'Working'		--The schema of the output object name
	, @objectName				= 'vwQIPP_SUS_OP1213'			-- A suffix to add to the view where you want the output to go. NULL assumes you don't want to create an object.					
    , @DateFrom 				= '2012-04-01' 	-- The dates you want determine the tables to be queried
    , @DateTo 					= '2013-03-31'	-- The dates you want determine the tables to be queried
	, @dataSourceIN 			= 'SUS'			-- Indicates the database that will be used (options: 'SUS', 'HES', 'PbR')
	, @tableTypeIn 				= 'OP'			-- The type of table that you want, e.g. 'IP', 'OP', 'AE' 
	, @variableType 			= '1'			-- Where do you want the variables to be in your query 1 = case, 2 = sum(case), 3 = Where (multiple strategies separated by AND), 4 = Where (multiple strategies separated by OR)
	, @adHocAggregate 			= ''			-- Custom aggregates to be included in select
	, @adHocSelect				= '
	, op.EpisodeId AS [encrypted_hesid]
	, op.SUSGeneratedIdentifier AS [attendkey]
    , op.[Cost] 
    , CASE
		 WHEN op.[Age] > 89
			 THEN ''90plus''
		 ELSE RIGHT(''00'' + ISNULL(CAST(FLOOR(op.[Age] / 5) * 5 as nvarchar(2)), ''''), 2) + 
			 ''to'' + RIGHT(''00'' + ISNULL(CAST((FLOOR(op.[Age] / 5) * 5) + 4 as nvarchar(2)), ''''), 2) 
		 END as [AgeGroup5]
    , LEFT(CCGCode, 3) [CCGCode]
	, LEFT(OP.GenderDescription, 1) as [Gender]
	'

	, @adHocWhere				= ''			-- Custom where clause
	, @outputType				= '1'			-- Irrelevant unless @objectName is non-null. 1 = View, 2 = Insert into
	, @includeAggregates		= '0'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbAggregates)
	, @includeDescriptors		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbDescriptors)
	, @includeStrategies		= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeStrategiesAdHoc	= '1'			-- 0 or NULL = Exclude, 1 = Include (from Input.tbStrategies where isAdHoc = '0')
	, @includeFYearColumn		= '1'			-- 0 or NULL = Exclude, 1 = Include
	, @debug					= '0'			-- If 0, execute the SQL. If 1, print the SQL to the console.  