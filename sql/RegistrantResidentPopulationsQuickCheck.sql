-- Quick check CCG registrant/resident populations
select 
	res.CCGCode
	, res.ONSCode
	, res.CCGDescription
	, res.[Population] [ResidentPopulation]
	, reg.[Population] [RegistrantPopulation]
	, reg.[Population] / res.[Population]

from (  SELECT [CCGCode]
      ,[ONSCode]
      ,[CCGDescription]
      ,[AgeGroup]
      ,[Gender]
      ,[Year]
      ,[Population]
  FROM [StrategicReference].[dbo].[tbCCGPopEstimates]
  where [Year] = 2013   and AgeGroup = 'AllAges' and Gender = 'p') res
  left outer join (
	SELECT [Date]
	      ,[CCGCode]
	      ,[Gender]
	      ,[AgeGroup]
	      ,[Population]
	  FROM [StrategicReference].[dbo].[vwCCGRegisteredGPPopulation]
	  where Gender = 'p' and AgeGroup = 'AllAges' AND [Date] = '2013-07-01'
	) reg on res.CCGCode = reg.CCGCode

  
  where res.CCGCode like '02[NRW]%'
  order by CCGCode