SET NOCOUNT ON
GO
SELECT 
	(COUNT([attendkey]) * [ESP2013OverSubjectPop]) / 2 as [DSRate]
	, (COUNT([attendkey]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSRateVar]
	, (SUM([Cost]) * [ESP2013OverSubjectPop]) / 2 as [DSCosts]
	, (SUM([Cost]) * POWER([ESP2013OverSubjectPop], 2)) / 2 as [DSCostsVar]
	, COUNT([attendkey]) [Attendances]
    , SUM([Cost]) [Costs]

	, [FYear]
	, [Cons_Ref_v1]
	, [FUF_NonSurgAdult_v1]
	, [FUF_NonSurgChild_v1]
	, [FUF_SurgAdult_v1]
	, [FUF_SurgChild_v1]
	, [GPRef_NonSurgAdult_v1]
	, [GPRef_NonSurgChild_v1]
	, [GPRef_SurgAdult_v1]
	, [GPRef_SurgChild_v1]
	, [PLCV_v1]

FROM [StrategicAnalytics_QIPP].[Working].[vwQIPP_HES_OP1516] qipp
	left outer join (


		select pop.AgeGroup, pop.Gender, esp.[ESP2013] / pop.[Population] [ESP2013OverSubjectPop]
		from 

		-- England 2016 population projection (2014-based)
		--https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/2014basednationalpopulationprojectionstableofcontents
		(VALUES ('00to04','M',1749110)	,('00to04','F', 1662962)
		,('05to09','M', 1750231)	,('05to09','F', 1666421)
		,('10to14','M', 1565304)	,('10to14','F', 1492664)
		,('15to19','M', 1622844)	,('15to19','F', 1540617)
		,('20to24','M', 1835846)	,('20to24','F', 1747919)
		,('25to29','M', 1939518)	,('25to29','F', 1897998)
		,('30to34','M', 1871837)	,('30to34','F', 1878146)
		,('35to39','M', 1770465)	,('35to39','F', 1779659)
		,('40to44','M', 1748565)	,('40to44','F', 1773748)
		,('45to49','M', 1913717)	,('45to49','F', 1958969)
		,('50to54','M', 1905555)	,('50to54','F', 1958966)
		,('55to59','M', 1666798)	,('55to59','F', 1705279)
		,('60to64','M', 1433266)	,('60to64','F', 1490993)
		,('65to69','M', 1470155)	,('65to69','F', 1559879)
		,('70to74','M', 1136597)	,('70to74','F', 1243585)
		,('75to79','M',  829374)	,('75to79','F', 969004)
		,('80to84','M',  586675)	,('80to84','F', 762144)
		,('85to89','M',  326195)	,('85to89','F', 518467)
		,('90plus','M',  147681)	,('90plus','F', 341548)) as pop(AgeGroup,Gender,Population)

		-- OLD ESTIMATES FOR 14/15
		-- England 2014 population projection (2012-based)
		--http://www.ons.gov.uk/ons/rel/npp/national-population-projections/2012-based-projections/rft-table-a2-4-principal-projection---england-population-in-age-groups.xls
		
		--(VALUES ('00to04','M',1757150)	,('00to04','F',1673954)
		--,('05to09','M',1662358)	,('05to09','F',1586159)
		--,('10to14','M',1511230)	,('10to14','F',1439512)
		--,('15to19','M',1654400)	,('15to19','F',1568490)
		--,('20to24','M',1849065)	,('20to24','F',1779037)
		--,('25to29','M',1875468)	,('25to29','F',1857620)
		--,('30to34','M',1844554)	,('30to34','F',1860642)
		--,('35to39','M',1691701)	,('35to39','F',1694615)
		--,('40to44','M',1827543)	,('40to44','F',1863508)
		--,('45to49','M',1930428)	,('45to49','F',1977081)
		--,('50to54','M',1835515)	,('50to54','F',1873244)
		--,('55to59','M',1571204)	,('55to59','F',1606007)
		--,('60to64','M',1423615)	,('60to64','F',1482882)
		--,('65to69','M',1444014)	,('65to69','F',1525273)
		--,('70to74','M',1039557)	,('70to74','F',1144890)
		--,('75to79','M',818952)	,('75to79','F',963203)
		--,('80to84','M',561511)	,('80to84','F',753283)
		--,('85to89','M',304004)	,('85to89','F',502519)
		--,('90plus','M',136902)	,('90plus','F',336769)) as pop(AgeGroup,Gender,Population)

		inner join [StrategicReference].[dbo].tbESP esp -- database has changed to Strategic Ref.
					on esp.AgeGroup = pop.AgeGroup
	) pop
		on qipp.AgeGroup5 = pop.AgeGroup
			and qipp.Gender = pop.Gender
GROUP BY
	[FYear]
	, pop.[ESP2013OverSubjectPop]
	, [Cons_Ref_v1]
	, [FUF_NonSurgAdult_v1]
	, [FUF_NonSurgChild_v1]
	, [FUF_SurgAdult_v1]
	, [FUF_SurgChild_v1]
	, [GPRef_NonSurgAdult_v1]
	, [GPRef_NonSurgChild_v1]
	, [GPRef_SurgAdult_v1]
	, [GPRef_SurgChild_v1]
	, [PLCV_v1]