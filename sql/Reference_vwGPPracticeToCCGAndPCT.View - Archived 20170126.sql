USE [StrategicReference]
GO

/****** Object:  View [dbo].[vwGPPracticeToCCGAndPCT]    Script Date: 26/01/2017 10:32:14 ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE VIEW [dbo].[vwGPPracticeToCCGAndPCT]	AS
-- Created by JS 17/06/2015
SELECT 
	gp.OrganisationCode				'GPPractice'
	, LEFT(ccg.OrganisationCode, 3)	'CCGCode'
    , LEFT(pct.OrganisationCode, 3)	'PCTCode'
	, 'Direct'						'Method'
  
FROM 
	[Reference].[Organisation].[tbCCGHierarchy] hierarchy
	left outer join [Reference].[Organisation].[tbOrganisation] gp
		on hierarchy.GPPracticeId = gp.OrganisationId
	left outer join [Reference].[Organisation].[tbOrganisation] ccg
		on hierarchy.CCGId = ccg.OrganisationId
	left outer join [Reference].[Organisation].[tbOrganisation] pct
		on hierarchy.pctId = pct.OrganisationId

UNION ALL

SELECT 
	org.OrganisationCode
	,  pct 'CCGCode'
	,	NULL 'PCTCode'
	, 'PostcodeOfPractice' 'Method'
	
FROM
	[Reference].[Organisation].[tbOrganisation] org
	left outer join [Reference].[Organisation].[tbCCGHierarchy] hierarchy
		on hierarchy.GPPracticeId = org.OrganisationId
	left outer join [Reference].[Postcode].[tbNHSPD] nhspd
		on REPLACE(org.Postcode, ' ', '') = nhspd.PostCodeStripped
	left outer join [Reference].[Organisation].[tbOrganisation] org2
		on nhspd.PCT + '00' = org2.OrganisationCode

WHERE 
	org.TypeId = 3
	and hierarchy.GPPracticeId is null
	and org.Postcode is not null
	-- You can add some more restrictions to make sure that this second match only returns CCGs, 
	-- but I think that it is more helpful to leave it as it is.
	--and (org2.TypeId = 31 --CCGs
	--	or org2.TypeId = 23 -- Welsh LHBs
	--	or org2.TypeId = 1	-- Channel Islands and maybe some other stuff
	--)


GO


