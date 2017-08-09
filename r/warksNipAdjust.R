#warks north quick fix
ipData <- ipData %>%
  select(-EOLC_Long_v2, -EOLC_Short_v2
	, -RAID_ED_v2
	, -ZeroLoS_NP_Adult_v2
	, -ZeroLoS_NP_Child_v2
	, -Obesity_Large_v1
	, -Obesity_Marginal_v1
	, -Obesity_Somewhat_v1
) %>%
  rename(
  EOLC_Long_v2 = EOLC_Long_v3
  , EOLC_Short_v2 = EOLC_Short_v3
  , RAID_ED_v2 = RAID_ED_v3
  , ZeroLoS_NP_Adult_v2 = ZeroLoS_NP_Adult_v3
  , ZeroLoS_NP_Child_v2 = ZeroLoS_NP_Child_v3
  , Obesity_Large_v1 = Obesity_Large_v2
  , Obesity_Marginal_v1 = Obesity_Marginal_v2
  , Obesity_Somewhat_v1 = Obesity_Somewhat_v2
)