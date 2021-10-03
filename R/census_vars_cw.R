# census_vars <- tibble::tribble(
# 	~variable_cat, ~variable_code, ~variable_label,
# 		"population", "P001001", "population",
# 		"race", "P006002", "white",
# 		"race", "P006003", "black",
# 		"race", "P006004", "aian",
# 		"race", "P006005", "asian",
# 		"race", "P006006",  "nhpi",
# 		"race", "P006007", "other",
# 		"kids", "P012003", "under5_male",
# 		"kids", "P012027", "under5_female")
#
# acs_vars <- tibble::tribble(
# 		~variable_cat, ~variable_code, ~variable_label,
# 		"kids", "B01001_003E", "under5_boys",
# 		"kids", "B01001_027E", "under5_girls",
# 		"median income", "B06011_001E", "median_income",
# 		"preschool enrollment", "B14001_003E", "preschool_enrollment",
# 		"public preschool enrollment", "B14003_004E", "public_preschool_enrollment",
# 		"family income distribution", "B19101_002E", "family_income_0_10",
# 		"family income distribution", "B19101_003E", "family_income_10_15",
# 		"family income distribution", "B19101_004E", "family_income_15_20",
# 		"family income distribution", "B19101_005E", "family_income_20_25",
# 		"family income distribution", "B19101_006E", "family_income_25_30",
# 		"family income distribution", "B19101_007E", "family_income_30_35",
# 		"family income distribution", "B19101_008E", "family_income_35_40",
# 		"family income distribution", "B19101_009E", "family_income_40_45",
# 		"family income distribution", "B19101_010E", "family_income_45_50",
# 		"family income distribution", "B19101_011E", "family_income_50_60",
# 		"family income distribution", "B19101_012E", "family_income_60_75",
# 		"family income distribution", "B19101_013E", "family_income_75_100",
# 		"family income distribution", "B19101_014E", "family_income_100_125",
# 		"family income distribution", "B19101_015E", "family_income_125_150",
# 		"family income distribution", "B19101_016E", "family_income_150_200",
# 		"family income distribution", "B19101_017E", "family_income_200_300")
#
# usethis::use_data(census_vars, acs_vars, internal = TRUE, overwrite = TRUE)
