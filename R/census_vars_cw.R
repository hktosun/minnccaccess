# census_vars <- tibble::tribble(~variable_cat, ~variable_code, ~variable_label,
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
acs_vars <- tibble::tribble(
		~variable_cat, ~variable_code, ~variable_label,
		"kids", "B06001_002E", "population_under5",
		"median income", "B06011_001E", "median_income",
		"preschool enrollment", "B14001_003E", "preschool_enrollment",
		"public preschool enrollment", "B14003_004E", "public_preschool_enrollment")

use_data(census_vars, acs_vars, internal = TRUE, overwrite = TRUE)

B19101_001E
