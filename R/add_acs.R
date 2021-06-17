#' Add ACS variables

#' Add ACS variables to data frames.

#' @importFrom magrittr %>%
#'
#' @param data Data to add ACS variables to
#' @param variable ACS variable(s) to add
#' @param geography Geographic unit
#' @param year Vintage
#' @param by_name TRUE to use county name for matching
#' @param CENSUS_API_KEY Census API key
#'
#'
#' @export


add_acs <- function(data, variable, geography, year = 2019, by_name = FALSE, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){

	acs_df <- get_acs(variable, geography, year, CENSUS_API_KEY)

	geography_var <- names(acs_df)[1]

	if(by_name & geography == "county"){
		geography_var <- names(acs_df)[2]
	}

	data %>%
		dplyr::inner_join(acs_df, by = geography_var)


}
