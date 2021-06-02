#' Add Census variables

#' Add Census variables to data frames.

#' @importFrom magrittr %>%
#'
#' @param data Data to add Census variables to
#' @param variable Census variable to add
#' @param geography Geographic unit
#' @param year Vintage
#' @param CENSUS_API_KEY Census API key
#'


add_census <- function(data, variable, geography, year = 2010, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){

	census_df <- get_census(variable, geography, year, CENSUS_API_KEY)

	geography_var <- names(census_df)[1]

	data %>%
		left_join(census_df, by = geography_var)


}
