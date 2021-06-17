#' Create family access map
#'
#' Create adjusted-supply family access map
#'
#' @importFrom magrittr %>%
#'
#' @param geography Census geography to get the centroid of
#' @param as_of Access as of date
#' @param access_to Either licensed providers or all providers in Nware. Default is licensed providers
#' @param radius Consider providers/families that are in `radius` miles or closer.
#' @param acs_year ACS vintage for demographic variables
#' @param kid_per_dot Number of kids per dot on the map
#' @param decay Decay parameter (used to weight access by distance) in calculating the access measure
#'
#' @return An tmap object (html)
#' @export


create_access_map <- function(geography = "census-tract", as_of = "2021-04-01", access_to = "licensed_providers", radius = 10, acs_year = 2019, kid_per_dot = 100,
							  decay = 4){

	measure_access(geography = geography,
						 year = 2010,
						 as_of = as_of,
						 acs_year = acs_year,
						 radius = radius,
						 decay = decay) %>%
		simulate_families(kid_per_dot = kid_per_dot) %>%
		plot_map(fill_with = kid_per_dot, palette = c("red", "white", "blue"), n = 5, alpha = 0.5)

}
