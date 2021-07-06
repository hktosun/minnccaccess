#' Create access data using hexagons
#'
#' Create child care access data using hexagons
#'
#' @importFrom magrittr %>%
#' @param method "hexagon" or "census"
#' @param geography Geography
#' @param geo_year Geography year. 2010 or 2020
#' @param acs_year 5-year ACS end year
#' @param hex_side Length of side of hexagons
#' @param radius Consider providers/families that are in `radius` miles or closer.
#' @param decay Decay parameter (used to weight access by distance) in calculating the access measure
#' @param as_of Access as of date
#'
#' @return An sf object
#'
#' @export
#'


create_access <- function(method = "hexagon", geography = "census-block-group", geo_year = 2010, acs_year = 2019, hex_side = 1, radius = 10, decay = 4, as_of = "2021-04-01"){

	if(!method %in% c("hexagon", "census")){
		stop('`method` should be "hexagon" or "census"')
	}
	if(method == "hexagon"){
		access_data <- create_access_hexagon(geography = geography, geo_year = geo_year, acs_year = acs_year, hex_side = hex_side, radius = radius, decay = decay, as_of = as_of)
	} else if(method == "census"){
		access_data <- create_access_census(geography = geography, geo_year = geo_year, acs_year = acs_year, radius = radius, decay = decay, as_of = as_of)
	}

	access_data

}


