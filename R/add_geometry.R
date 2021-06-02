#' Add geometries to Census data frames
#'
#' Add geometries to Census data frames that are imported using other functions in this package.
#'
#' @importFrom magrittr %>%

#' @param data Census data to attach to shapefiles
#' @param geography Geographic unit  of the geometry
#' @param year Vintage of the geographic unit. Valid inputs depend on what the geography is.
#' @param by_name TRUE to use county name for matching
#'
#' @return An sf object
#' @export




add_geometry <- function(data, geography, year = NULL, by_name = FALSE){

	shapes <- import_geometry(geography = geography, year = year)

	n1 <- nrow(shapes)
	n2 <- nrow(data)

	if(n2 > n1){
		warning(paste0(n2 - n1, " rows dropped due to non-matching geography."))
	}

	geography_var <- names(shapes)[1]

	if(by_name & geography == "county"){
		geography_var <- names(shapes)[2]
		shapes <- shapes[, c(2, 1)]
	}

	shapes %>%
		dplyr::left_join(data, by = geography_var)

}
