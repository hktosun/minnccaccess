#' Add geometries to Census data frames
#'
#' Add geometries to Census data frames that are imported using other functions in this package.
#'
#' @importFrom magrittr %>%

#' @param data Census data to attach to shapefiles
#' @param geography Geographic unit  of the geometry
#' @param year Vintage of the geographic unit. Valid inputs depend on what the geography is.
#'
#' @return An sf object
#' @export




add_geometry <- function(data, geography, year = NULL){

	shapes <- import_geometry(geography = geography, year = year)

	n1 <- nrow(shapes)
	n2 <- nrow(data)

	if(n2 > n1){
		warning(paste0(n2 - n1, " rows dropped due to non-matching geography."))
	}

	shapes %>%
		dplyr::left_join(data)

}
