#' Add geographies
#'
#' Add administrative and other geographies into datasets.
#'

#' @param data A tibble with address fields `street`, `city`, `state`.
#' @param geography Geographies to add to. Defaults to all geographies available.
#' @param year Vintage for the geography
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble
#' @export
#'
#'
#'

add_locs <- function(data, geography, year = NULL, gdrive_root = "~/Google Drive"){

	subpath <- "/MinnCCAccess/Data Cabinet/Geographic Data/data/address_geocodes.csv"

	path <- paste0(gdrive_root, subpath)
	locs <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))


	if(missing(geography)){
		locs <- locs %>%
			dplyr::select(.data$street, .data$city, .data$state, .data$lat, .data$lon)
	} else {
		if(geography == "county"){
			geographic_var <- "gis_county"
		} else {
			geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", year)
		}

		locs <- locs %>%
			dplyr::select(.data$street, .data$city, .data$state, .data$lat, .data$lon, tidyselect::all_of(geographic_var))
	}

	data <- dplyr::left_join(data, locs, by = c("street", "city", "state"))

	return(data)
}
