#' Add geographies
#'
#' Add administrative and other geographies into datasets.
#'

#' @param data A tibble with address fields `street`, `city`, `state`.
#' @param geography Geographies to add to. Defaults to all geographies available.
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble
#' @export
#'
#'
#'

add_locs <- function(data, geography, gdrive_root = "~/Google Drive"){
	subpath <- "/MinnCCAccess/Data Cabinet/Geographic Data/data/address_geocodes.csv"

	path <- paste0(gdrive_root, subpath)
	locs <- readr::read_csv(path, col_types = readr::cols(.default = readr::col_character()))

	if(!missing(geography)){
		locs <- locs %>%
			dplyr::select(.data$street, .data$city, .data$state, tidyselect::all_of(geography))
	}

	data <- dplyr::left_join(data, locs, by = c("street", "city", "state"))

	return(data)
}
