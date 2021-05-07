#' Add geographies
#'
#' Add administrative and other geographies into datasets.
#'

#' @param data A tibble with address fields `street`, `city`, `state`.
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble
#' @export
#'
#'
#'

add_locs <- function(data, gdrive_root = "~/Google Drive"){
	subpath <- "/MinnCCAccess/Data Cabinet/Geographic Data/data/address_geocodes.csv"

	path <- paste0(gdrive_root, subpath)
	locs <- readr::read_csv(path)

	data <- dplyr::left_join(data, locs, by = c("street", "city", "state"))

	return(data)
}
