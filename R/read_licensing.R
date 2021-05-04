#' Read DHS Licensing Data
#'
#' Read DHS Licensing Data that lives in the shared Google Drive folder.
#'
#'
#' @param table character
#' @param version character
#' @param filetype character
#'
#' @return tibble

read_licensing <- function(table = "provider", version = "2021-04", filetype = "csv"){
	if(!table %in% c("provider", "shift")){
		stop("Table does not exist.")
	}
	if(!filetype %in% c("csv", "rds", ".csv", ".rds")){
		stop("File type does not exist.")
	}

	if(filetype == ".csv") filetype = "csv"
	if(filetype == ".rds") filetype = "rds"

	root <- "~/Google Drive/MinnCCAccess/Data Cabinet/Licensing Data/"
	subpath <- paste0("Licensing Data ", version, " Build/data/licensing_panel_", table, ".", filetype)

	path <- paste0(root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 2000000)
		df <- dplyr::mutate(df, license_id = as.character(license_id))
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}
	return(df)

}
