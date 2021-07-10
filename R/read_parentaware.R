#' Read Parent Aware Data
#'
#' Read Parent Aware Data from the shared Google Drive folder.
#'
#' @importFrom magrittr %>%
#' @param filetype "rds" or "csv". Defaults to "rds".
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder. Defaults to "~/Google Drive".
#'
#'
#' @return A tibble
#'
#' @export

read_parentaware <- function(filetype = "rds", gdrive_root = "~/Google Drive"){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Parent Aware Data/data/parent_aware_rating_panel", ".", filetype)
	path <- paste0(gdrive_root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 4000000)
	} else if(filetype == "rds"){
		df <- readr::read_rds(path)
	}

	df

}
