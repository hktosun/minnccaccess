#' Read Parent Aware Data
#'
#' Read Parent Aware Data from the shared Google Drive folder.
#'
#' @importFrom magrittr %>%
#' @param filetype "rds" or "csv". Defaults to "rds".
#' @param GDRIVE_ROOT The local path to the folder that contains the MinnCCAccess folder.
#'
#'
#' @return A tibble
#'
#' @export

read_parentaware <- function(filetype = "rds", GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Parent Aware Data/data/parent_aware_rating_panel", ".", filetype)
	path <- paste0(GDRIVE_ROOT, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 4000000)
	} else if(filetype == "rds"){
		df <- readr::read_rds(path)
	}

	df

}
