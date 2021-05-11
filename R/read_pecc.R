#' Read PECC Grant Data
#'
#' Read PECC Grant Data from the shared Google Drive folder.
#'
#' @param filetype "rds" or "csv"
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble
#' @export

read_pecc <- function(filetype = "rds", gdrive_root = "~/Google Drive"){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/PECC Grant Data/data/pecc_grant.", filetype)
	path <- paste0(gdrive_root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 20000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}
	return(df)
}
