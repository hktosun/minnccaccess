#' Read PECC Grant Data
#'
#' Read PECC Grant Data from the shared Google Drive folder.
#'
#' @param filetype "rds" or "csv"
#' @param GDRIVE_ROOT The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble
#' @export

read_pecc <- function(filetype = "rds", GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/PECC Grant Data/data/pecc_grant.", filetype)
	path <- paste0(GDRIVE_ROOT, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 20000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}
	return(df)
}
