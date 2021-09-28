#' Read DHS Licensing Data
#'
#' Read DHS Licensing Data from the shared Google Drive folder.
#'
#'
#' @param table Licensing table to read. Either "provider", or "shift".
#' @param version Licensing data version. Either "2021-04" or "2021-02".
#' @param filetype Data file type. Either "csv" or "rds".
#' @param filled Missing variables filled in using other data sources (like Nware) or imputation.
#' @param GDRIVE_ROOT The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble of Licensing data.
#' @export

read_licensing <- function(table = "provider", version, filetype = "rds", filled = FALSE, GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){
	if(!table %in% c("provider", "shift")){
		stop("Table does not exist.")
	}
	if(!filetype %in% c("csv", "rds", ".csv", ".rds")){
		stop("File type does not exist.")
	}

	if(filled){
		subpath <- "/MinnCCAccess/Data Cabinet/Custom Data Products/For Hasan - 2021-09-26/data."
		path <- paste0(GDRIVE_ROOT, subpath, filetype)
		if(filetype == "rds") df <- readr::read_rds(path)
		if(filetype == "csv") df <- readr::read_csv(path)
	} else {

		if(filetype == ".csv") filetype = "csv"
		if(filetype == ".rds") filetype = "rds"


		subpath <- paste0("/MinnCCAccess/Data Cabinet/Licensing Data/Licensing Data ", version, " Build/data/licensing_panel_", table, ".", filetype)

		path <- paste0(GDRIVE_ROOT, subpath)

		if(filetype == "csv"){
			df <- readr::read_csv(path, guess_max = 2000000)
			df <- dplyr::mutate(df, license_id = as.character(.data$license_id))
		}
		if(filetype == "rds"){
			df <- readr::read_rds(path)
		}
	}
	df

}
