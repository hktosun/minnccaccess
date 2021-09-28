#' Read Nware Data
#'
#' Read Nware Data from the shared Google Drive folder.
#'
#'
#' @param table Nware table to read. Either "characteristics", "provider_schedule_daily",  "provider_schedule_hourly", "shift", or "age_group_shift".
#' @param version Nware version. Either "2021-04" or "2020-10".
#' @param filetype Data file type. Either "csv" or "rds".
#' @param GDRIVE_ROOT The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return A tibble of Nware data.
#' @export

read_nware <- function(table = "characteristics", version, filetype = "rds", GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){

	if(!table %in% c("characteristics", "provider_schedule_daily", "provider_schedule_hourly", "shift", "age_group_shift",
					 "characteristics_before_2012", "provider_schedule_daily_before_2012", "provider_schedule_hourly_before_2012", "shift_before_2012", "sge_group_shift_before_2012")){
		stop("Table does not exist.")
	}
	if(!filetype %in% c("csv", "rds", ".csv", ".rds")){
		stop("File type does not exist.")
	}

	if(filetype == ".csv") filetype = "csv"
	if(filetype == ".rds") filetype = "rds"


	subpath <- paste0("/MinnCCAccess/Data Cabinet/Nware Data/Nware Data ", version, " Build/data/", table, ".", filetype)

	path <- paste0(GDRIVE_ROOT, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 1000000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}
	return(df)

}
