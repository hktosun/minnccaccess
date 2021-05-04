#' Read Nware Data
#'
#' Read Nware Data from the shared Google Drive folder.
#'
#'
#' @param table Nware table to read. Either "characteristics", "provider_schedule_daily",  "provider_schedule_hourly", "shift", or "shift_age_group".
#' @param version Nware version. Either "2021-04" or "2020-10".
#' @param filetype Data file type. Either "csv" or "rds".
#'
#' @return A tibble of Nware data.
#' @export

read_nware <- function(table = "characteristics", version = "2021-04", filetype = "csv"){

	if(!table %in% c("characteristics", "provider_schedule_daily", "provider_schedule_hourly", "shift", "shift_age_group",
					 "characteristics_before_2012", "provider_schedule_daily_before_2012", "provider_schedule_hourly_before_2012", "shift_before_2012", "shift_age_group_before_2012")){
		stop("Table does not exist.")
	}
	if(!filetype %in% c("csv", "rds", ".csv", ".rds")){
		stop("File type does not exist.")
	}

	if(filetype == ".csv") filetype = "csv"
	if(filetype == ".rds") filetype = "rds"

	root <- "~/Google Drive/MinnCCAccess/Data Cabinet/Nware Data/"
	subpath <- paste0("Nware Data ", version, " Build/data/", table, ".", filetype)

	path <- paste0(root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 1000000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}
	return(df)

}
