#' Read Public School Enrollment Data
#'
#' Read Public School Enrollment Data from the shared Google Drive folder.
#'
#'
#' @param table "school" or "district"
#' @param filetype "rds" or "csv"
#' @param total `TRUE` to ignore demographic subgroups.
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#'
#' @return A tibble
#'
#' @source Minnesota Department of Education
#' @export

read_public <- function(table = "school", total = TRUE, filetype = "rds", gdrive_root = "~/Google Drive"){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Public School Enrollment/data/enrollment_", table, ".", filetype)
	path <- paste0(gdrive_root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 4000000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}

	if(total){
		df <- df |>
			dplyr::filter(.data$gender == "Total", .data$race == "Total", .data$subgroup == "Total")
	}

	return(df)
}
