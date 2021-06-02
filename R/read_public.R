#' Read Public School Enrollment Data
#'
#' Read Public School Enrollment Data from the shared Google Drive folder.
#'
#' @importFrom magrittr %>%
#'
#' @param table "school" or "district"
#' @param subgroups `TRUE` to add demographic subgroups.
#' @param all_grades `TRUE` to get grade school enrollments as well as K and Pre-K
#' @param filetype "rds" or "csv"
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#'
#' @return A tibble
#'
#' @source Minnesota Department of Education
#' @export

read_public <- function(table = "school", subgroups = FALSE, all_grades = FALSE, filetype = "rds", gdrive_root = "~/Google Drive"){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Public School Enrollment/data/enrollment_", table, ".", filetype)
	path <- paste0(gdrive_root, subpath)

	if(filetype == "csv"){
		df <- readr::read_csv(path, guess_max = 4000000)
	}
	if(filetype == "rds"){
		df <- readr::read_rds(path)
	}

	if(!all_grades){
		df <- df %>%
			dplyr::filter(.data$grade %in% c("EC", "KG", "ECSE", "PK"))
	}

	if(!subgroups){
		df <- df %>%
			dplyr::filter(.data$gender == "Total", .data$race == "Total", .data$subgroup == "Total") %>%
			dplyr::select(-c(.data$gender, .data$race, .data$subgroup))
	}

	return(df)
}
