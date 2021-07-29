#' Read Public School Enrollment Data
#'
#' Read Public School Enrollment Data from the shared Google Drive folder.
#'
#' @importFrom magrittr %>%
#'
#' @param table "school" or "school-district". Defaults to "school".
#' @param subgroups Set TRUE to add demographic (gender, race) and other subgroups. Defaults to FALSE.
#' @param all_grades Set TRUE to get grade school enrollments as well as enrollment in early education. Defaults to FALSE.
#' @param filetype "rds" or "csv". Defaults to "rds".
#' @param GDRIVE_ROOT The local path to the folder that contains the MinnCCAccess folder. Defaults to "~/Google Drive".
#'
#'
#' @return A tibble
#'
#' @source Minnesota Department of Education
#' @export

read_public <- function(table = "school", subgroups = FALSE, all_grades = FALSE, filetype = "rds", GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Public School Enrollment/data/enrollment_", table, ".", filetype)
	path <- paste0(GDRIVE_ROOT, subpath)

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
