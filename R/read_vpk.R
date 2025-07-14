#' Read VPK data
#'
#' Read VPK data using various sources
#'
#' @param source "enrollment" or "allocation"
#' @param geography Geographic unit attached to the VPK data
#' @param geo_year Vintage of the geography
#'
#' @return A tibble
#' @export
#'
#'
read_vpk <- function(source = "enrollment", geography = "census-place", geo_year = 2010, GDRIVE_ROOT = Sys.getenv("GDRIVE_ROOT")){

	if(source == "enrollment"){
		vpk_locs <- read_public(GDRIVE_ROOT = GDRIVE_ROOT) %>%
			dplyr::filter(.data$grade == "PK", .data$year >= 2017) %>%
			dplyr::mutate(school_district_id = dplyr::case_when(
				.data$school_district_id == "2909-01" & .data$year == 2021 ~ "0706-01",
				TRUE ~ .data$school_district_id
			))
		school_locations <- get_geometry('school-location', 2020, GDRIVE_ROOT = GDRIVE_ROOT)
		df <- vpk_locs %>%
			dplyr::left_join(school_locations, by = c("school_district_id", "school_number")) %>%
			sf::st_as_sf() %>%
			dplyr::select(.data$year, .data$school_district_id, .data$school_number, .data$school_name, n = .data$count)

		geom <- get_geometry(geography, geo_year, GDRIVE_ROOT = GDRIVE_ROOT)
		df <- df %>%
			sf::st_join(geom, join = sf::st_within)

		df
	}

}
