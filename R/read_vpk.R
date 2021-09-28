#' Load VPK data
#'
#' Load VPK data using various sources
#'
#' @param source "enrollment" or "allocation"
#'
#' @export
#'
#'
read_vpk <- function(source = "enrollment", geography = "census-place", geo_year = 2010){

	if(source == "enrollment"){
		vpk_locs <- read_public() %>%
			dplyr::filter(grade == "PK", year >= 2017) %>%
			dplyr::mutate(school_district_id = dplyr::case_when(
				school_district_id == "2909-01" & year == 2021 ~ "0706-01",
				TRUE ~ school_district_id
			))

		df <- vpk_locs %>%
			dplyr::left_join(get_geometry("school-location", 2020), by = c("school_district_id", "school_number")) %>%
			sf::st_as_sf() %>%
			dplyr::select(year, school_district_id, school_number, school_name, n = count)


		df <- df %>%
			sf::st_join(get_geometry(geography, geo_year), join = sf::st_within)

		df
	}

}
