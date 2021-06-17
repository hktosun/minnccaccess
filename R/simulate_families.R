filter_sf <- function(.data, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL) {
	bb <- sf::st_bbox(.data)
	if (!is.null(xmin)) bb["xmin"] <- xmin
	if (!is.null(xmax)) bb["xmax"] <- xmax
	if (!is.null(ymin)) bb["ymin"] <- ymin
	if (!is.null(ymax)) bb["ymax"] <- ymax
	sf::st_filter(.data, sf::st_as_sfc(bb), .predicate = sf::st_within)
}


#' Create synthetic locations for families
#'
#' Create synthetic locations for families within each geography
#'
#'
#' @importFrom magrittr %>%
#'
#' @param data Distance/drive time
#' @param kid_per_dot Number of kids per dot on map
#'
#' @return A tibble
#' @export

simulate_families <- function(data, kid_per_dot = 4){

	geographic_var <- names(data)[1]

	geography <- dplyr::case_when(
		geographic_var %in% c("census_tract_2010", "census_tract_2020") ~ "census-tract",
		geographic_var %in% c("county") ~ "county",
 		geographic_var %in% c("census_block_group_2010", "census_block_group_2020") ~ "census-block-group"
	)

	year <- dplyr::case_when(
		geographic_var %in% c("census_tract_2010", "census_block_group_2010") ~ 2010,
		geographic_var %in% c("county") ~ 2019,
		geographic_var %in% c("census_tract_2020", "census_block_group_2020") ~ 2020
	)

	data <- data %>%
		add_acs("kids", geography, 2019) %>%
		add_geometry(geography, year = year)

	water <- get_geometry("water-bodies") %>%
		filter_sf(xmin = -97.23909, xmax = -89.48338, ymin = 43.49936, ymax = 49.38448)

	data <- data %>%
		dplyr::group_by(.data[[geographic_var]], .data$seat_per_kid) %>%
		tidyr::nest() %>%
		dplyr::mutate(sample_locs = purrr::map(.data$data, ~(sf::st_sample(.x$geometry, round(.x$population_under5 / kid_per_dot)) %>% st_sf()))) %>%
		dplyr::ungroup() %>%
		dplyr::select(-.data$data) %>%
		tidyr::unnest(.data$sample_locs) %>%
		sf::st_sf()
}
