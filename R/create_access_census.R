#' Measure access
#'
#' Measure access of families to child care
#'
#'

#' @importFrom rlang :=
#' @importFrom magrittr %>%
#'
#' @param geography Geography
#' @param geo_year Geography vintage
#' @param as_of Date
#' @param acs_year ACS year
#' @param radius Mile radius around geography centroid
#' @param decay Decay parameter for weighting function
#' @export


create_access_census <- function(geography = "census-block-group", geo_year = 2010, as_of = "2021-04-01", acs_year = 2019, radius = 10, decay = 4){

	licensing <- read_licensing() %>%
		dplyr::filter(.data$date == as_of) %>%
		dplyr::select(.data$license_id, .data$licensed_capacity)

	if(geography == "county"){
		geographic_var <- "county_id"
	} else{
		geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", geo_year)
	}

	state <- get_geometry("state-house-district", year = 2012) %>%
		sf::st_union() %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")


	distances <- get_distance_to_providers(geography = geography, geo_year = geo_year, destination = "licensing", as_of = as_of) %>%
		dplyr::left_join(licensing, by = "license_id") %>%
		add_acs("kids", geography, year = acs_year)

	seat_per_kid <- distances %>%
		dplyr::mutate(close_enough = dplyr::if_else(.data$distance <= radius * 1e3 * 1.609, 1, 0)) %>%
		dplyr::mutate(weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ population_under5 * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data$license_id) %>%
		dplyr::mutate(total_weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ sum(.data$weighted_population_under5, na.rm = TRUE),
			TRUE ~ NA_real_)) %>%
		dplyr::mutate(seat_per_kid = .data$licensed_capacity / .data$total_weighted_population_under5) %>%
		dplyr::ungroup() %>%
		dplyr::select(tidyselect::any_of(geographic_var), .data$license_id, .data$distance, .data$seat_per_kid, .data$close_enough)

	seats_per_kid <- seat_per_kid %>%
		dplyr::mutate(weighted_seat_per_kid = dplyr::case_when(
			.data$close_enough == 1 ~ seat_per_kid * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data[[geographic_var]]) %>%
		dplyr::summarize(seat_per_kid = sum(.data$weighted_seat_per_kid, na.rm = TRUE))

	seats_per_kid <- seats_per_kid %>%
		add_acs("kids", geography, year = acs_year)

	seats_per_kid <- seats_per_kid %>%
		dplyr::mutate(seat_per_kid = dplyr::case_when(
			seat_per_kid >= quantile(seats_per_kid$seat_per_kid, 0.98) ~ quantile(seats_per_kid$seat_per_kid, 0.98),
			seat_per_kid <= quantile(seats_per_kid$seat_per_kid, 0.02) ~ quantile(seats_per_kid$seat_per_kid, 0.02),
			TRUE ~ seat_per_kid
		)) %>%
		add_geometry(geography, geo_year) %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")

	state <- get_geometry("state-house-district", year = 2012) %>%
		sf::st_union() %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")

	seats_per_kid <- seats_per_kid %>%
		sf::st_intersection(state)


	seats_per_kid

}
