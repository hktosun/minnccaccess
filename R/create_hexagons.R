#' Create access map using hexagons
#'
#' Create child care access map using hexagons
#'
#' @importFrom magrittr %>%
#'
#' @param geography Geography
#' @param geography_year Geography year. 2010 or 2020
#' @param acs_year 5-year ACS end year
#' @param hex_side Length of side of hexagons
#' @param radius Consider providers/families that are in `radius` miles or closer.
#' @param decay Decay parameter (used to weight access by distance) in calculating the access measure
#' @param kid_per_dot Number of kids per dot on the map
#' @param as_of Access as of date
#'
#' @return A tmap object
#'
#' @export
#'

create_hexagon_access_map <- function(geography = "census-block-group", geography_year = 2010, acs_year = 2019, hex_side = 1, radius = 10, decay = 4, kid_per_dot = 100, as_of = "2021-04-01"){

	geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", geography_year)
	base_geographies <- get_geometry(geography, geography_year) %>%
		sf::st_transform("+proj=utm +datum=NAD83 +units=m") %>%
		dplyr::mutate(area = sf::st_area(.)) %>%
		dplyr::mutate(area = as.numeric(.data$area)) %>%
		add_acs("kids", geography, year = acs_year)


	hexagons <- base_geographies %>%
		sf::st_make_grid(what = "polygons", square = FALSE, cellsize = hex_side * 3941) %>%
		tibble::as_tibble() %>%
		sf::st_as_sf() %>%
		dplyr::mutate(hex_id = dplyr::row_number())


	hex_population <- sf::st_intersection(base_geographies, hexagons) %>%
		dplyr::mutate(intersect_area = sf::st_area(.)) %>%
		dplyr::mutate(intersect_area = as.numeric(.data$intersect_area)) %>%
		as.data.frame() %>%
		tibble::as_tibble() %>%
		dplyr::select(tidyselect::one_of(geographic_var), .data$hex_id, .data$area, .data$intersect_area, .data$population_under5) %>%
		dplyr::mutate(pop = (.data$intersect_area/.data$area) * .data$population_under5) %>%
		dplyr::group_by(.data$hex_id) %>%
		dplyr::summarize(population_under5 = sum(.data$pop))

	hex_polygons_with_pop <- hexagons %>%
		dplyr::inner_join(hex_population, by = "hex_id")

	hex_centroids_with_pop_sf <- hex_polygons_with_pop %>%
		sf::st_centroid()

	hex_centroids_with_pop <- hex_centroids_with_pop_sf %>%
		as.data.frame() %>%
		tibble::as_tibble() %>%
		dplyr::select(-.data$geometry)

	licensing <- read_licensing() %>%
		dplyr::filter(.data$date == as_of) %>%
		dplyr::select(id2 = .data$license_id, .data$licensed_capacity, .data$street, .data$city, .data$state)

	providers <- licensing %>%
		add_locs() %>%
		dplyr::mutate_at(dplyr::vars(.data$lat, .data$lon), as.numeric) %>%
		dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
		dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
		dplyr::select(.data$id2, .data$lat2, .data$lon2, .data$licensed_capacity)


	providers_sf <- providers %>%
		sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326) %>%
		sf::st_transform("+proj=utm +datum=NAD83 +units=m")


	grid <- tidyr::expand_grid(hex_centroids_with_pop, providers)

	distances <- sf::st_distance(hex_centroids_with_pop_sf, providers_sf) %>%
		tibble::as_tibble() %>%
		tibble::rowid_to_column() %>%
		tidyr::pivot_longer(2:(nrow(providers) + 1),
							names_to = "name",
							values_to = "distance") %>%
		dplyr::mutate(distance = as.numeric(.data$distance)) %>%
		dplyr::pull(.data$distance)

	grid$distance <- distances # in meters

	seats_per_kid_stage1 <- grid %>%
		dplyr::mutate(close_enough = dplyr::if_else(.data$distance <= radius * 2.5 * 1e3 * 1.609, 1, 0)) %>%
		dplyr::mutate(weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ .data$population_under5 * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data$id2) %>%
		dplyr::mutate(total_weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ sum(.data$weighted_population_under5, na.rm = TRUE),
			TRUE ~ NA_real_)) %>%
		dplyr::mutate(seat_per_kid = .data$licensed_capacity / .data$total_weighted_population_under5) %>%
		dplyr::ungroup()

	seats_per_kid_stage2 <- seats_per_kid_stage1 %>%
		dplyr::mutate(weighted_seat_per_kid = dplyr::case_when(
			.data$close_enough == 1 ~ .data$seat_per_kid * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data$hex_id) %>%
		dplyr::summarize(seat_per_kid = sum(.data$weighted_seat_per_kid, na.rm = TRUE))

	seats_per_kid_sf <- hex_polygons_with_pop %>%
		dplyr::inner_join(seats_per_kid_stage2, by = "hex_id")


	seats_per_kid_sf <- seats_per_kid_sf %>%
		dplyr::mutate(seat_per_kid = dplyr::case_when(
			.data$seat_per_kid >= quantile(seats_per_kid_sf$seat_per_kid, 0.98) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.98),
			.data$seat_per_kid <= quantile(seats_per_kid_sf$seat_per_kid, 0.02) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.02),
			TRUE ~ .data$seat_per_kid
		))


	family_locations_with_seat_per_kid <- seats_per_kid_sf %>%
		dplyr::group_by(.data$hex_id, .data$seat_per_kid) %>%
		tidyr::nest() %>%
		dplyr::mutate(sample_locs = purrr::map(.data$data, ~(sf::st_sample(.x$geometry, round(.x$population_under5 / kid_per_dot)) %>% sf::st_sf()))) %>%
		dplyr::ungroup() %>%
		dplyr::select(-.data$data) %>%
		tidyr::unnest(.data$sample_locs) %>%
		sf::st_sf()


	access_map <- plot_map(family_locations_with_seat_per_kid, fill_with = .data$seat_per_kid, palette = c("red", "pink", "grey", "lightblue", "blue"), n = 5, alpha = 0.5,
				   breaks = c(0, 0.3, 0.6, 0.9, 1.2, 1.5))

	access_map
}


