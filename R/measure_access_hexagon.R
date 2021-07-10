#' Measure child care access using hexagons
#'
#' Measure access of families to child care using hexagons
#'
#' @importFrom magrittr %>%
#'
#' @param geography Geography
#' @param geo_year Geography year. 2010 or 2020
#' @param acs_year 5-year ACS end year
#' @param hex_side Length of side of hexagons
#' @param radius Consider providers/families that are in `radius` miles or closer.
#' @param decay Decay parameter (used to weight access by distance) in calculating the access measure
#' @param as_of Access as of date
#'
#' @return An sf object
#'
#' @export
#'

measure_access_hexagon <- function(geography = "census-block-group", geo_year = 2010, acs_year = 2019, hex_side = 1, radius = 10, decay = 4, as_of = "2021-04-01"){

	geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", geo_year)

	base_geographies <- get_geometry(geography, geo_year) %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m") %>%
		dplyr::mutate(area = sf::st_area(.)) %>%
		dplyr::mutate(area = as.numeric(.data$area)) %>%
		add_acs("kids", geography, year = acs_year)

	state <- get_geometry("state-house-district", year = 2012) %>%
		sf::st_union() %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")


	hexagons <- state %>%
		sf::st_make_grid(what = "polygons", square = FALSE, cellsize = hex_side * sqrt(3) * 1609) %>%
		tibble::as_tibble() %>%
		sf::st_as_sf() %>%
		dplyr::mutate(hex_id = dplyr::row_number()) %>%
		sf::st_intersection(state)

	rm(state)


	hex_population <- sf::st_intersection(base_geographies, hexagons) %>%
		dplyr::mutate(intersect_area = sf::st_area(.)) %>%
		dplyr::mutate(intersect_area = as.numeric(.data$intersect_area)) %>%
		as.data.frame() %>%
		tibble::as_tibble() %>%
		dplyr::select(tidyselect::one_of(geographic_var), .data$hex_id, .data$area, .data$intersect_area, .data$population_under5) %>%
		dplyr::mutate(pop = (.data$intersect_area/.data$area) * .data$population_under5) %>%
		dplyr::group_by(.data$hex_id) %>%
		dplyr::summarize(population_under5 = sum(.data$pop))

	rm(base_geographies)

	hexagons <- hexagons %>%
		dplyr::semi_join(hex_population, by = "hex_id")

	hexagon_centroids <- hexagons %>%
		sf::st_centroid()



	hexagon_centroids_m <- hexagon_centroids %>%
		sf::st_coordinates() %>%
		tibble::as_tibble() %>%
		dplyr::rename(X1 = .data$X, Y1 = .data$Y)

	rm(hexagon_centroids)

	hexagon_centroids_m$id1 <- hexagon_centroids$hex_id

	licensing <- read_licensing() %>%
		dplyr::filter(.data$date == as_of) %>%
		dplyr::select(id2 = .data$license_id, .data$licensed_capacity, .data$street, .data$city, .data$state)

	providers <- licensing %>%
		add_locs() %>%
		dplyr::mutate_at(dplyr::vars(.data$lat, .data$lon), as.numeric) %>%
		dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
		dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
		dplyr::select(.data$id2, .data$lat2, .data$lon2, .data$licensed_capacity)

	rm(licensing)

	providers_m <- providers %>%
		sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326) %>%
		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m") %>%
		sf::st_coordinates() %>%
		tibble::as_tibble() %>%
		dplyr::rename(X2 = .data$X, Y2 = .data$Y)

	providers_m$id2 <- providers$id2



	df_m <- tidyr::expand_grid(hexagon_centroids_m, providers_m)

	rm(providers_m, hexagon_centroids_m)

	df_m <- df_m %>%
		dplyr::mutate(distance = sqrt((.data$X2 - .data$X1)^2 + (.data$Y2 - .data$Y1)^2)) %>%
		dplyr::select(.data$id1, .data$id2, .data$distance) %>%
		dplyr::rename(hex_id = .data$id1, license_id = .data$id2)

	seats_per_kid_stage1 <- df_m %>%
		dplyr::mutate(close_enough = dplyr::if_else(.data$distance <= radius * 1e3 * 1.609, 1, 0)) %>%
		dplyr::left_join(hex_population, by = "hex_id") %>%
		dplyr::left_join(providers, by = c("license_id" = "id2")) %>%
		dplyr::mutate(weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ .data$population_under5 * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data$license_id) %>%
		dplyr::mutate(total_weighted_population_under5 = dplyr::case_when(
			.data$close_enough == 1 ~ sum(.data$weighted_population_under5, na.rm = TRUE),
			TRUE ~ NA_real_)) %>%
		dplyr::mutate(seat_per_kid = .data$licensed_capacity / .data$total_weighted_population_under5) %>%
		dplyr::ungroup()

	rm(df_m)

	seats_per_kid_stage2 <- seats_per_kid_stage1 %>%
		dplyr::mutate(weighted_seat_per_kid = dplyr::case_when(
			.data$close_enough == 1 ~ .data$seat_per_kid * exp(-((.data$distance)/10000)^decay),
			TRUE ~ NA_real_
		)) %>%
		dplyr::group_by(.data$hex_id) %>%
		dplyr::summarize(seat_per_kid = sum(.data$weighted_seat_per_kid, na.rm = TRUE))

	rm(seats_per_kid_stage1)


	seats_per_kid_sf <- hexagons %>%
		dplyr::inner_join(seats_per_kid_stage2, by = "hex_id") %>%
		dplyr::left_join(hex_population, by = "hex_id")


	seats_per_kid_sf <- seats_per_kid_sf %>%
		dplyr::mutate(seat_per_kid = dplyr::case_when(
			.data$seat_per_kid >= quantile(seats_per_kid_sf$seat_per_kid, 0.98) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.98),
			.data$seat_per_kid <= quantile(seats_per_kid_sf$seat_per_kid, 0.02) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.02),
			TRUE ~ .data$seat_per_kid
		))

	seats_per_kid_sf
}


