


create_hexagons <- function(geography = "census-block-group", geo_year = 2010, acs_year = 2019, hex_side = 1){

	geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", geo_year)

	water <- get_geometry("water-bodies") %>%
		dplyr::slice_max(.data$acres, n = 20000) %>%
		sf::st_transform("+proj=utm +zone=15N +datum=NAD83 +units=m") %>%
		sf::st_union()

	state <- get_geometry("state-house-district", year = 2012) %>%
		sf::st_union() %>%
		sf::st_transform("+proj=utm +zone=15N +datum=NAD83 +units=m")


	base_geographies <- get_geometry(geography, geo_year) %>%
		sf::st_transform("+proj=utm +zone=15N +datum=NAD83 +units=m") %>%
		dplyr::mutate(area = sf::st_area(.)) %>%
		dplyr::mutate(area = as.numeric(.data$area)) %>%
		add_acs("kids", geography, year = acs_year) %>%
		sf::st_intersection(state) %>%
		sf::st_difference(water)


	hexagons <- state %>%
		sf::st_make_grid(what = "polygons", square = FALSE, cellsize = hex_side * sqrt(3) * 1609) %>%
		tibble::as_tibble() %>%
		sf::st_as_sf() %>%
		dplyr::mutate(hex_id = dplyr::row_number()) %>%
		sf::st_intersection(state) %>%
		sf::st_difference(water)



	hex_population <- sf::st_intersection(base_geographies, hexagons) %>%
		dplyr::mutate(intersect_area = sf::st_area(.)) %>%
		dplyr::mutate(intersect_area = as.numeric(.data$intersect_area)) %>%
		as.data.frame() %>%
		tibble::as_tibble() %>%
		dplyr::select(tidyselect::one_of(geographic_var), .data$hex_id, .data$area, .data$intersect_area, .data$population_under5) %>%
		dplyr::mutate(pop = (.data$intersect_area/.data$area) * .data$population_under5) %>%
		dplyr::group_by(.data$hex_id) %>%
		dplyr::summarize(population_under5 = sum(.data$pop))

	#rm(base_geographies)

	hexagons <- hexagons %>%
		dplyr::semi_join(hex_population, by = "hex_id")

	# tmap::tmap_mode("view")
	# tmap::tm_shape(hexagons) + tmap::tm_polygons()

	hexagons

	#sf::st_write(hexagons, "~/Desktop/hexagons.shp")

}
