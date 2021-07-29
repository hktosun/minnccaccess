#
# measure_access_osm <- function(){
#
# 	library(tidyverse)
# 	library(minnccaccess)
# 	library(tmap)
# 	library(osmdata)
# 	library(osmplotr)
# 	library(sf)
# 	tmap_mode("view")
#
# 	census_bg <- get_geometry("census-block-group", 2010)
# 	census_b <- get_geometry("census-block", 2010)
#
# 	# For all census block groups, I should create a bbox around it, get the buildings, then filter the buildings to the ones that are actually
# 	# in the block group, then combine them into one single shape, and attach all info from the block group to that single shape. Then combine all such
# 	# shapes into a single dataframe, and use it as the base geography.
#
#
# 	counties <- get_geometry("county") %>%
# 		select(county_id) %>%
# 		group_by(county_id) %>%
# 		nest() %>%
# 		mutate(bbox = map(data, ~st_bbox(.x))) %>%
# 		mutate(buildings = map(bbox, ~{
# 			Sys.sleep(3)
# 			osmplotr::extract_osm_objects(key = "building", bbox = .x) %>%
# 				st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")
# 		})) %>%
# 		mutate(data = map(data, ~st_transform(.x, "+proj=utm +zone=15 +datum=NAD83 +units=m"))) %>%
# 		mutate(buildings = map(buildings, ~(sf::st_union(.x) %>% sf::st_as_sf()) %>% dplyr::rename(geometry = x))) %>%
# 		mutate(build = map2(data, buildings, ~st_intersection(.y, .x))) %>%
# 		mutate(exists = 1)
#
# 	a <- census_bg %>%
# 		group_by(census_block_group_2010) %>%
# 		nest() %>%
# 		ungroup() %>%
# 		rename(cbg = data) %>%
# 		mutate(county_id = str_sub(census_block_group_2010, 1, 5)) %>%
# 		left_join(counties, by = "county_id") %>%
# 		filter(exists == 1) %>%
# 		mutate(cbg = map(cbg, ~st_transform(.x, "+proj=utm +zone=15 +datum=NAD83 +units=m"))) %>%
# 		mutate(build = map2(cbg, build, ~st_intersection(.y, .x))) %>%
# 		mutate(build = map(build, ~st_as_sf(.x))) %>%
# 		mutate(nrow_build = map(build, ~nrow(.x))) %>%
# 		mutate(build = ifelse(nrow_build == 0, cbg, build)) %>%
# 		dplyr::select(census_block_group_2010, cbg, build) %>%
# 		mutate(build = map(build, ~dplyr::mutate(.x, area = sf::st_area(.)))) %>%
# 		mutate(build = map(build, ~dplyr::mutate(.x, area = as.numeric(area)))) %>%
# 		dplyr::select(census_block_group_2010, build)
#
#
#
# 	a2 <- a  %>%
# 		unnest(c(build)) %>%
# 		ungroup() %>%
# 		st_as_sf() %>%
# 		add_acs("kids", "census-block-group", 2019)
#
#
#
# 	state <- get_geometry("state-house-district", year = 2012) %>%
# 		sf::st_union() %>%
# 		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m")
#
# 	hexagons <- state %>%
# 		sf::st_make_grid(what = "polygons", square = FALSE, cellsize = 1 * sqrt(3) * 1609) %>%
# 		tibble::as_tibble() %>%
# 		sf::st_as_sf() %>%
# 		dplyr::mutate(hex_id = dplyr::row_number())
#
# 	r <- sf::st_intersection(a2, hexagons) %>%
# 		dplyr::mutate(intersect_area = sf::st_area(.)) %>%
# 		dplyr::mutate(intersect_area = as.numeric(.data$intersect_area)) %>%
# 		tibble::as_tibble() %>%
# 		dplyr::select(hex_id, area, intersect_area, population_under5) %>%
# 		dplyr::mutate(pop = (intersect_area/area) * population_under5) %>%
# 		dplyr::group_by(hex_id) %>%
# 		dplyr::summarize(population_under5 = sum(pop))
#
#
# 	hexagons <- hexagons %>%
# 		left_join(r, by = "hex_id")
#
#
# 	hexagon_centroids <- hexagons %>%
# 		sf::st_centroid()
#
#
#
# 	hexagon_centroids_m <- hexagon_centroids %>%
# 		sf::st_coordinates() %>%
# 		tibble::as_tibble() %>%
# 		dplyr::rename(X1 = .data$X, Y1 = .data$Y)
#
# 	hexagon_centroids_m$id1 <- hexagon_centroids$hex_id
#
# 	rm(hexagon_centroids)
#
# 	licensing <- read_licensing() %>%
# 		dplyr::filter(.data$date == as_of) %>%
# 		dplyr::select(id2 = .data$license_id, .data$licensed_capacity, .data$street, .data$city, .data$state)
#
# 	providers <- licensing %>%
# 		add_locs() %>%
# 		dplyr::mutate_at(dplyr::vars(.data$lat, .data$lon), as.numeric) %>%
# 		dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
# 		dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
# 		dplyr::select(.data$id2, .data$lat2, .data$lon2, .data$licensed_capacity)
#
# 	rm(licensing)
#
# 	providers_m <- providers %>%
# 		sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326) %>%
# 		sf::st_transform("+proj=utm +zone=15 +datum=NAD83 +units=m") %>%
# 		sf::st_coordinates() %>%
# 		tibble::as_tibble() %>%
# 		dplyr::rename(X2 = .data$X, Y2 = .data$Y)
#
# 	providers_m$id2 <- providers$id2
#
#
#
# 	df_m <- tidyr::expand_grid(hexagon_centroids_m, providers_m)
#
# 	rm(providers_m, hexagon_centroids_m)
#
# 	df_m <- df_m %>%
# 		dplyr::mutate(distance = sqrt((.data$X2 - .data$X1)^2 + (.data$Y2 - .data$Y1)^2)) %>%
# 		dplyr::select(.data$id1, .data$id2, .data$distance) %>%
# 		dplyr::rename(hex_id = .data$id1, license_id = .data$id2)
#
# 	seats_per_kid_stage1 <- df_m %>%
# 		dplyr::mutate(close_enough = dplyr::if_else(.data$distance <= radius * 1e3 * 1.609, 1, 0)) %>%
# 		dplyr::left_join(hex_population, by = "hex_id") %>%
# 		dplyr::left_join(providers, by = c("license_id" = "id2")) %>%
# 		dplyr::mutate(weighted_population_under5 = dplyr::case_when(
# 			.data$close_enough == 1 ~ .data$population_under5 * exp(-((.data$distance)/10000)^decay),
# 			TRUE ~ NA_real_
# 		)) %>%
# 		dplyr::group_by(.data$license_id) %>%
# 		dplyr::mutate(total_weighted_population_under5 = dplyr::case_when(
# 			.data$close_enough == 1 ~ sum(.data$weighted_population_under5, na.rm = TRUE),
# 			TRUE ~ NA_real_)) %>%
# 		dplyr::mutate(seat_per_kid = .data$licensed_capacity / .data$total_weighted_population_under5) %>%
# 		dplyr::ungroup()
#
# 	rm(df_m)
#
# 	seats_per_kid_stage2 <- seats_per_kid_stage1 %>%
# 		dplyr::mutate(weighted_seat_per_kid = dplyr::case_when(
# 			.data$close_enough == 1 ~ .data$seat_per_kid * exp(-((.data$distance)/10000)^decay),
# 			TRUE ~ NA_real_
# 		)) %>%
# 		dplyr::group_by(.data$hex_id) %>%
# 		dplyr::summarize(seat_per_kid = sum(.data$weighted_seat_per_kid, na.rm = TRUE))
#
# 	rm(seats_per_kid_stage1)
#
#
# 	seats_per_kid_sf <- hexagons %>%
# 		dplyr::inner_join(seats_per_kid_stage2, by = "hex_id") %>%
# 		dplyr::left_join(hex_population, by = "hex_id")
#
#
# 	seats_per_kid_sf <- seats_per_kid_sf %>%
# 		dplyr::mutate(seat_per_kid = dplyr::case_when(
# 			.data$seat_per_kid >= quantile(seats_per_kid_sf$seat_per_kid, 0.98) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.98),
# 			.data$seat_per_kid <= quantile(seats_per_kid_sf$seat_per_kid, 0.02) ~ quantile(seats_per_kid_sf$seat_per_kid, 0.02),
# 			TRUE ~ .data$seat_per_kid
# 		))
#
# 	seats_per_kid_sf
#
# }
#
#
