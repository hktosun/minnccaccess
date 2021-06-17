#' Get Euclidean  distance
#'
#' Get Euclidean distance from population-weighted centroids to child care providers
#'
#'
#' @importFrom magrittr %>%
#'
#' @param geography Geographies to add to. Defaults to all geographies available.
#' @param year Vintage for the geography
#' @param destination List of providers to calculate distance to
#' @param as_of Date
#'
#' @return A tibble
#' @export



get_distance_to_providers <- function(geography = "census-tract", year = 2010, destination = "licensing", as_of = "2021-04-01"){

	if(geography == "county"){
		geographic_var <- "county"
	} else{
		geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", year)
	}


	df1 <- get_centroids(geography = geography, year = year) %>%
		dplyr::rename(id = geographic_var) %>%
		dplyr::select(.data$id, .data$lat, .data$lon)

	df1_sf <- df1 %>%
		sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

	if(destination == "licensing"){
		df2 <- read_licensing() %>%
			dplyr::filter(.data$date == as.Date(as_of)) %>%
			add_locs() %>%
			dplyr::mutate_at(vars(.data$lat, .data$lon), as.numeric) %>%
			dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
			dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
			dplyr::rename(id2 = .data$license_id) %>%
			dplyr::select(.data$id2, .data$lat2, .data$lon2)

	} else if(destination == "nware"){
		df2 <- read_nware() %>%
			dplyr::filter(.data$date == as.Date(as_of)) %>%
			add_locs(geography = c("lat", "lon")) %>%
			dplyr::mutate_at(vars(.data$lat, .data$lon), as.numeric) %>%
			dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
			dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
			dplyr::rename(id2 = .data$provider_uid) %>%
			dplyr::select(.data$id2, .data$lat2, .data$lon2)
	}

	df2_sf <- df2 %>%
		sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326)

	df <- tidyr::expand_grid(df1, df2)

	distances <- sf::st_distance(df1_sf, df2_sf) %>%
		tibble::as_tibble() %>%
		tibble::rowid_to_column() %>%
		tidyr::pivot_longer(2:(nrow(df2) + 1),
							names_to = "name",
							values_to = "distance") %>%
		dplyr::mutate(distance = as.numeric(.data$distance)) %>%
		dplyr::pull(.data$distance)

	df$distance <- distances # in meters

	df

}
