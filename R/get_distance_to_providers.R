#' Get Euclidean  distance
#'
#' Get Euclidean distance from population-weighted centroids to child care providers
#'
#'
#' @importFrom magrittr %>%
#'
#' @param geography Geographies to add to. Defaults to all geographies available.
#' @param geo_year Vintage for the geography
#' @param destination List of providers to calculate distance to
#' @param as_of Date
#'
#' @return A tibble
#' @export



get_distance_to_providers <- function(geography = "census-tract", geo_year = 2010, destination = "licensing", as_of = "2021-04-01"){

	if(geography == "county"){
		geographic_var <- "county"
	} else{
		geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", geo_year)
	}


	df1 <- get_centroids(geography = geography, year = geo_year) %>%
		dplyr::rename(id1 = geographic_var, lat1 = .data$lat, lon1 = .data$lon) %>%
		dplyr::select(.data$id1, .data$lat1, .data$lon1)

	df1_m <- df1 %>%
		sf::st_as_sf(coords = c("lon1", "lat1"), crs = 4326) %>%
		sf::st_transform("+proj=utm +zone=15 +datum=WGS84 +units=m") %>%
		sf::st_coordinates() %>%
		tibble::as_tibble() %>%
		dplyr::rename(X1 = .data$X, Y1 = .data$Y)

	df1_m$id1 = df1$id1

	if(destination == "licensing"){
		df2 <- read_licensing() %>%
			dplyr::filter(.data$date == as.Date(as_of)) %>%
			add_locs() %>%
			dplyr::mutate_at(vars(.data$lat, .data$lon), as.numeric) %>%
			dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
			dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
			dplyr::rename(id2 = .data$license_id) %>%
			dplyr::select(.data$id2, .data$lat2, .data$lon2)

		provider_var <- "license_id"

	} else if(destination == "nware"){
		df2 <- read_nware() %>%
			dplyr::filter(.data$date == as.Date(as_of)) %>%
			add_locs(geography = c("lat", "lon")) %>%
			dplyr::mutate_at(vars(.data$lat, .data$lon), as.numeric) %>%
			dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
			dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
			dplyr::rename(id2 = .data$provider_uid) %>%
			dplyr::select(.data$id2, .data$lat2, .data$lon2)

		provider_var <- "provider_uid"
	}

	df2_m <- df2 %>%
		sf::st_as_sf(coords = c("lon2", "lat2"), crs = 4326) %>%
		sf::st_transform("+proj=utm +zone=15 +datum=WGS84 +units=m") %>%
		sf::st_coordinates() %>%
		tibble::as_tibble() %>%
		dplyr::rename(X2 = .data$X, Y2 = .data$Y)

	df2_m$id2 = df2$id2

	df_m <- tidyr::expand_grid(df1_m, df2_m)

	df_m <- df_m %>%
		dplyr::mutate(distance = sqrt((.data$X2 - .data$X1)^2 + (.data$Y2 - .data$Y1)^2)) %>%
		dplyr::select(.data$id1, .data$id2, .data$distance) %>%
		dplyr::rename(!!enquo(geographic_var) := .data$id1, !!enquo(provider_var) := .data$id2)

	df_m

}
