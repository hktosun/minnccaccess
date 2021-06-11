get_osrm_duration <- function(data){

	df <- data %>%
		dplyr::group_by(.data$id, .data$lat, .data$lon) %>%
		dplyr::mutate(row_id = dplyr::row_number()) %>%
		dplyr::mutate(group_id = ceiling(.data$row_id/50)) %>%
		dplyr::ungroup()

	batches <- df %>%
		dplyr::group_by(.data$id, .data$group_id) %>%
		dplyr::summarize(provs_in_group = dplyr::n())

	df <- df %>%
		dplyr::group_by(.data$id, .data$lat, .data$lon, .data$group_id) %>%
		dplyr::summarize(lonlat = paste(.data$lon2, ",", .data$lat2, sep = "", collapse = ";"),
				  id2s = paste(.data$id2, collapse = ";")) %>%
		dplyr::ungroup() %>%
		dplyr::left_join(batches, by = c("id", "group_id")) %>%
		dplyr::mutate(url = purrr::pmap(list(.data$lon, .data$lat, .data$lonlat), ~paste0("http://router.project-osrm.org/table/v1/driving/", ..1, ",", ..2, ";", ..3, "?sources=0"))) %>%
		dplyr::mutate(duration = purrr::map(.data$url, ~jsonlite::fromJSON(.x))) %>%
		dplyr::mutate(duration = purrr::map(.data$duration, ~(.x[["durations"]]))) %>%
		dplyr::mutate(duration = purrr::map2(.data$duration, .data$provs_in_group, ~(.x[1, 2:(.y + 1)]))) %>%
		dplyr::mutate(id2 = stringr::str_split(.data$id2s, ";")) %>%
		tidyr::unnest(c(.data$id2, .data$duration)) %>%
		dplyr::select(.data$id, .data$id2, .data$duration)

	df

}

#' Get Euclidean and driving distance
#'
#' Get (Euclidean) distance and driving duration using OSRM.
#'

#' @param geography Geographies to add to. Defaults to all geographies available.
#' @param year Vintage for the geography
#' @param destination List of providers to calculate distance to
#' @param date Date
#'
#' @return A tibble
#' @export
#'
#'
#'

get_drive_time <- function(geography = "census-tract", year = 2010, destination = "licensing", date = "2021-04-01"){

	if(geography == "county"){
		geographic_var <- "county"
	} else{
		geographic_var <- paste0(stringr::str_replace_all(geography, "-", "_"), "_", year)
	}


	df1 <- get_centroids(geography = geography, year = year) %>%
		dplyr::rename(id = geographic_var) %>%
		dplyr::select(.data$id, .data$lat, .data$lon) %>%
		dplyr::sample_n(10)

	df1_sf <- df1 %>%
		sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

	if(destination == "licensing"){
		df2 <- read_licensing() %>%
			dplyr::rename(mydate = .data$date) %>%
			dplyr::filter(.data$mydate == as.Date(date)) %>%
			add_locs() %>%
			dplyr::mutate_at(vars(.data$lat, .data$lon), as.numeric) %>%
			dplyr::rename(lat2 = .data$lat, lon2 = .data$lon) %>%
			dplyr::filter(!is.na(.data$lat2) & !is.na(.data$lon2)) %>%
			dplyr::rename(id2 = .data$license_id) %>%
			dplyr::select(.data$id2, .data$lat2, .data$lon2) %>%
			dplyr::sample_n(300)

	} else if(destination == "nware"){
		df2 <- read_nware() %>%
			dplyr::rename(mydate = .data$date) %>%
			dplyr::filter(.data$mydate == as.Date(date)) %>%
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

	durations <- df %>%
		dplyr::filter(.data$distance <= 48280) %>%
		get_osrm_duration()


	df <- df %>%
		dplyr::left_join(durations, by = c("id", "id2"))

	df
}

