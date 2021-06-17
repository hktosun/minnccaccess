#' Get drive time
#'
#' Get drive time for pairs of locations.
#'
#' @importFrom magrittr %>%
#'
#' @param data A data frame with a pair of locations. Columns `id`, `lat`, `lon`, `id2`, `lat2`, `lon2` should be present.
#' @param max_distance Maximum distance to calculate drive times
#'
#' @return A tibble
#' @export


get_drive_time <- function(data, max_distance = 30){

	df <- data %>%
		dplyr::filter(.data$distance <= max_distance * 1e3 * 1.609)

	if(nrow(df) == 0){
		stop("All pairs are filtered out. Choose another value for max_distance.")
	}

	df <- df %>%
		dplyr::group_by(.data$id, .data$lat, .data$lon) %>%
		dplyr::mutate(row_id = dplyr::row_number()) %>%
		dplyr::mutate(group_id = ceiling(.data$row_id/50)) %>%
		dplyr::ungroup()

	batches <- df %>%
		dplyr::group_by(.data$id, .data$group_id) %>%
		dplyr::summarize(provs_in_group = dplyr::n()) %>%
		dplyr::ungroup()

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

	 data <- data %>%
		dplyr::left_join(df, by = c("id", "id2"))

	data
}

