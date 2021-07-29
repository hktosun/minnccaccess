#' Create synthetic locations for families
#'
#' Create synthetic locations for families with access measure
#'
#'
#' @importFrom magrittr %>%
#'
#' @param data Distance/drive time
#' @param kid_per_dot Number of kids per dot on map
#'
#' @return A tibble
#' @export

simulate_families <- function(data, kid_per_dot){

	data <- data %>%
		dplyr::mutate(k = .data$population_under5/kid_per_dot,
					  k_int = as.integer(.data$k),
					  p = .data$k - .data$k_int,
					  n_points = ifelse(stats::runif(1) <= .data$p, .data$k_int + 1, .data$k_int))

	families <- sf::st_sample(data, size = data$n_points) %>%
		sf::st_sf() %>%
		sf::st_join(data)

	families
}
