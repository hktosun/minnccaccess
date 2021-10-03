#' Convert long/lat to planar coordinates
#'
#' Convert long/;at coordinates to planar coordinates.
#'
#' @param df sf tibble
#'
#' @return An sf tibble
#' @export

to_planar <- function(df){
	df %>%
		sf::st_transform("+proj=utm +zone=15N +datum=NAD83 +units=m")
}

#' Convert planar to long/lat coordinates
#'
#' Convert planar coordinates to long/lat coordinates.
#'
#' @param df sf tibble
#'
#' @return An sf tibble
#' @export
#'
to_longlat <- function(df){
	df %>%
		sf::st_transform("+proj=longlat +datum=WGS84")
}
