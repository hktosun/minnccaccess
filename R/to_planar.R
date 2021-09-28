

#' @export

to_planar <- function(df){
	df %>%
		sf::st_transform("+proj=utm +zone=15N +datum=NAD83 +units=m")
}

#' @export
#'
to_longlat <- function(df){
	df %>%
		sf::st_transform("+proj=longlat +datum=WGS84")
}
