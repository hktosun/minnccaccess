#' Plot shapefiles
#'
#' Plot geography shapefiles using geom_sf
#'
#' @import sf
#' @import tmap
#' @import ggplot2
#' @param data An sf object that contains the geometries
#' @param method Plotting method. Can be "sf" or "tmap"
#' @param fill_with (Optional) The variable from the sf object to fill the polygons.
#'
#' @return A ggplot or tmap object.
#' @export
#'
#'

plot_map <- function(data, method = "sf", fill_with){

	if(!method %in% c("sf", "tmap")){
		stop("Method should be `sf` or `tmap`")
	}

	if(method == "sf"){
		if(missing(fill_with)){
			p <- ggplot(data) +
				geom_sf() +
				theme_void()
		} else {
			p <- ggplot(data) +
				geom_sf(aes(fill = {{fill_with}})) +
				theme_void()
		}
	}

	if(method == "tmap"){
		if(missing(fill_with)){
			p <- tm_shape(data) +
				tm_polygons() +
				tm_layout(frame = FALSE)
		} else {
			p <- tm_shape(data) +
				tm_polygons(col = quo_name(enquo(fill_with))) +
				tm_layout(frame = FALSE, legend.position = c("right","bottom"), legend.outside = TRUE)
		}
	}
	p
}
