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
#' @param tmap_mode Map mode if tmap is chosen as the mapping method.
#' @param alpha Transparency
#' @param ... Other arguments passed on to `geom_sf`, `tmap_polygons`, and `tmap_dots`
#'
#' @return A ggplot or tmap object.
#' @export
#'
#'

plot_map <- function(data, method = "tmap", fill_with, tmap_mode = "view", alpha = 0.5, ...){

	if(!method %in% c("sf", "tmap")){
		stop("Method should be `sf` or `tmap`")
	}

	if(method != "tmap" & tmap_mode == "plot"){
		stop("Set method to `tmap`.")
	}



	if(!"sf" %in% class(data) & !"sfc" %in% class(data)){
		n1 <- nrow(data)
		data <- data %>%
			dplyr::filter(!is.na(.data$lat) & !is.na(.data$lon)) %>%
			sf::st_as_sf(coords = c("lon", "lat"),
						 crs = 4326)
		n2 <- nrow(data)

		if(n2 < n1){
			warning(paste0(n1 - n2, " observation(s) have been dropped due to missing lat/lon."))
		}
	}

	if(method == "sf"){
		if("sfc_POINT" %in% class(data$geometry)){
			if(missing(fill_with)){
				p <- ggplot(data) +
					geom_sf(...) +
					theme_void()
			} else {
				p <- ggplot(data) +
					geom_sf(aes(color = {{fill_with}}), ...) +
					theme_void()
			}
		}

		if("sfc_MULTIPOLYGON" %in% class(data$geometry) | "sfc_POLYGON" %in% class(data$geometry) | "sfc_GEOMETRY" %in% class(data)){
			if(missing(fill_with)){
				p <- ggplot(data) +
					geom_sf(alpha = alpha) +
					theme_void()
			} else {
				p <- ggplot(data) +
					geom_sf(aes(fill = {{fill_with}}), alpha = alpha) +
					theme_void()
			}
		}
	}


	if(method == "tmap"){

		if(!tmap_mode %in% c("view", "plot")){
			stop("`tmap_mode` should be `view` or `plot`.")
		}

		if(tmap_mode == "view"){
			tmap::tmap_mode("view")
		}

		if(tmap_mode == "plot"){
			tmap::tmap_mode("plot")
		}

		if("sfc_POINT" %in% class(data$geometry)){
			if(missing(fill_with)){
				p <- tm_shape(data) +
					tm_dots(...) +
					tm_layout(frame = FALSE)
			} else {
				p <- tm_shape(data) +
					tm_dots(col = quo_name(enquo(fill_with)), ...) +
					tm_layout(frame = FALSE, legend.position = c("right","bottom"), legend.outside = TRUE)
			}
		}

		if("sfc_MULTIPOLYGON" %in% class(data$geometry) | "sfc_POLYGON" %in% class(data$geometry) | "sfc_GEOMETRY" %in% class(data)){
			if(missing(fill_with)){
				p <- tm_shape(data) +
					tm_polygons(alpha = alpha, ...) +
					tm_layout(frame = FALSE)
			} else {
				p <- tm_shape(data) +
					tm_polygons(col = quo_name(enquo(fill_with)), alpha = alpha, ...) +
					tm_layout(frame = FALSE, legend.position = c("right","bottom"), legend.outside = TRUE)
			}
		}

	}

	p
}
