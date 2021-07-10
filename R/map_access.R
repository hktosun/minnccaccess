#' Create access map using access data
#'
#' Create child care access map using access data
#'
#' @importFrom magrittr %>%
#'

#' @param data Access data
#' @param n_color Number of colors
#' @param style tmap style
#' @param kid_per_dot Number of kids per dot on the map
#' @param dots Map family locations
#' @param fill Color areas
#' @param ... Other tmao options
#'
#' @return A tmap object
#'
#' @export
#'


map_access <- function(data, n_color = 5, style, kid_per_dot = 4, dots = TRUE, fill = TRUE, ...){

	tmap::tmap_mode("view")

	if(dots){
		families <- simulate_families(data, kid_per_dot)
	}

	palette5 <- c("#db4325", "#eda450", "#e7e2bd", "#54c3ac", "#0c6565") # color palette taken from npr
	palette10 <- c("#a73d37", "#c4684c", "#dd9669", "#ecbc88", "#f9e7ad", "#e6eebc", "#bbd1b1", "#8eb2a5", "#649296", "#417385") # color palette taken from opportunityatlas

	access_max <- ceiling(max(data$seat_per_kid) * 10)/10
	breaks <- seq(0, access_max, access_max/n_color)

	if(n_color == 5){
		palette = palette5
	} else if(n_color == 10){
		palette = palette10
	}

	# breaks  Percentiles with differently populated areas don't make sense.
	#rrr$gps$plot1$tmLayer2$fill.legend.hist.misc$breaks

	if(dots){
		dots_map <- tmap::tm_shape(families) +
			tmap::tm_dots(col = "seat_per_kid", style = style, palette = palette, n = n_color, alpha = 1, border.lwd = 1,
						  breaks = breaks, ...)
	}
	if(fill){
		fill_map <- tmap::tm_shape(data) +
			tmap::tm_polygons(col = "seat_per_kid", style = style, palette = palette, n = n_color, alpha = 0.5,
							  breaks = breaks, lwd = 0.01, ...)
	}

	if(dots & fill){
		access_map <-  dots_map + fill_map
	} else if(dots & !fill){
		access_map <- dots_map
	} else if(!dots & fill){
		access_map <- fill_map
	} else{
		access_map <- NULL
	}

	access_map
}

