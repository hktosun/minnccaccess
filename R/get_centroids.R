#' Get population-weighted centroids for Census geographies.

#' Get centroids for Census geographies like census block group, census tract, and for some other geographies.

#' @importFrom magrittr %>%
#'
#' @param geography Geographical unit
#' @param year Vintage
#'
#' @export
#'
get_centroids <- function(geography, year = 2010){

	if(year != 2010){
		stop("Only year = 2010 is available (for now).")
	}

	if(!geography %in% c("county", "census-tract", "census-block-group")){
		stop("Invalid geography. Try `county`, `census-tract`, or `census-block-group`.")
	}
	if(geography == "county"){
		if(year == 2010){
			df <- readr::read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO27.txt")

			df <- df %>%
				dplyr::mutate(county_id = paste0(.data$STATEFP, .data$COUNTYFP),
							  county = .data$COUNAME) %>%
				dplyr::select(.data$county, .data$county_id, population = .data$POPULATION, lat = .data$LATITUDE, lon = .data$LONGITUDE)

		}
	}

	else if(geography == "census-tract"){
		if(year == 2010){
			df <- readr::read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/tract/CenPop2010_Mean_TR27.txt")

			df <- df %>%
				dplyr::mutate(census_tract_2010 = paste0(.data$STATEFP, .data$COUNTYFP, .data$TRACTCE)) %>%
				dplyr::select(.data$census_tract_2010, population = .data$POPULATION, lat = .data$LATITUDE, lon = .data$LONGITUDE)
		}
	}


	else if(geography == "census-block-group"){
		if(year == 2010){
			df <- readr::read_csv("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG27.txt")

			df <- df %>%
				dplyr::mutate(census_block_group_2010 = paste0(.data$STATEFP, .data$COUNTYFP, .data$TRACTCE, .data$BLKGRPCE)) %>%
				dplyr::select(.data$census_block_group_2010, population = .data$POPULATION, lat = .data$LATITUDE, lon = .data$LONGITUDE)
		}
	}

	df
}
