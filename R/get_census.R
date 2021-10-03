create_census_url <- function(variable, geography, year, CENSUS_API_KEY){
	if(geography == "census-block"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=block:*&in=state:27&in=county:*&in=tract:*&&key=", CENSUS_API_KEY)
	}

	else if(geography == "census-block-group"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=block%20group:*&in=state:27&in=county:*&in=tract:*&key=", CENSUS_API_KEY)
	}

	else if(geography == "census-tract"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=tract:*&in=state:27&key=", CENSUS_API_KEY)
	}

	else if(geography == "census-place"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=place:*&in=state:27&key=", CENSUS_API_KEY)
	}

	else if(geography == "county"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=county:*&in=state:27&key=", CENSUS_API_KEY)
	}


	else if(geography == "congressional-district"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=congressional%20district:*&in=state:27&key=", CENSUS_API_KEY)
	}

	else if(geography == "state-house-district"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=state%20legislative%20district%20(lower%20chamber):*&in=state:27&key=", CENSUS_API_KEY)
	}

	else if(geography == "state-senate-district"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=state%20legislative%20district%20(upper%20chamber):*&in=state:27&key=", CENSUS_API_KEY)
	}

	else if(geography == "zcta"){
		url <- paste0("https://api.census.gov/data/", year, "/dec/sf1?get=", variable, ",NAME&for=zip%20code%20tabulation%20area%20(or%20part):*&in=state:27&key=", CENSUS_API_KEY)
	}

	url

}

read_url <- function(url){
	a <- jsonlite::fromJSON(url) %>%
		tibble::as_tibble()
	names(a) <- c("var", as.character(a[1, 2:ncol(a)]))
	a <- a[-1, ]
	a

}

census_pivot_wider <- function(data, geography, year = 2010){

	data <- data %>%
		dplyr::select(-.data$url, -.data$variable_cat) %>%
		tidyr::unnest(.data$data) %>%
		dplyr::mutate(var = as.numeric(.data$var)) %>%
		dplyr::select(-.data$variable_code)

	if(geography == "census-block"){
		data <- data %>%
			dplyr::mutate(census_block_2010 = paste0(.data$state, .data$county, .data$tract, .data$block)) %>%
			dplyr::select(-.data$county, -.data$tract, -.data$block)

	} else if(geography == "census-block-group"){
		data <- data %>%
			dplyr::mutate(census_block_group_2010 = paste0(.data$state, .data$county, .data$tract, .data$`block group`)) %>%
			dplyr::select(-.data$county, -.data$tract, -.data$`block group`)

	} else if(geography == "census-tract"){
		data <- data %>%
			dplyr::mutate(census_tract_2010 = paste0(.data$state, .data$county, .data$tract)) %>%
			dplyr::select(-.data$county, -.data$tract)

	} else if(geography == "census-place"){
		data <- data %>%
			dplyr::mutate(census_place_id_2010 = paste0(.data$state, .data$place),
						  census_place_2010 = stringr::str_remove(.data$NAME, " CDP, Minnesota| city, Minnesota")) %>%
			dplyr::select(-.data$place) %>%
			dplyr::filter(!.data$census_place_id_2010 %in% c("2718440", "2739394"))

	} else if(geography == "county"){
		data <- data %>%
			dplyr::mutate(county_id = paste0(.data$state, .data$county),
						  county = stringr::str_remove_all(.data$NAME, " County, Minnesota")) %>%
			dplyr::select(.data$county_id, tidyselect::everything())

	} else if(geography == "zcta"){
		data <- data %>%
			dplyr::rename(zcta_2010 = .data$`zip code tabulation area (or part)`)

	} else if(geography == "congressional-district"){
		data <- data %>%
			dplyr::mutate(congressional_district_id_2012 = as.character(readr::parse_number(.data$`congressional district`))) %>%
			dplyr::select(-.data$`congressional district`)

	} else if(geography == "state-house-district"){
		data <- data %>%
			dplyr::rename(state_house_district_id_2012 = .data$`state legislative district (lower chamber)`)

	} else if(geography == "state-senate-district"){
		data <- data %>%
			dplyr::rename(state_senate_district_id_2012 = .data$`state legislative district (upper chamber)`) %>%
			dplyr::mutate(state_senate_district_id_2012 = stringr::str_sub(.data$state_senate_district_id_2012, 2, 3))
	}

	data <- data %>%
		dplyr::select(-.data$state, -.data$NAME) %>%
		tidyr::pivot_wider(names_from = .data$variable_label,
						   values_from = .data$var)

	data
}


#' Get Decennial Census Data
#'
#' Get Decennial Census data about population, etc.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param variable Census variable
#' @param geography Geographic unit for the data
#' @param year Census vintage
#' @param CENSUS_API_KEY Census API Key
#'
#' @return A tibble of Census data.

#' @export

get_census <- function(variable, geography, year = 2010, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){

	if(!variable %in% c("population", "race", "kids")){
		stop("Invalid variable.")
	}

	if(!geography %in% c("census-block", "census-block-group", "census-tract", "census-place", "county", "zcta", "congressional-district",
						 "state-house-district", "state-senate-district")){
		stop("Invalid geography.")
	}

	if(CENSUS_API_KEY == "" | is.null(CENSUS_API_KEY) | is.na(CENSUS_API_KEY)){
		stop("Provide a valid API Key.")
	}

	vars <- census_vars %>%
		dplyr::filter(.data$variable_cat %in% variable)


	mn <- vars %>%
		dplyr::mutate(url = purrr::map_chr(.data$variable_code, ~create_census_url(.x, geography, year, CENSUS_API_KEY))) %>%
		dplyr::mutate(data = purrr::map(url, ~read_url(.x)))

	mn <- mn %>%
		census_pivot_wider(geography, year)

	if("kids" %in% variable){
		mn <- mn %>%
			dplyr::mutate(population_under5 = .data$under5_male + .data$under5_female) %>%
			dplyr::select(-.data$under5_male, -.data$under5_female)
	}

	mn


}
