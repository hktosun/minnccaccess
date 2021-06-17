create_acs_url <- function(variable, geography, year, CENSUS_API_KEY){

	url <- paste0("https://api.census.gov/data/", year, "/acs/acs5?get=NAME,", variable)

	if(geography == "census-block-group"){
		url <-  paste0(url, "&for=block%20group:*&in=state:27&in=county:*&in=tract:*&key=")
	}

	else if(geography == "census-tract"){
		url <- paste0(url, "&for=tract:*&in=state:27&key=")
	}
	else if(geography == "census-place"){
		url <- paste0(url, "&for=place:*&in=state:27&key=")
	}

	else if(geography == "county"){
		url <- paste0(url, "&for=county:*&in=state:27&key=")
	}

	else if(geography == "congressional-district"){
		url <- paste0(url, "&for=congressional%20district:*&in=state:27&key=")
	}

	else if(geography == "state-senate-district"){
		url <- paste0(url, "&for=state%20legislative%20district%20(upper%20chamber):*&in=state:27&key=")
	}

	else if(geography == "state-house-district"){
		url <- paste0(url, "&for=state%20legislative%20district%20(lower%20chamber):*&in=state:27&key=")
	}

	else if(geography == "zcta"){
		url <- paste0(url, "&for=zip%20code%20tabulation%20area:*&in=state:27&key=")
	}

	else if(geography == "state"){
		url <- paste0(url, "&for=state:27&key=")
	}

	url <- paste0(url, CENSUS_API_KEY)
	url
}

acs_read_url <- function(url){
	a <- jsonlite::fromJSON(url) %>%
		tibble::as_tibble()
	names(a) <- c(a[1, 1], "var", a[1, 3:ncol(a)])
	a <- a[-1, ]

}

acs_pivot_wider <- function(data, geography){

	data <- data %>%
		dplyr::select(-.data$url, -.data$variable_cat) %>%
		tidyr::unnest(.data$data) %>%
		dplyr::mutate(var = as.numeric(.data$var)) %>%
		dplyr::select(-.data$variable_code)

	if(geography == "census-block"){
		data <- data %>%
			dplyr::mutate(census_block_2010 = paste0(.data$state, .data$county, .data$tract, .data$block)) %>%
			dplyr::select(-.data$county, -.data$tract, -.data$block)
	}

	else if(geography == "census-block-group"){
		data <- data %>%
			dplyr::mutate(census_block_group_2010 = paste0(.data$state, .data$county, .data$tract, .data$`block group`)) %>%
			dplyr::select(-.data$county, -.data$tract, -.data$`block group`)
	}

	else if(geography == "census-tract"){
		data <- data %>%
			dplyr::mutate(census_tract_2010 = paste0(.data$state, .data$county, .data$tract)) %>%
			dplyr::select(-.data$county, -.data$tract)
	}

	else if(geography == "census-place"){
		data <- data %>%
			dplyr::mutate(census_place_id_2010 = paste0(.data$state, .data$place)) %>%
			dplyr::select(-.data$place)
	}

	else if(geography == "county"){
		data <- data %>%
			dplyr::mutate(county_id = paste0(.data$state, .data$county),
						  county = stringr::str_remove_all(.data$NAME, " County, Minnesota")) %>%
			dplyr::select(.data$county_id, tidyselect::everything())
	}

	else if(geography == "zcta"){
		data <- data %>%
			dplyr::rename(zcta_2010 = .data$`zip code tabulation area (or part)`)
	}

	else if(geography == "congressional-district"){
		data <- data %>%
			dplyr::mutate(congressional_district_id_2012 = as.character(readr::parse_number(.data$`congressional district`))) %>%
			dplyr::select(-.data$`congressional district`)
	}

	else if(geography == "state-house-district"){
		data <- data %>%
			dplyr::rename(state_house_district_id_2012 = .data$`state legislative district (lower chamber)`)
	}

	else if(geography == "state-senate-district"){
		data <- data %>%
			dplyr::rename(state_senate_district_id_2012 = .data$`state legislative district (upper chamber)`) %>%
			dplyr::mutate(state_senate_district_id_2012 = stringr::str_sub(.data$state_senate_district_id_2012, 2, 3))
	}

	else if(geography == "state"){
		data <- data %>%
			dplyr::mutate(name = .data$NAME)
	}

	data <- data %>%
		dplyr::select(-.data$state, -.data$NAME) %>%
		tidyr::pivot_wider(names_from = .data$variable_label, values_from = .data$var)

	data
}



#' Get ACS Data
#'
#' Get ACS data about population, income, etc.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @param variable ACS variable
#' @param geography Geographic unit for the data
#' @param year ACS 5-year vintage (end year)
#' @param CENSUS_API_KEY Census API Key
#'
#' @return A tibble

#' @export

get_acs <- function(variable, geography, year = 2019, CENSUS_API_KEY = Sys.getenv("CENSUS_API_KEY")){


	if(CENSUS_API_KEY == "" | is.null(CENSUS_API_KEY) | is.na(CENSUS_API_KEY)){
		stop("Provide a valid API Key.")
	}

	vars <- acs_vars %>%
		dplyr::filter(.data$variable_cat %in% variable)


	df <- vars %>%
		dplyr::mutate(url = purrr::map_chr(.data$variable_code, ~create_acs_url(.x, geography, year, CENSUS_API_KEY))) %>%
		dplyr::mutate(data = purrr::map(url, ~acs_read_url(.x)))

	df <- df %>%
		acs_pivot_wider(geography)

	if(variable == "kids"){
		df <- df %>%
			dplyr::mutate(population_under5 = .data$under5_boys + .data$under5_girls) %>%
			dplyr::select(-.data$under5_boys, -.data$under5_girls)
	}

	return(df)

}
