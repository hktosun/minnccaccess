#' Read shapefiles.
#'
#' Read shapefiles from the shared Google Drive folder.
#'
#' @importFrom magrittr %>%
#' @param geography Geographic unit to map. Can be "census-block", "census-block-group", "census-tract", "county", etc.
#' @param year Vintage of the geographic unit. Valid inputs depend on what the geography is.
#' @param gdrive_root The local path to the folder that contains the MinnCCAccess folder.
#'
#' @return An sf object
#' @export


import_map <- function(geography, year, gdrive_root = "~/Google Drive"){

	if(!is.numeric(year)){stop("`year` should be numeric.")}

	subpath <- paste0("/MinnCCAccess/Data Cabinet/Geographic Data/shapefiles/", geography, "/", geography, "_", year, "/", geography, "_" , year, ".dbf")
	path <- paste0(gdrive_root, subpath)

	if(!geography %in% c("cbsa", "census-block", "census-block-group",
						 "census-place", "census-tract", "congressional-district",
						 "county", "mn-house-district", "mn-senate-district",
						 "puma", "school-district", "urban-area", "zcta", "zip-code")){
		stop("Invalid geography.")
	}

	if(geography %in% c("census-block", "census-block-group", "census-tract",
						"census-place", "zcta", "urban-area", "cbsa", "puma")){

		if(!year %in% c(2010, 2020)){
			stop("`year` should be 2010 or 2020 for Census geographies.")
		}

		df <- sf::st_read(path)
		sf::st_crs(df) <- 4326
		df <- sf::st_transform(df, crs = 4326)

		if(year == 2010){
			df <- switch(geography,
						 "census-block"       = dplyr::select(df, census_block_2010 = .data$GEOID10),
						 "census-block-group" = dplyr::select(df, census_block_group_2010 = .data$GEOID),
						 "census-tract"       = dplyr::select(df, census_tract_2010 = .data$GEOID),
						 "census-place"       = dplyr::select(df, census_place_2010 = .data$NAMELSAD),
						 "zcta"               = dplyr::select(df, zcta_2010 = .data$GEOID10),
						 "urban-area"         = dplyr::select(df, urban_type_2010 = .data$UATYP10, urban_area_2010 = .data$NAMELSAD10),
						 "cbsa"               = dplyr::select(df, cbsa_id_2010 = .data$GEOID, cbsa_name = .data$NAME, cbsa_type = .data$LSAD),
						 "puma"               = dplyr::select(df, puma_id_2010 = .data$GEOID10)
			)
		}

		if(year == 2020){
			df <- switch(geography,
						 "census-block"       = dplyr::select(df, census_block_2020 = .data$GEOID20),
						 "census-block-group" = dplyr::select(df, census_block_group_2020 = .data$GEOID20),
						 "census-tract"       = dplyr::select(census_tract_2020 = .data$GEOID20),
						 "census-place"       = dplyr::select(df, census_place_2020 = .data$NAMELSAD20),
						 "zcta"               = dplyr::select(df, zcta_2020 = .data$GEOID20),
						 "urban-area"         = dplyr::select(df, urban_type_2020 = .data$UATYP10, urban_area_2020 = .data$NAMELSAD20),
						 "cbsa"               = dplyr::select(df, cbsa_id_2020 = .data$GEOID, cbsa_type = .data$LSAD),
						 "puma"               = dplyr::select(df, puma_id_2020 = .data$GEOID20)
			)
		}
	}

	else if(geography %in% c("congressional-district", "mn-house-district", "mn-senate-district")){

		if(!year %in% c(2012)){
			stop("`year` should be 2012 for political geographies.")
		}

		df <- sf::st_read(path) %>%
			sf::st_transform("+proj=longlat +datum=WGS84")
		sf::st_crs(df) <- 4326
		df <- sf::st_transform(df, crs = 4326)

		if(year == 2012){
			df <- switch(geography,
						 "congressional-district" = dplyr::select(df, congressional_district_id_2012 = .data$DISTRICT),
						 "mn-house-district"      = dplyr::select(df, mn_house_district_id_2012 = .data$district),
						 "mn-senate-district"     = dplyr::select(df, mn_senate_district_id_2012 = .data$district)
			)
		}

	}

	else if(geography == "county"){
		df <- sf::st_read(path) %>%
			sf::st_transform("+proj=longlat +datum=WGS84") %>%
			dplyr::filter(.data$STATEFP == 27)

		st_crs(df) <- 4326

		df <- df %>%
			sf::st_transform(crs = 4326) %>%
			dplyr::select(county = .data$NAME)
	}

	else if(geography == "school-district"){

		if(!dplyr::between(year, 2011, 2021)) {
			stop("Year should be between 2011 and 2021.")
		}

		namekey <- c(`SDNAME` = "school_district_name",
					 `UNI_NAM` = "school_district_name",
					 `UNI_TYP` = "school_district_type",
					 `UNI_MAJ` = "school_district_number",
					 `SDNUM` = "school_district_number",
					 `geometry` = "geometry")

		df <- sf::st_read(path) %>%
			st_transform("+proj=longlat +datum=WGS84")

		st_crs(df) <- 4326

		df <- df %>%
			sf::st_transform(crs = 4326) %>%
			dplyr::select(tidyselect::one_of("SDNAME", "UNI_NAM", "UNI_TYP", "UNI_MAJ", "SDNUM"))

		colnames(df) <- namekey[names(df)]

		df <- df %>%
			dplyr::mutate(school_district_number = stringr::str_pad(.data$school_district_number, 4, side = "left", pad = "0"),
						  school_district_type = stringr::str_pad(.data$school_district_type, 2, side = "left", pad = "0")) %>%
			dplyr::mutate(school_district_id = paste0(.data$school_district_number, "-", .data$school_district_type)) %>%
			dplyr::select(.data$school_district_id)
	}

	else if(geography == "zip-code"){

		subpath <- paste0("/MinnCCAccess/Data Cabinet/Geographic Data/shapefiles/zip-code/zip-code_", year, "/zip_poly.gdb")
		path <- paste0(gdrive_root, subpath)
		df <- rgdal::readOGR(dsn = path) %>%
			sf::st_as_sf() %>%
			dplyr::filter(.data$STATE == "MN") %>%
			dplyr::select(zipcode = .data$ZIP_CODE)

	}
	df
}

