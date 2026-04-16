# global.R
# Shared functions, data, and libraries
library(shiny)
library(bslib)
library(mapgl)
library(magrittr)
library(bsicons)
library(dplyr)
library(sf)
library(DT)
library(plotly)
library(readxl)
library(jsonlite)
library(httr)
if (!requireNamespace("arrow", quietly = TRUE)) {
  stop(
    "Package 'arrow' is required for the Parquet inventory cache. Install it with install.packages(\"arrow\").",
    call. = FALSE
  )
}
source("funs/plots.R")
source("funs/api_utils.R")
source("funs/build_pmtiles.R")

stations_pmtiles_path <- file.path("www", "stations.pmtiles")
stations_mvt_tiles <- character(0)
stations_tiles_source_layer <- "stations"
inventory_cache_parquet_path <- file.path("data", "api_inventory_cache.parquet")

stations_pmtiles_url <- if (file.exists(stations_pmtiles_path)) "session_dynamic" else ""

stations_render_mode <- if (stations_pmtiles_url != "") {
  "pmtiles"
} else if (length(stations_mvt_tiles) > 0) {
  "mvt"
} else {
  "geojson"
}

print("Loading subdaily inventory dataset...")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

first_non_missing <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  x <- unlist(x, recursive = TRUE, use.names = FALSE)
  if (length(x) == 0) {
    return(default)
  }
  idx <- which(!is.na(x) & as.character(x) != "")
  if (length(idx) == 0) default else as.character(x[idx[1]])
}

infer_continent_from_country <- function(country_name) {
  country_name <- trimws(as.character(country_name %||% ""))
  if (country_name == "") return(NA_character_)

  # Hardcoded fallback for environments without `countrycode`
  continent_map <- c(
    "United States of America" = "North America", "Angola" = "Africa", "Australia" = "Australia",
    "Kazakhstan" = "Asia", "China" = "Asia", "Germany" = "Europe", "South Africa" = "Africa",
    "Russian Federation" = "Europe", "Haiti" = "North America", "Morocco" = "Africa", "Morroco" = "Africa",
    "U.K. of Great Britain and Northern Ireland" = "Europe", "Uzbekistan" = "Asia", "Estonia" = "Europe",
    "Tunisia" = "Africa", "Sudan" = "Africa", "Norway" = "Europe", "Algeria" = "Africa",
    "Bolivia" = "South America", "Iceland" = "Europe", "Ecuador" = "South America", "Ukraine" = "Europe",
    "Mongolia" = "Asia", "United Arab Emirates" = "Asia", "Reunion" = "Africa", "Argentina" = "South America",
    "Italy" = "Europe", "Iraq" = "Asia", "United Republic of Tanzania" = "Africa", "Thailand" = "Asia",
    "Japan" = "Asia", "Brazil" = "South America", "Egypt" = "Africa", "Mozambique" = "Africa",
    "Portugal" = "Europe", "Afghanistan" = "Asia", "France" = "Europe", "Canada" = "North America",
    "Singapore" = "Asia", "Romania" = "Europe", "Oman" = "Asia", "Spain" = "Europe", "Sweden" = "Europe",
    "Swaziland" = "Africa", "Finland" = "Europe", "Vietnam" = "Asia", "Poland" = "Europe", "Pakistan" = "Asia",
    "Cameroon" = "Africa", "Azerbaijan" = "Asia", "Guam" = "Oceania", "Cuba" = "North America",
    "Libyan Arab Jamahiriya" = "Africa", "Tajikistan" = "Asia", "Denmark" = "Europe", "Slovenia" = "Europe",
    "Turkey" = "Asia", "Greece" = "Europe", "Iran (Islamic Republic of)" = "Asia", "Bosnia & Herzegovina" = "Europe",
    "Congo" = "Africa", "Malawi" = "Africa", "Greenland" = "North America", "Mauritania" = "Africa",
    "Ghana" = "Africa", "Switzerland" = "Europe", "Zimbabwe" = "Africa", "Mexico" = "North America",
    "Venezuela" = "South America", "Armenia" = "Asia", "India" = "Asia", "Bahamas" = "North America",
    "Madagascar" = "Africa", "Republic of Korea" = "Asia", "Mali" = "Africa", "Belgium" = "Europe",
    "Philippines" = "Asia", "The former Yugoslav Republic of Macedonia" = "Europe", "New Zealand" = "Oceania",
    "Kenya" = "Africa", "Lao People's Democratic Republic" = "Asia", "Democratic People's Republic of Korea" = "Asia",
    "Lithuania" = "Europe", "Slovakia" = "Europe", "Svalbard and Jan Mayen Islands" = "Europe",
    "Namibia" = "Africa", "French Polynesia" = "Oceania", "Colombia" = "South America", "Austria" = "Europe",
    "Paraguay" = "South America", "Belarus" = "Europe", "Croatia" = "Europe", "Nigeria" = "Africa",
    "Moldova, Republic of" = "Europe", "Myanmar" = "Asia", "Saudi Arabia" = "Asia", "Czech Republic" = "Europe",
    "Peru" = "South America", "Kiribati" = "Oceania", "Niger" = "Africa", "Chile" = "South America",
    "Hungary" = "Europe", "Puerto Rico" = "North America", "Syrian Arab Republic" = "Asia",
    "Martinique" = "North America", "Serbia" = "Europe", "Rwanda" = "Africa", "Indonesia" = "Asia",
    "Israel" = "Asia", "Guinea-Bissau" = "Africa", "Guyana" = "South America", "Vanuatu" = "Oceania",
    "Turkmenistan" = "Asia", "Dominican Republic" = "North America", "Antigua & Barbuda" = "North America",
    "Bulgaria" = "Europe", "Qatar" = "Asia", "Malaysia" = "Asia", "Nepal" = "Asia", "Sri Lanka" = "Asia",
    "Papua New Guinea" = "Oceania", "Georgia" = "Asia", "Bangladesh" = "Asia", "Taiwan" = "Asia",
    "Montenegro" = "Europe", "Gambia" = "Africa", "Jammu-Kashmir" = "Asia", "Burkina Faso" = "Africa",
    "Uruguay" = "South America", "Ireland" = "Europe", "Midway Is." = "Oceania", "Chad" = "Africa",
    "Kuwait" = "Asia", "Macao" = "Asia", "Sierra Leone" = "Africa", "C\U0099te d'Ivoire" = "Africa",
    "Latvia" = "Europe", "Somalia" = "Africa", "Democratic Republic of the Congo" = "Africa",
    "Brunei Darussalam" = "Asia", "Senegal" = "Africa", "Hong Kong" = "Asia", "Zambia" = "Africa",
    "Guinea" = "Africa", "Tonga" = "Oceania", "Falkland Islands (Malvinas)" = "South America",
    "Guadeloupe" = "North America", "Marshall Islands" = "Oceania", "Panama" = "North America",
    "Kyrgyzstan" = "Asia", "Uganda" = "Africa", "Arunachal Pradesh" = "Asia", "Jamaica" = "North America",
    "Netherlands" = "Europe", "Cambodia" = "Asia", "Togo" = "Africa", "Pitcairn Island" = "Oceania",
    "New Caledonia" = "Oceania", "Albania" = "Europe", "French Guiana" = "South America",
    "El Salvador" = "North America", "Cayman Islands" = "North America", "Costa Rica" = "North America",
    "Suriname" = "South America", "Sao Tome and Principe" = "Africa", "Gaza Strip" = "Asia",
    "Seychelles" = "Africa", "South Sudan" = "Africa", "Eritrea" = "Africa", "Cyprus" = "Asia",
    "Guatemala" = "North America", "Yemen" = "Asia", "Botswana" = "Africa", "Cook Islands" = "Oceania",
    "Jordan" = "Asia", "Nicaragua" = "North America", "Mayotte" = "Africa", "Lesotho" = "Africa",
    "Lebanon" = "Asia", "Djibouti" = "Africa", "Tuvalu" = "Oceania", "Niue" = "Oceania",
    "Honduras" = "North America", "Mauritius" = "Africa", "Fiji" = "Oceania", "Micronesia (Federated States of)" = "Oceania",
    "French Southern and Antarctic Territories" = "Antarctica", "Palau" = "Oceania", "Gabon" = "Africa",
    "Central African Republic" = "Africa", "Norfolk Island" = "Oceania", "Christmas Island" = "Australia", "Cocos (Keeling) Islands" = "Australia", "Benin" = "Africa",
    "American Samoa" = "Oceania", "Netherlands Antilles" = "North America", "Bahrain" = "Asia",
    "Ethiopia" = "Africa", "Northern Mariana Islands" = "Oceania",
    "Azores Islands" = "Europe", "Malta" = "Europe", "Cape Verde" = "Africa", "Nauru" = "Oceania",
    "Luxembourg" = "Europe", "West Bank" = "Asia", "British Indian Ocean Territory" = "Asia",
    "Isle of Man" = "Europe", "Trinidad and Tobago" = "North America", "South Georgia & the South Sandwich Islands" = "Antarctica",
    "Madeira Islands" = "Europe", "Dominica" = "North America", "Belize" = "North America", "Montserrat" = "North America",
    "Timor-Leste" = "Asia", "Barbados" = "North America", "United States Virgin Islands" = "North America",
    "Liechtenstein" = "Europe", "Comoros" = "Africa", "Samoa" = "Oceania",
    "Hala'ib Triangle" = "Africa", "Kuril Islands" = "Asia"
  )

  if (country_name %in% names(continent_map)) {
    return(as.character(continent_map[country_name]))
  }

  if (requireNamespace("countrycode", quietly = TRUE)) {
    continent <- suppressWarnings(countrycode::countrycode(country_name, "country.name", "continent"))
    if (!is.na(continent) && continent != "") {
      return(as.character(continent))
    }
  }

  NA_character_
}

infer_continent_from_coords <- function(lon, lat) {
  if (!is.finite(lon) || !is.finite(lat)) {
    return(NA_character_)
  }

  if (lat <= -60) return("Antarctica")
  if (lon >= -170 && lon <= -20 && lat >= 5 && lat <= 85) return("North America")
  if (lon >= -95 && lon <= -30 && lat >= -60 && lat < 15) return("South America")
  if (lon >= -25 && lon <= 60 && lat >= 34 && lat <= 72) return("Europe")
  if (lon >= -20 && lon <= 55 && lat >= -35 && lat <= 38) return("Africa")
  if (lon >= 112 && lon <= 155 && lat >= -45 && lat <= -10) return("Australia")
  if (lon >= 110 && lon <= 180 && lat >= -50 && lat <= 5) return("Oceania")
  if (lon >= 25 && lon <= 180 && lat >= -10 && lat <= 82) return("Asia")

  NA_character_
}

extract_api_records <- function(api_payload) {
  if (is.null(api_payload)) {
    return(list())
  }

  candidates <- list(
    api_payload$metadata,
    api_payload$records,
    api_payload$items,
    api_payload$hits$hits,
    api_payload$hits
  )

  for (candidate in candidates) {
    if (is.list(candidate) && length(candidate) > 0) {
      return(candidate)
    }
  }

  list()
}

record_to_inventory_row <- function(rec) {
  geom_coords <- NULL
  if (is.list(rec$geom) && length(rec$geom) > 0 && !is.null(rec$geom[[1]]$coordinates)) {
    geom_coords <- rec$geom[[1]]$coordinates
  }

  coords <- rec$geometry$coordinates %||% rec$geom$coordinates %||% geom_coords %||% list(NA_real_, NA_real_)
  lon_coord <- if (length(coords) >= 1) coords[[1]] else NA_real_
  lat_coord <- if (length(coords) >= 2) coords[[2]] else NA_real_

  abstract_text <- first_non_missing(list(
    rec$resourceAbstractObject$default,
    rec$resourceAbstract,
    rec$abstract
  ), default = "")

  lat_from_abstract <- suppressWarnings(as.numeric(sub(".*LAT:\\s*([-0-9.]+).*", "\\1", abstract_text)))
  if (!grepl("LAT:", abstract_text, fixed = TRUE)) {
    lat_from_abstract <- NA_real_
  }
  lon_from_abstract <- suppressWarnings(as.numeric(sub(".*LONG:\\s*([-0-9.]+).*", "\\1", abstract_text)))
  if (!grepl("LONG:", abstract_text, fixed = TRUE)) {
    lon_from_abstract <- NA_real_
  }
  elev_from_abstract <- suppressWarnings(as.numeric(sub(".*HEIGHT:\\s*([-0-9.]+).*", "\\1", abstract_text)))
  if (!grepl("HEIGHT:", abstract_text, fixed = TRUE)) {
    elev_from_abstract <- NA_real_
  }

  country_from_abstract <- suppressWarnings(sub(".*COUNTRY:\\s*([^:]+?)\\s+LAT:.*", "\\1", abstract_text))
  if (identical(country_from_abstract, abstract_text)) {
    country_from_abstract <- NA_character_
  }

  temporal <- rec$resourceTemporalDateRange %||% rec$resourceTemporalExtentDateRange %||% list()
  temporal_gte <- if (is.list(temporal) && length(temporal) > 0) temporal[[1]]$gte else NA_character_
  temporal_lte <- if (is.list(temporal) && length(temporal) > 0) temporal[[1]]$lte else NA_character_

  station_name <- first_non_missing(list(
    rec$station_name,
    rec$resourceTitle,
    rec$resourceTitleObject$default,
    rec$title,
    rec$name
  ), default = "Unknown station")

  # Extract and normalize observed variables from the API record
  obs_vars_raw <- rec$observed_variables
  if (is.null(obs_vars_raw) || length(obs_vars_raw) == 0) {
    if (!is.null(rec$tag)) {
      extracted <- sapply(rec$tag, function(t) t$default %||% t$langNaN %||% "")
      extracted <- extracted[extracted != "" & extracted != "climatologyMeteorologyAtmosphere"]
      obs_vars_raw <- as.list(extracted)
    } else {
      obs_vars_raw <- list()
    }
  }

  obs_vars_str <- if (is.list(obs_vars_raw)) {
    paste(unlist(obs_vars_raw), collapse = ", ")
  } else {
    as.character(obs_vars_raw)
  }
  obs_vars_str <- gsub("_", " ", obs_vars_str)
  if (obs_vars_str == "") obs_vars_str <- "Unknown"

  country <- first_non_missing(list(rec$country, rec$location$country, country_from_abstract), default = "Unknown")
  region <- first_non_missing(list(rec$region, rec$location$region), default = "")

  longitude <- suppressWarnings(as.numeric(first_non_missing(list(
    rec$lon,
    rec$longitude,
    lon_coord,
    lon_from_abstract
  ), default = NA_real_)))

  latitude <- suppressWarnings(as.numeric(first_non_missing(list(
    rec$lat,
    rec$latitude,
    lat_coord,
    lat_from_abstract
  ), default = NA_real_)))

  inferred_continent_country <- infer_continent_from_country(country)
  inferred_continent_coords <- infer_continent_from_coords(longitude, latitude)
  continent <- first_non_missing(
    list(rec$continent, rec$location$continent, inferred_continent_country, inferred_continent_coords),
    default = "Unknown"
  )

  is_open_data <- tolower(first_non_missing(list(rec$isOpenData), default = ""))
  policy_from_open <- if (is_open_data %in% c("true", "1", "yes")) {
    "Open Data"
  } else if (is_open_data %in% c("false", "0", "no")) {
    "Restricted"
  } else {
    NA_character_
  }

  start_year <- suppressWarnings(as.integer(first_non_missing(list(
    rec$start_year,
    rec$begin,
    rec$properties$start_year,
    substr(temporal_gte, 1, 4),
    substr(rec$createDate, 1, 4)
  ), default = NA_integer_)))

  end_year <- suppressWarnings(as.integer(first_non_missing(list(
    rec$end_year,
    rec$end,
    rec$properties$end_year,
    substr(temporal_lte, 1, 4),
    substr(rec$changeDate, 1, 4)
  ), default = NA_integer_)))

  data.frame(
    station_name = station_name,
    country = country,
    continent = continent,
    region = region,
    longitude = longitude,
    latitude = latitude,
    elevation = suppressWarnings(as.numeric(first_non_missing(list(rec$elevation, rec$elev, elev_from_abstract), default = NA_real_))),
    wmo_id = first_non_missing(list(rec$wmo_id, rec$WMO_ID), default = NA_character_),
    station_id = first_non_missing(list(rec$station_id, rec$uuid, rec$id), default = NA_character_),
    fips_code = first_non_missing(list(rec$fips_code), default = NA_character_),
    source_name = first_non_missing(list(rec$source_name, rec$source), default = "GeoNetwork API"),
    source_uid = first_non_missing(list(rec$source_uid), default = NA_character_),
    data_type = first_non_missing(list(rec$data_type), default = NA_character_),
    data_policy = first_non_missing(list(rec$data_policy, rec$station_data_policy, policy_from_open), default = NA_character_),
    update_status = first_non_missing(list(rec$update_status), default = NA_character_),
    timestep = first_non_missing(list(rec$timestep), default = NA_character_),
    obs_freq = first_non_missing(list(rec$obs_freq), default = NA_character_),
    platform_type = first_non_missing(list(rec$platform_type), default = NA_character_),
    station_status = first_non_missing(list(rec$station_status), default = NA_character_),
    start_year = start_year,
    end_year = end_year,
    observed_variables = obs_vars_str,
    has_t = grepl("Temperature", obs_vars_str, ignore.case = TRUE),
    has_pp = grepl("Precipitation", obs_vars_str, ignore.case = TRUE),
    has_sd = grepl("Sunshine", obs_vars_str, ignore.case = TRUE),
    has_ws = grepl("Wind Speed", obs_vars_str, ignore.case = TRUE) | (grepl("Wind", obs_vars_str, ignore.case = TRUE) & !grepl("Direction", obs_vars_str, ignore.case = TRUE)),
    has_wd = grepl("Wind Direction", obs_vars_str, ignore.case = TRUE),
    has_slp = grepl("Sea Level Pressure", obs_vars_str, ignore.case = TRUE),
    has_dpt = grepl("Dew Point", obs_vars_str, ignore.case = TRUE),
    has_wbt = grepl("Wet Bulb", obs_vars_str, ignore.case = TRUE),
    has_rh = grepl("Relative Humidity", obs_vars_str, ignore.case = TRUE),
    has_sc = grepl("Snow Cover", obs_vars_str, ignore.case = TRUE),
    has_snow = grepl("Snow Depth", obs_vars_str, ignore.case = TRUE),
    has_cc = grepl("Cloud Cover", obs_vars_str, ignore.case = TRUE),
    has_sp = grepl("Station Pressure", obs_vars_str, ignore.case = TRUE),
    stringsAsFactors = FALSE
  )
}

build_popup_content <- function(data) {
  paste0(
    "<b>", data$station_name, "</b><br>",
    "Country: ", data$country, " | Continent: ", data$continent, "<br>",
    "Elevation: ", data$elevation, " m<br>",
    "Period: ", data$start_year, " - ", data$end_year, "<br>",
    "Variables: ", data$observed_variables
  )
}

process_api_inventory <- function(base_url = "https://stationhub.geo.meteoromania.ro/geonetwork", page_size = 5000L, max_records = 50000L, strict = FALSE, progress_callback = NULL) {
  if (base_url == "") {
    if (strict) {
      stop("GeoNetwork API load failed: base URL is not configured.", call. = FALSE)
    }
    return(NULL)
  }

  endpoint <- paste0(sub("/$", "", base_url), "/srv/api/search/records/_search")
  
  all_records <- list()
  current_from <- 0
  last_sort_value <- NULL
  
  while (current_from < max_records) {
    request_size <- min(as.integer(page_size), max_records - current_from)
    
    body <- list(
      size = request_size,
      sort = list(list("id" = "asc")),
      query = list(
        bool = list(
          filter = list(
            list(term = list(isTemplate = list(value = "n")))
          )
        )
      ),
      `_source` = list(
        includes = c(
          "uuid", "id", "resourceType", "resourceTitle", "resourceTitleObject.default",
          "resourceAbstractObject.default", "resourceAbstract", "title", "name",
          "station_name", "country", "continent", "region", "location.country",
          "location.continent", "location.region", "lon", "lat", "longitude",
          "latitude", "geometry.coordinates", "geom.coordinates", "elevation",
          "elev", "wmo_id", "WMO_ID", "station_id", "fips_code", "source_name",
          "source_uid", "source", "data_type", "data_policy", "station_data_policy",
          "isOpenData", "update_status", "timestep", "obs_freq", "platform_type",
          "station_status", "observed_variables", "start_year", "end_year",
          "createDate", "changeDate", "begin", "end", "resourceTemporalDateRange.gte",
          "resourceTemporalDateRange.lte", "resourceTemporalExtentDateRange.gte",
          "resourceTemporalExtentDateRange.lte", "properties.start_year",
          "properties.end_year", "tag"
        )
      )
    )

    if (!is.null(last_sort_value)) {
      body$search_after <- I(unlist(last_sort_value))
    }

    response <- tryCatch(
      POST(endpoint, body = body, encode = "json"),
      error = function(e) {
        if (strict && length(all_records) == 0) {
          stop(paste0("GeoNetwork API load failed: ", e$message), call. = FALSE)
        }
        message("GeoNetwork API load failed: ", e$message)
        NULL
      }
    )

    if (is.null(response)) break

    if (status_code(response) >= 400) {
      err_msg <- paste0("GeoNetwork API load failed: HTTP ", status_code(response), " from /srv/api/search/records/_search")
      if (strict && length(all_records) == 0) {
        stop(err_msg, call. = FALSE)
      }
      message(err_msg)
      break
    }

    payload <- tryCatch(
      fromJSON(content(response, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE),
      error = function(e) {
        if (strict && length(all_records) == 0) {
          stop(paste0("GeoNetwork API load failed: invalid JSON response: ", e$message), call. = FALSE)
        }
        message("GeoNetwork API load failed: invalid JSON response: ", e$message)
        NULL
      }
    )

    if (is.null(payload)) break

    records <- extract_api_records(payload)
    if (length(records) == 0) break

    all_records <- c(all_records, records)
    current_from <- current_from + length(records)
    last_sort_value <- records[[length(records)]]$sort
    msg <- sprintf("  Fetched %d records (total: %d)", length(records), current_from)
    message(msg)
    
    if (!is.null(progress_callback)) {
      # Use a scaling factor so fetching takes up roughly 0.4 to 0.7 of the progress bar
      progress_callback(0.4 + (current_from / max_records) * 0.3, detail = msg)
    }
    
    if (length(records) < request_size) break
  }

  if (length(all_records) == 0) {
    return(NULL)
  }

  # --- Vectorized record processing (replaces slow per-record lapply) ---
  sources <- lapply(all_records, function(rec) rec$`_source` %||% rec)

  # Pre-build a country -> continent lookup to avoid calling countrycode 20k times
  country_continent_cache <- new.env(hash = TRUE, parent = emptyenv())

  infer_continent_cached <- function(country_name) {
    country_name <- trimws(as.character(country_name %||% ""))
    if (country_name == "") return(NA_character_)
    cached <- country_continent_cache[[country_name]]
    if (!is.null(cached)) return(cached)
    result <- infer_continent_from_country(country_name)
    country_continent_cache[[country_name]] <- result
    result
  }

  message("  Processing records...")
  if (!is.null(progress_callback)) {
    progress_callback(0.75, detail = "Processing station records...")
  }
  rows <- lapply(sources, function(rec) {
    # Coordinates
    geom_coords <- NULL
    if (is.list(rec$geom) && length(rec$geom) > 0 && !is.null(rec$geom[[1]]$coordinates)) {
      geom_coords <- rec$geom[[1]]$coordinates
    }
    coords <- rec$geometry$coordinates %||% rec$geom$coordinates %||% geom_coords %||% list(NA_real_, NA_real_)
    lon_coord <- if (length(coords) >= 1) coords[[1]] else NA_real_
    lat_coord <- if (length(coords) >= 2) coords[[2]] else NA_real_

    # Abstract parsing
    abstract_text <- rec$resourceAbstractObject$default %||% rec$resourceAbstract %||% rec$abstract %||% ""
    if (is.null(abstract_text) || length(abstract_text) == 0) abstract_text <- ""

    has_lat <- grepl("LAT:", abstract_text, fixed = TRUE)
    lat_abs <- if (has_lat) suppressWarnings(as.numeric(sub(".*LAT:\\s*([-0-9.]+).*", "\\1", abstract_text))) else NA_real_
    has_lon <- grepl("LONG:", abstract_text, fixed = TRUE)
    lon_abs <- if (has_lon) suppressWarnings(as.numeric(sub(".*LONG:\\s*([-0-9.]+).*", "\\1", abstract_text))) else NA_real_
    has_ht <- grepl("HEIGHT:", abstract_text, fixed = TRUE)
    elev_abs <- if (has_ht) suppressWarnings(as.numeric(sub(".*HEIGHT:\\s*([-0-9.]+).*", "\\1", abstract_text))) else NA_real_

    country_abs <- suppressWarnings(sub(".*COUNTRY:\\s*([^:]+?)\\s+LAT:.*", "\\1", abstract_text))
    if (identical(country_abs, abstract_text)) country_abs <- NA_character_

    # Temporal
    temporal <- rec$resourceTemporalDateRange %||% rec$resourceTemporalExtentDateRange %||% list()
    temporal_gte <- if (is.list(temporal) && length(temporal) > 0) temporal[[1]]$gte else NA_character_
    temporal_lte <- if (is.list(temporal) && length(temporal) > 0) temporal[[1]]$lte else NA_character_

    station_name <- rec$station_name %||% rec$resourceTitle %||% rec$resourceTitleObject$default %||% rec$title %||% rec$name %||% "Unknown station"
    if (is.list(station_name)) station_name <- station_name[[1]]
    station_name <- as.character(station_name)
    if (is.null(station_name) || length(station_name) == 0 || is.na(station_name) || station_name == "") station_name <- "Unknown station"

    # Observed variables from tag
    obs_vars_raw <- rec$observed_variables
    if (is.null(obs_vars_raw) || length(obs_vars_raw) == 0) {
      if (!is.null(rec$tag)) {
        extracted <- vapply(rec$tag, function(t) t$default %||% t$langNaN %||% "", FUN.VALUE = character(1))
        extracted <- extracted[extracted != "" & extracted != "climatologyMeteorologyAtmosphere"]
        obs_vars_raw <- as.list(extracted)
      } else {
        obs_vars_raw <- list()
      }
    }
    obs_vars_str <- paste(unlist(obs_vars_raw), collapse = ", ")
    obs_vars_str <- gsub("_", " ", obs_vars_str)
    if (obs_vars_str == "") obs_vars_str <- "Unknown"

    country <- rec$country %||% rec$location$country %||% country_abs %||% "Unknown"
    if (is.null(country) || is.na(country) || country == "") country <- "Unknown"
    region <- rec$region %||% rec$location$region %||% ""
    if (is.null(region) || is.na(region)) region <- ""

    longitude <- suppressWarnings(as.numeric(rec$lon %||% rec$longitude %||% lon_coord %||% lon_abs %||% NA_real_))
    latitude <- suppressWarnings(as.numeric(rec$lat %||% rec$latitude %||% lat_coord %||% lat_abs %||% NA_real_))

    continent <- rec$continent %||% rec$location$continent %||% NA_character_
    if (is.na(continent) || continent == "") continent <- infer_continent_cached(country)
    if (is.na(continent) || continent == "") continent <- infer_continent_from_coords(longitude, latitude)
    if (is.null(continent) || is.na(continent) || continent == "") continent <- "Unknown"

    is_open <- tolower(rec$isOpenData %||% "")
    policy <- if (is_open %in% c("true", "1", "yes")) "Open Data" else if (is_open %in% c("false", "0", "no")) "Restricted" else NA_character_

    start_year <- suppressWarnings(as.integer(rec$start_year %||% rec$begin %||% rec$properties$start_year %||% substr(temporal_gte %||% "", 1, 4) %||% NA_integer_))
    end_year <- suppressWarnings(as.integer(rec$end_year %||% rec$end %||% rec$properties$end_year %||% substr(temporal_lte %||% "", 1, 4) %||% NA_integer_))

    list(
      station_name = station_name,
      country = country,
      continent = continent,
      region = region,
      longitude = longitude,
      latitude = latitude,
      elevation = suppressWarnings(as.numeric(rec$elevation %||% rec$elev %||% elev_abs %||% NA_real_)),
      wmo_id = rec$wmo_id %||% rec$WMO_ID %||% NA_character_,
      station_id = rec$station_id %||% rec$uuid %||% rec$id %||% NA_character_,
      fips_code = rec$fips_code %||% NA_character_,
      source_name = rec$source_name %||% rec$source %||% "GeoNetwork API",
      source_uid = rec$source_uid %||% NA_character_,
      data_type = rec$data_type %||% NA_character_,
      data_policy = rec$data_policy %||% rec$station_data_policy %||% policy %||% NA_character_,
      update_status = rec$update_status %||% NA_character_,
      timestep = rec$timestep %||% NA_character_,
      obs_freq = rec$obs_freq %||% NA_character_,
      platform_type = rec$platform_type %||% NA_character_,
      station_status = rec$station_status %||% NA_character_,
      start_year = start_year,
      end_year = end_year,
      observed_variables = obs_vars_str,
      has_t = grepl("Temperature", obs_vars_str, ignore.case = TRUE),
      has_pp = grepl("Precipitation", obs_vars_str, ignore.case = TRUE),
      has_sd = grepl("Sunshine", obs_vars_str, ignore.case = TRUE),
      has_ws = grepl("Wind Speed", obs_vars_str, ignore.case = TRUE) | (grepl("Wind", obs_vars_str, ignore.case = TRUE) & !grepl("Direction", obs_vars_str, ignore.case = TRUE)),
      has_wd = grepl("Wind Direction", obs_vars_str, ignore.case = TRUE),
      has_slp = grepl("Sea Level Pressure", obs_vars_str, ignore.case = TRUE),
      has_dpt = grepl("Dew Point", obs_vars_str, ignore.case = TRUE),
      has_wbt = grepl("Wet Bulb", obs_vars_str, ignore.case = TRUE),
      has_rh = grepl("Relative Humidity", obs_vars_str, ignore.case = TRUE),
      has_sc = grepl("Snow Cover", obs_vars_str, ignore.case = TRUE),
      has_snow = grepl("Snow Depth", obs_vars_str, ignore.case = TRUE),
      has_cc = grepl("Cloud Cover", obs_vars_str, ignore.case = TRUE),
      has_sp = grepl("Station Pressure", obs_vars_str, ignore.case = TRUE)
    )
  })

  api_data <- bind_rows(rows)

  # Flatten any list columns to character (prevents geojsonsf serialization errors)
  list_cols <- names(which(vapply(api_data, is.list, logical(1))))
  for (lc in list_cols) {
    api_data[[lc]] <- vapply(api_data[[lc]], function(x) as.character(x[[1]] %||% NA_character_), character(1))
  }

  api_data <- api_data %>%
    mutate(
      country = trimws(country),
      continent = trimws(continent),
      region = trimws(as.character(region)),
      data_type = trimws(data_type),
      data_policy = trimws(data_policy),
      update_status = trimws(update_status),
      timestep = trimws(timestep),
      obs_freq = trimws(obs_freq),
      platform_type = trimws(platform_type),
      station_status = trimws(station_status)
    )

  if (!"observed_variables" %in% names(api_data)) {
    api_data$observed_variables <- "Unknown"
  }

  api_data$popup_content <- build_popup_content(api_data)

  api_data
}

empty_inventory_data <- function() {
  data.frame(
    station_name = character(),
    country = character(),
    continent = character(),
    region = character(),
    longitude = numeric(),
    latitude = numeric(),
    elevation = numeric(),
    wmo_id = character(),
    station_id = character(),
    fips_code = character(),
    source_name = character(),
    source_uid = character(),
    data_type = character(),
    data_policy = character(),
    update_status = character(),
    timestep = character(),
    obs_freq = character(),
    platform_type = character(),
    station_status = character(),
    start_year = integer(),
    end_year = integer(),
    observed_variables = character(),
    has_t = logical(),
    has_pp = logical(),
    has_sd = logical(),
    has_ws = logical(),
    has_wd = logical(),
    has_slp = logical(),
    has_dpt = logical(),
    has_wbt = logical(),
    has_rh = logical(),
    has_sc = logical(),
    has_snow = logical(),
    has_cc = logical(),
    has_sp = logical(),
    popup_content = character(),
    stringsAsFactors = FALSE
  )
}

read_inventory_cache <- function(cache_path = inventory_cache_parquet_path) {
  arrow::read_parquet(cache_path, as_data_frame = TRUE)
}

write_inventory_cache <- function(data, cache_path = inventory_cache_parquet_path) {
  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  arrow::write_parquet(data, sink = cache_path)
  invisible(cache_path)
}

load_inventory_data <- function() {
  if (file.exists(inventory_cache_parquet_path)) {
    cached_data <- read_inventory_cache(inventory_cache_parquet_path)
    message(sprintf("Inventory source: Parquet cache (fast-booting %d stations)", nrow(cached_data)))
    return(cached_data)
  }

  # If cache doesn't exist, fast-boot with an empty dataframe matching the API schema
  # server.R will detect nrow == 0 != api_total and fetch the actual data
  message("Inventory source: Empty schema (first boot)")

  empty_inventory_data()
}
inventory_data <- load_inventory_data()



country_choices <- sort(unique(inventory_data$country))
country_choices <- country_choices[!is.na(country_choices) & country_choices != "" & country_choices != "N/A"]

region_choices <- sort(unique(inventory_data$region))
region_choices <- region_choices[!is.na(region_choices) & region_choices != ""]

data_type_choices <- sort(unique(inventory_data$data_type))
data_type_choices <- data_type_choices[!is.na(data_type_choices) & data_type_choices != ""]

data_policy_choices <- sort(unique(inventory_data$data_policy))
data_policy_choices <- data_policy_choices[!is.na(data_policy_choices) & data_policy_choices != ""]

update_status_choices <- sort(unique(inventory_data$update_status))
update_status_choices <- update_status_choices[!is.na(update_status_choices) & update_status_choices != ""]

timestep_choices <- sort(unique(inventory_data$timestep))
timestep_choices <- timestep_choices[!is.na(timestep_choices) & timestep_choices != ""]

obs_freq_choices <- sort(unique(inventory_data$obs_freq))
obs_freq_choices <- obs_freq_choices[!is.na(obs_freq_choices) & obs_freq_choices != ""]

var_choices <- c(
  "Temperature" = "t", "Precipitation" = "pp", "Sunshine Duration" = "sd",
  "Wind Speed" = "ws", "Wind Direction" = "wd", "Sea Level Pressure" = "slp",
  "Dew Point" = "dpt", "Wet Bulb" = "wbt", "Relative Humidity" = "rh",
  "Snow Cover" = "sc", "Snow Depth" = "snow", "Cloud Cover" = "cc",
  "Station Pressure" = "sp"
)

min_yr <- suppressWarnings(min(inventory_data$start_year, na.rm = TRUE))
if (!is.finite(min_yr)) {
  min_yr <- 1700
}
max_yr <- as.integer(format(Sys.Date(), "%Y"))

print(paste0("Subdaily inventory loaded: ", nrow(inventory_data), " stations"))
