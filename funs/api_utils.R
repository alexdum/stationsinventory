check_api_total_records <- function(base_url = "https://stationhub.geo.meteoromania.ro/geonetwork") {
  if (base_url == "") {
    return(NA_integer_)
  }
  
  endpoint <- paste0(sub("/$", "", base_url), "/srv/api/search/records/_search")
  
  body <- list(
    size = 0L,
    track_total_hits = TRUE,
    query = list(
      bool = list(
        filter = list(
          list(term = list(isTemplate = list(value = "n")))
        )
      )
    )
  )
  
  response <- tryCatch(
    httr::POST(endpoint, body = body, encode = "json", httr::timeout(5)),
    error = function(e) NULL
  )
  
  if (is.null(response) || httr::status_code(response) >= 400) {
    return(NA_integer_)
  }
  
  payload <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), simplifyDataFrame = FALSE),
    error = function(e) NULL
  )
  
  if (is.null(payload) || is.null(payload$hits) || is.null(payload$hits$total)) {
    return(NA_integer_)
  }
  
  total <- payload$hits$total$value %||% payload$hits$total
  return(as.integer(total))
}
