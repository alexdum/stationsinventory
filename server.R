# server.R
# Server logic with MapLibre and basemap switching
function(input, output, session) {

  # Serve PMTiles via a custom HTTP route to support Range requests
  if (stations_render_mode == "pmtiles") {
    stations_pmtiles_url <<- session$registerDataObj(
      name   = "stations_pmtiles",
      data   = "www/stations.pmtiles",
      filterFunc = function(data, req) {
        filepath <- data
        if (!file.exists(filepath)) return(NULL)
        size <- file.info(filepath)$size
        range_header <- req$HTTP_RANGE

        if (is.null(range_header)) {
          return(list(
            status = 200L,
            headers = list(
              "Content-Type" = "application/octet-stream",
              "Content-Length" = as.character(size),
              "Accept-Ranges" = "bytes"
            ),
            body = filepath
          ))
        }

        r <- regmatches(range_header, regexec("bytes=([0-9]+)-([0-9]*)", range_header))[[1]]
        if (length(r) == 0) {
          return(list(status = 416L, headers = list("Content-Range" = paste0("bytes */", size)), body = ""))
        }

        start <- as.numeric(r[2])
        end <- if (r[3] != "") as.numeric(r[3]) else (size - 1)

        if (end >= size) end <- size - 1
        if (start < 0) start <- 0
        len <- end - start + 1

        con <- file(filepath, "rb")
        seek(con, start)
        bytes <- readBin(con, "raw", n = len)
        close(con)

        return(list(
          status = 206L,
          headers = list(
            "Content-Type" = "application/octet-stream",
            "Content-Length" = as.character(len),
            "Content-Range" = paste0("bytes ", start, "-", end, "/", size),
            "Accept-Ranges" = "bytes"
          ),
          body = bytes
        ))
      }
    )
  }

  ofm_positron_style <- "https://tiles.openfreemap.org/styles/positron"
  ofm_bright_style <- "https://tiles.openfreemap.org/styles/bright"
  sentinel_url <- "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"
  sentinel_attribution <- "Tiles &copy; Esri &mdash; Source: Esri"

  style_change_trigger <- reactiveVal(0)
  stations_before_id <- reactiveVal(NULL)
  basemap_debounced <- shiny::debounce(reactive(input$basemap), 200)
  station_search_debounced <- shiny::debounce(reactive({
    s <- input$station_search
    if (is.null(s) || length(s) == 0) "" else trimws(s[1])
  }), 200)

  inventory_data_rv <- reactiveVal(load_inventory_data())
  current_default_year_range <- reactive({
    data <- inventory_data_rv()
    min_yr2 <- suppressWarnings(min(data$start_year, na.rm = TRUE))
    if (!is.finite(min_yr2)) min_yr2 <- 1700
    c(min_yr2, as.integer(format(Sys.Date(), "%Y")))
  })

  refresh_filter_controls <- function(data) {
    min_yr2 <- suppressWarnings(min(data$start_year, na.rm = TRUE))
    if (!is.finite(min_yr2)) min_yr2 <- 1700
    updateSliderInput(session, "year_range",
      min = min_yr2, max = as.integer(format(Sys.Date(), "%Y")),
      value = c(min_yr2, as.integer(format(Sys.Date(), "%Y"))))

    update_country_choices(selected = "All")
    update_station_choices(country = "All")
  }

  update_country_choices <- function(selected = "All") {
    data_curr <- isolate(inventory_data_rv())
    country_choices_curr <- sort(unique(data_curr$country))
    country_choices_curr <- country_choices_curr[
      !is.na(country_choices_curr) &
        country_choices_curr != "" &
        country_choices_curr != "N/A"
    ]

    if (!selected %in% country_choices_curr) {
      selected <- "All"
    }

    freezeReactiveValue(input, "country_filter")
    updateSelectizeInput(
      session,
      "country_filter",
      choices = c("All Countries" = "All", country_choices_curr),
      selected = selected,
      server = TRUE
    )
  }

  update_station_choices <- function(country = "All") {
    data_curr <- isolate(inventory_data_rv())
    if (!identical(country, "All")) {
      data_curr <- data_curr[!is.na(data_curr$country) & data_curr$country == country, ]
    }
    station_names <- sort(unique(data_curr$station_name))
    station_names <- station_names[
      !is.na(station_names) & station_names != "" & station_names != "Unknown station"
    ]
    freezeReactiveValue(input, "station_search")
    updateSelectizeInput(
      session,
      "station_search",
      choices = c("", station_names),
      selected = "",
      server = TRUE
    )
  }

  observeEvent(inventory_data_rv(), {
    update_country_choices(
      selected = isolate(input$country_filter %||% "All")
    )
    update_station_choices(
      country = isolate(input$country_filter %||% "All")
    )
  }, ignoreInit = TRUE)
  
  generate_dynamic_title <- reactive({
    loc <- "Global"
    if (!is.null(input$country_filter) && input$country_filter != "All") {
      loc <- input$country_filter
    }
    
    data <- dashboard_data()
    n_count <- format(nrow(data), big.mark = ",")
    paste0("<h4 style='margin-bottom: 20px; font-weight: 600; color: #2C3E50;'>", loc, " Stations Overview (", n_count, " Stations)</h4>")
  })

  output$dynamic_title_table <- renderUI({ HTML(generate_dynamic_title()) })
  output$dynamic_title_dashboard <- renderUI({ HTML(generate_dynamic_title()) })

  initial_lng <- 0
  initial_lat <- 38
  initial_zoom <- 3

  output$map <- renderMaplibre({
    print("Initializing MapLibre...")
    maplibre(
      style = ofm_positron_style,
      center = c(initial_lng, initial_lat),
      zoom = initial_zoom
    ) %>%
      add_navigation_control(show_compass = FALSE, visualize_pitch = FALSE, position = "top-left")
  })

  observeEvent(input$home_zoom, {
    maplibre_proxy("map") %>%
      fly_to(center = c(initial_lng, initial_lat), zoom = initial_zoom)
  })

  label_layer_ids <- c(
    "waterway_line_label", "water_name_point_label", "water_name_line_label",
    "highway-name-path", "highway-name-minor", "highway-name-major",
    "highway-shield-non-us", "highway-shield-us-interstate", "road_shield_us",
    "airport", "label_other", "label_village", "label_town", "label_state",
    "label_city", "label_city_capital", "label_country_3", "label_country_2", "label_country_1",
    "road_oneway", "road_oneway_opposite", "poi_r20", "poi_r7", "poi_r1", "poi_transit",
    "waterway-line-label", "water-name-point-label", "water-name-line-label",
    "highway-shield-non-us", "highway-shield-us-interstate", "road-shield-us",
    "label-other", "label-village", "label-town", "label-state",
    "label-city", "label-city-capital", "label-country-3", "label-country-2", "label-country-1",
    "place_villages", "place_town", "place_country_2", "place_country_1",
    "place_state", "place_continent", "place_city_r6", "place_city_r5",
    "place_city_dot_r7", "place_city_dot_r4", "place_city_dot_r2", "place_city_dot_z7",
    "place_capital_dot_z7", "place_capital", "roadname_minor", "roadname_sec",
    "roadname_pri", "roadname_major", "motorway_name", "watername_ocean",
    "watername_sea", "watername_lake", "watername_lake_line", "poi_stadium",
    "poi_park", "poi_zoo", "airport_label", "country-label", "state-label",
    "settlement-major-label", "settlement-minor-label", "settlement-subdivision-label",
    "road-label", "waterway-label", "natural-point-label", "poi-label", "airport-label"
  )

  country_border_layer_ids <- c(
    "boundary_3", "boundary_2", "boundary_disputed"
  )

  resolve_station_before_id <- local({
    style_before_id_cache <- new.env(parent = emptyenv())

    function(style_url, fallback_id = "boundary_3") {
      if (is.null(style_url) || identical(style_url, "")) {
        return(fallback_id)
      }

      if (exists(style_url, envir = style_before_id_cache, inherits = FALSE)) {
        return(get(style_url, envir = style_before_id_cache, inherits = FALSE))
      }

      before_id <- fallback_id

      tryCatch({
        response <- httr::GET(
          style_url,
          httr::user_agent("stationinventory/1.0"),
          httr::timeout(3)
        )
        httr::stop_for_status(response)

        style_spec <- jsonlite::fromJSON(
          httr::content(response, as = "text", encoding = "UTF-8"),
          simplifyVector = FALSE
        )

        layer_ids <- vapply(
          style_spec$layers %||% list(),
          function(layer) layer$id %||% NA_character_,
          character(1)
        )
        overlay_layer_ids <- c(country_border_layer_ids, label_layer_ids)
        matching_ids <- layer_ids[layer_ids %in% overlay_layer_ids]

        if (length(matching_ids) > 0) {
          before_id <- matching_ids[[1]]
        }
      }, error = function(e) {
        message("Style inspection failed for station layer ordering: ", e$message)
      })

      assign(style_url, before_id, envir = style_before_id_cache)
      before_id
    }
  })

  get_station_before_id_for_basemap <- function(basemap) {
    if (identical(basemap, "ofm_bright")) {
      return(resolve_station_before_id(ofm_bright_style, fallback_id = "boundary_3"))
    }

    resolve_station_before_id(ofm_positron_style, fallback_id = "boundary_3")
  }

  stations_before_id(get_station_before_id_for_basemap("ofm_positron"))

  non_label_layer_ids <- c(
    "background", "park", "water", "landcover_ice_shelf", "landcover_glacier",
    "landuse_residential", "landcover_wood", "waterway", "building",
    "tunnel_motorway_casing", "tunnel_motorway_inner", "aeroway-taxiway",
    "aeroway-runway-casing", "aeroway-area", "aeroway-runway",
    "road_area_pier", "road_pier", "highway_path", "highway_minor",
    "highway_major_casing", "highway_major_inner", "highway_major_subtle",
    "highway_motorway_casing", "highway_motorway_inner", "highway_motorway_subtle",
    "railway_transit", "railway_transit_dashline", "railway_service",
    "railway_service_dashline", "railway", "railway_dashline",
    "highway_motorway_bridge_casing", "highway_motorway_bridge_inner"
  )

  apply_label_visibility <- function(proxy, show_labels) {
    visibility <- if (isTRUE(show_labels)) "visible" else "none"
    for (layer_id in label_layer_ids) {
      tryCatch(
        { proxy %>% set_layout_property(layer_id, "visibility", visibility) },
        error = function(e) {}
      )
    }
  }

  observeEvent(basemap_debounced(), {
    req(basemap_debounced())
    basemap <- basemap_debounced()
    proxy <- maplibre_proxy("map")
    session$sendCustomMessage("show-map-overlay", list())
    # Style switch destroys all layers; mark PMTiles layer as needing re-add
    pmtiles_layer_added(FALSE)

    if (basemap %in% c("ofm_positron", "ofm_bright")) {
      style_url <- if (basemap == "ofm_positron") ofm_positron_style else ofm_bright_style
      proxy %>% set_style(style_url, preserve_layers = FALSE)
      stations_before_id(get_station_before_id_for_basemap(basemap))
      current_session <- shiny::getDefaultReactiveDomain()
      selected_basemap <- basemap
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          current_basemap <- isolate(input$basemap)
          if (is.null(current_basemap) || current_basemap != selected_basemap) return()
          apply_label_visibility(maplibre_proxy("map"), isolate(input$show_labels))
          style_change_trigger(isolate(style_change_trigger()) + 1)
        })
      }, delay = 0.35)
    } else if (basemap == "sentinel") {
      proxy %>% set_style(ofm_positron_style, preserve_layers = FALSE)
      current_session <- shiny::getDefaultReactiveDomain()
      selected_basemap <- basemap
      later::later(function() {
        shiny::withReactiveDomain(current_session, {
          current_basemap <- isolate(input$basemap)
          if (is.null(current_basemap) || current_basemap != selected_basemap) return()
          unique_suffix <- as.numeric(Sys.time()) * 1000
          source_id <- paste0("sentinel_source_", unique_suffix)
          layer_id <- paste0("sentinel_layer_", unique_suffix)
          maplibre_proxy("map") %>%
            add_raster_source(id = source_id, tiles = c(sentinel_url), tileSize = 256, attribution = sentinel_attribution) %>%
            add_layer(id = layer_id, type = "raster", source = source_id, paint = list("raster-opacity" = 1), before_id = "background")
          for (lid in non_label_layer_ids) {
            tryCatch({ maplibre_proxy("map") %>% set_layout_property(lid, "visibility", "none") }, error = function(e) {})
          }
          apply_label_visibility(maplibre_proxy("map"), isolate(input$show_labels))
          stations_before_id(get_station_before_id_for_basemap(basemap))
          style_change_trigger(isolate(style_change_trigger()) + 1)
        })
      }, delay = 0.5)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$show_labels, {
    req(input$basemap %in% c("ofm_positron", "ofm_bright", "sentinel"))
    apply_label_visibility(maplibre_proxy("map"), input$show_labels)
  })

  # --- Data Filtering ---
  base_filtered_stations <- reactive({
    data <- inventory_data_rv()

    if (!is.null(input$country_filter) && input$country_filter != "All") {
      data <- data %>% filter(country == input$country_filter)
    }

    search_term <- station_search_debounced()
    if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
      data <- data %>% filter(station_name == trimws(search_term))
    }

    if (!is.null(input$var_filter) && length(input$var_filter) > 0 && length(input$var_filter) < length(var_choices)) {
      keep <- rep(FALSE, nrow(data))
      if ("t" %in% input$var_filter) keep <- keep | (data$has_t %||% FALSE)
      if ("pp" %in% input$var_filter) keep <- keep | (data$has_pp %||% FALSE)
      if ("sd" %in% input$var_filter) keep <- keep | (data$has_sd %||% FALSE)
      if ("ws" %in% input$var_filter) keep <- keep | (data$has_ws %||% FALSE)
      if ("wd" %in% input$var_filter) keep <- keep | (data$has_wd %||% FALSE)
      if ("slp" %in% input$var_filter) keep <- keep | (data$has_slp %||% FALSE)
      if ("dpt" %in% input$var_filter) keep <- keep | (data$has_dpt %||% FALSE)
      if ("wbt" %in% input$var_filter) keep <- keep | (data$has_wbt %||% FALSE)
      if ("rh" %in% input$var_filter) keep <- keep | (data$has_rh %||% FALSE)
      if ("sc" %in% input$var_filter) keep <- keep | (data$has_sc %||% FALSE)
      if ("snow" %in% input$var_filter) keep <- keep | (data$has_snow %||% FALSE)
      if ("cc" %in% input$var_filter) keep <- keep | (data$has_cc %||% FALSE)
      if ("sp" %in% input$var_filter) keep <- keep | (data$has_sp %||% FALSE)
      
      # Always keep stations with Unknown variables to prevent them from disappearing 
      # when the dataset lacks full variable metadata
      if ("observed_variables" %in% names(data)) {
        keep <- keep | (is.na(data$observed_variables) | data$observed_variables == "Unknown")
      }
      
      data <- data[keep, ]
    }

    data
  })

  filtered_stations <- reactive({
    data <- base_filtered_stations()
    sel_start <- input$year_range[1]
    sel_end <- input$year_range[2]
    data %>% filter(
      (is.na(start_year) | start_year <= sel_end),
      (is.na(end_year) | end_year >= sel_start)
    )
  })

  build_map_source_data <- function(data) {
    if (!all(c("longitude", "latitude") %in% names(data))) {
      return(NULL)
    }

    map_cols <- intersect(c("popup_content", "longitude", "latitude"), names(data))
    map_data <- data[, map_cols, drop = FALSE]
    map_data <- map_data[is.finite(map_data$longitude) & is.finite(map_data$latitude), , drop = FALSE]

    if (nrow(map_data) == 0) {
      return(NULL)
    }

    st_as_sf(map_data, coords = c("longitude", "latitude"), crs = 4326)
  }

  clear_station_layers <- function(proxy) {
    proxy %>%
      clear_layer("stations_layer_tiled") %>%
      clear_layer("stations_tiled_source") %>%
      clear_layer("stations_layer")
  }

  # Track whether the PMTiles layer has been added to the map
  pmtiles_layer_added <- reactiveVal(FALSE)

  # Build a MapLibre filter expression from the current UI filter inputs.
  # Returns a list structure that mapgl::set_filter() serializes to JSON.
  # Uses integer 0/1 because tippecanoe converts booleans to integers.
  build_filter_expression <- function() {
    # Use isolate() because this helper may be called from non-reactive
    # contexts (e.g. onFlushed). Reactive dependency tracking is handled
    # by the calling observeEvent, not by this function.
    isolate({
      conditions <- list()

      # Country filter
      country_val <- input$country_filter
      if (!is.null(country_val) && country_val != "All") {
        conditions <- c(conditions, list(list("==", list("get", "country"), country_val)))
      }

      # Station search filter
      search_term <- station_search_debounced()
      if (!is.null(search_term) && nchar(trimws(search_term)) > 0) {
        conditions <- c(conditions, list(list("==", list("get", "station_name"), trimws(search_term))))
      }

      # Year range filter — stations whose records overlap [sel_start, sel_end]
      # Equivalent to: (start_year IS NULL OR start_year <= sel_end) AND (end_year IS NULL OR end_year >= sel_start)
      sel_start <- input$year_range[1]
      sel_end   <- input$year_range[2]
      if (!is.null(sel_start) && !is.null(sel_end)) {
        default_yr <- current_default_year_range()
        if (sel_start != default_yr[1] || sel_end != default_yr[2]) {
          conditions <- c(conditions, list(
            list("any",
              list("!", list("has", "start_year")),
              list("<=", list("get", "start_year"), sel_end)
            )
          ))
          conditions <- c(conditions, list(
            list("any",
              list("!", list("has", "end_year")),
              list(">=", list("get", "end_year"), sel_start)
            )
          ))
        }
      }

      # Variable filter — keep features that have at least one selected variable (OR logic)
      # The has_* fields are stored as 0/1 integers in the PMTiles
      var_filter <- input$var_filter
      if (!is.null(var_filter) && length(var_filter) > 0 && length(var_filter) < length(var_choices)) {
        var_conditions <- list()
        var_col_map <- c(
          "t" = "has_t", "pp" = "has_pp", "sd" = "has_sd",
          "ws" = "has_ws", "wd" = "has_wd", "slp" = "has_slp",
          "dpt" = "has_dpt", "wbt" = "has_wbt", "rh" = "has_rh",
          "sc" = "has_sc", "snow" = "has_snow", "cc" = "has_cc",
          "sp" = "has_sp"
        )
        for (v in var_filter) {
          col_name <- var_col_map[[v]]
          if (!is.null(col_name)) {
            var_conditions <- c(var_conditions, list(list("==", list("get", col_name), 1L)))
          }
        }
        # Also keep stations with observed_variables == "Unknown"
        var_conditions <- c(var_conditions, list(list("==", list("get", "observed_variables"), "Unknown")))

        if (length(var_conditions) > 0) {
          conditions <- c(conditions, list(do.call(c, list(list("any"), var_conditions))))
        }
      }

      if (length(conditions) == 0) {
        return(NULL)  # No filter — show all
      } else if (length(conditions) == 1) {
        return(conditions[[1]])
      } else {
        return(do.call(c, list(list("all"), conditions)))
      }
    })
  }

  # Apply the current filter expression to the PMTiles layer via set_filter()
  apply_pmtiles_filter <- function() {
    filter_expr <- tryCatch(
      build_filter_expression(),
      error = function(e) NULL  # shiny.silent.error when inputs not ready; NULL = show all
    )
    proxy <- maplibre_proxy("map")
    tryCatch(
      proxy %>% set_filter("stations_layer_tiled", filter_expr),
      error = function(e) message("set_filter failed: ", conditionMessage(e))
    )
  }

  # Add the PMTiles source + circle layer to the map (done once)
  add_pmtiles_layer <- function() {
    proxy <- clear_station_layers(maplibre_proxy("map"))

    if (stations_render_mode == "pmtiles") {
      proxy <- tryCatch(
        proxy %>%
          add_pmtiles_source(
            id = "stations_tiled_source",
            url = stations_pmtiles_url,
            source_type = "vector"
          ),
        error = function(e) { message("add_pmtiles_source error: ", e$message); proxy }
      )
    } else {
      proxy <- tryCatch(
        proxy %>%
          add_vector_source(
            id = "stations_tiled_source",
            tiles = stations_mvt_tiles
          ),
        error = function(e) { message("add_vector_source error: ", e$message); proxy }
      )
    }

    proxy %>%
      add_circle_layer(
        id = "stations_layer_tiled",
        source = "stations_tiled_source",
        source_layer = stations_tiles_source_layer,
        circle_color = "#3498db",
        circle_radius = interpolate(
          property = "zoom",
          values = c(2, 6, 10),
          stops = c(2.5, 4.5, 8)
        ),
        circle_opacity = 0.8,
        circle_stroke_width = 1.5,
        circle_stroke_color = "#3498db",
        tooltip = concat(
          "<b>", get_column("station_name"), "</b><br>",
          "Country: ", get_column("country"), " | Continent: ", get_column("continent"), "<br>",
          "Elevation: ", get_column("elevation"), " m<br>",
          "Period: ", get_column("start_year"), " - ", get_column("end_year"), "<br>",
          "Variables: ", get_column("observed_variables")
        ),
        before_id = isolate(stations_before_id())
      )

    pmtiles_layer_added(TRUE)
    session$sendCustomMessage("hide-map-overlay", list())
  }

  observeEvent(input$country_filter, {
    # Reset station search when country changes
    update_station_choices(country = input$country_filter)

    if (input$country_filter == "All") {
      maplibre_proxy("map") %>%
        fly_to(center = c(initial_lng, initial_lat), zoom = initial_zoom)
    } else {
      data <- filtered_stations()
      data <- data[is.finite(data$longitude) & is.finite(data$latitude), ]
      req(nrow(data) > 0)
      bbox <- st_bbox(st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326))
      maplibre_proxy("map") %>%
        fit_bounds(bbox = as.numeric(bbox), padding = 50, animate = TRUE)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$station_search, {
    s <- input$station_search
    if (is.null(s) || length(s) == 0 || s == "") {
      # Selection cleared, fly back to country or global
      if (!is.null(input$country_filter) && input$country_filter != "All") {
        data <- filtered_stations()
        data <- data[is.finite(data$longitude) & is.finite(data$latitude), ]
        if (nrow(data) > 0) {
          bbox <- st_bbox(st_as_sf(data, coords = c("longitude", "latitude"), crs = 4326))
          maplibre_proxy("map") %>%
            fit_bounds(bbox = as.numeric(bbox), padding = 50, animate = TRUE)
        }
      } else {
        maplibre_proxy("map") %>%
          fly_to(center = c(initial_lng, initial_lat), zoom = initial_zoom)
      }
      return()
    }

    # Selection made, zoom to station
    data <- isolate(inventory_data_rv())
    station <- data[!is.na(data$station_name) & data$station_name == s[1], ]
    station <- station[is.finite(station$longitude) & is.finite(station$latitude), ]
    if (nrow(station) > 0) {
      maplibre_proxy("map") %>%
        fly_to(center = c(station$longitude[1], station$latitude[1]), zoom = 10, speed = 1.5)
    }
  }, ignoreInit = TRUE)

  # Helper: render stations onto the map
  # In PMTiles mode: adds the layer once, then uses set_filter() for updates.
  # In GeoJSON mode: clears and re-adds the layer with filtered data.
  render_stations_to_map <- function(data, force_geojson = FALSE) {
    use_tiled <- stations_render_mode %in% c("pmtiles", "mvt") && !force_geojson

    if (use_tiled) {
      if (!isTRUE(isolate(pmtiles_layer_added()))) {
        add_pmtiles_layer()
      }
      # Only apply filter if inputs are initialized (not during initial onFlushed render).
      # At startup, show all stations (no filter = show all).
      if (isTRUE(isolate(map_ready_to_filter()))) {
        apply_pmtiles_filter()
      }
      session$sendCustomMessage("hide-map-overlay", list())
      return(invisible(NULL))
    }

    # GeoJSON fallback
    proxy <- clear_station_layers(maplibre_proxy("map"))
    map_data <- build_map_source_data(data)
    if (!is.null(map_data) && nrow(map_data) > 0) {
      map_tooltip <- if ("popup_content" %in% names(map_data)) get_column("popup_content") else NULL

      proxy %>%
        add_circle_layer(
          id = "stations_layer",
          source = map_data,
          circle_color = "#3498db",
          circle_radius = interpolate(
            property = "zoom",
            values = c(2, 6, 10),
            stops = c(2.5, 4.5, 8)
          ),
          circle_opacity = 0.8,
          circle_stroke_width = 1.5,
          circle_stroke_color = "black",
          tooltip = map_tooltip,
          before_id = isolate(stations_before_id())
        )
    }
    session$sendCustomMessage("hide-map-overlay", list())
  }

  map_initialized      <- reactiveVal(FALSE)
  map_ready_to_filter  <- reactiveVal(FALSE)
  suppress_startup_rerender <- reactiveVal(TRUE)
  startup_input_sync_pending <- reactiveVal(TRUE)

  output$status_msg <- renderText({
    req(map_initialized())
    data <- filtered_stations()
    total <- nrow(inventory_data_rv())
    paste0("Showing ", format(nrow(data), big.mark = ","), " / ",
           format(total, big.mark = ","), " stations.")
  })

  observe({
    req(startup_input_sync_pending())
    req(
      input$country_filter,
      input$var_filter,
      input$year_range
    )

    default_year_range <- current_default_year_range()
    search_term <- station_search_debounced() %||% ""
    startup_filters_ready <-
      identical(input$country_filter, "All") &&
      nchar(trimws(search_term)) == 0 &&
      length(input$var_filter) == length(var_choices) &&
      setequal(input$var_filter, unname(var_choices)) &&
      length(input$year_range) == 2 &&
      all(input$year_range == default_year_range)

    if (isTRUE(startup_filters_ready)) {
      startup_input_sync_pending(FALSE)
    }
  })

  session$onFlushed(function() {
    tryCatch({
    session$sendCustomMessage("freezeUI", list(text = "Pinging GeoNetwork API..."))
    api_total    <- check_api_total_records()
    current_data <- isolate(inventory_data_rv())
    startup_input_sync_pending(TRUE)

    cached_unique_stations <- if ("station_id" %in% names(current_data)) {
      length(unique(na.omit(as.character(current_data$station_id))))
    } else {
      nrow(current_data)
    }

    cache_is_current <- !is.na(api_total) && (
      api_total == nrow(current_data) ||
      api_total == cached_unique_stations
    )

    should_fetch <- !is.na(api_total) && !cache_is_current
    message(sprintf(
      "Startup sync decision: api_total=%s cache_nrow=%s cache_unique_station_id=%s should_fetch=%s",
      ifelse(is.na(api_total), "NA", format(api_total, scientific = FALSE)),
      nrow(current_data),
      cached_unique_stations,
      should_fetch
    ))

    if (should_fetch) {
      session$sendCustomMessage("updateFreezeMsg", list(text = sprintf("Fetching %s stations from server...", format(api_total, big.mark = ","))))

      new_data <- process_api_inventory(
        max_records = if(is.na(api_total)) 50000L else api_total,
        strict = FALSE,
        progress_callback = function(val, detail = "") {
          session$sendCustomMessage("updateFreezeMsg", list(text = detail))
        }
      )

      if (!is.null(new_data) && nrow(new_data) > 0) {
        session$sendCustomMessage("updateFreezeMsg", list(text = "Saving cache to disk..."))
        tryCatch(
          write_inventory_cache(new_data),
          error = function(e) message("Cache write failed: ", e$message)
        )

        # Regenerate PMTiles from updated parquet cache
        session$sendCustomMessage("updateFreezeMsg", list(text = "Building vector tiles..."))
        pmtiles_ok <- tryCatch(
          build_pmtiles(),
          error = function(e) { message("PMTiles build failed: ", e$message); FALSE }
        )
        if (isTRUE(pmtiles_ok)) {
          stations_render_mode <<- "pmtiles"
          stations_pmtiles_url <<- "stations.pmtiles"
        }

        inventory_data_rv(new_data)
        inventory_data <<- new_data
        refresh_filter_controls(new_data)

        # Render stations NOW inside this progress window — no stale gap
        session$sendCustomMessage("updateFreezeMsg", list(text = sprintf("Rendering %s stations on map...", format(nrow(new_data), big.mark = ","))))
        render_stations_to_map(new_data)

        map_initialized(TRUE)
        session$sendCustomMessage("updateFreezeMsg", list(text = sprintf("\u2713 %s stations loaded", format(nrow(new_data), big.mark = ","))))
        Sys.sleep(0.6)
        session$sendCustomMessage("unfreezeUI", list())
        
        later::later(function() { map_ready_to_filter(TRUE) }, delay = 1.0)
      } else {
        session$sendCustomMessage("unfreezeUI", list())
        later::later(function() { map_ready_to_filter(TRUE) }, delay = 1.0)
      }
    } else {
      refresh_filter_controls(current_data)

      # Cache is current — render existing data inside this window
      session$sendCustomMessage("updateFreezeMsg", list(text = sprintf("Rendering %s stations on map...", format(nrow(current_data), big.mark = ","))))
      render_stations_to_map(current_data)

      map_initialized(TRUE)

      if (!is.na(api_total)) {
        session$sendCustomMessage("updateFreezeMsg", list(text = sprintf("\u2713 %s stations (cache current)", format(nrow(current_data), big.mark = ","))))
        Sys.sleep(0.4)
      }
      session$sendCustomMessage("unfreezeUI", list())
      later::later(function() { map_ready_to_filter(TRUE) }, delay = 1.0)
    }
    }, error = function(e) {
      message("=== onFlushed ERROR ===")
      message("Error message: ", e$message)
      message("Error call: ", deparse(e$call))
      message(traceback())
      session$sendCustomMessage("unfreezeUI", list())
    })
  }, once = TRUE)

  # Render Map — only for filter-triggered re-renders (NOT initial load)
  # When using PMTiles, filter changes are applied via set_filter() (instant, GPU-side)
  # so we don't need the freezeUI overlay.
  observeEvent(list(filtered_stations(), style_change_trigger()), {
    req(map_ready_to_filter())
    if (isTRUE(startup_input_sync_pending())) {
      return()
    }

    if (isTRUE(suppress_startup_rerender())) {
      suppress_startup_rerender(FALSE)
      if (isTRUE(tryCatch(all_stations_selected(), error = function(e) FALSE))) {
        return()
      }
    }

    use_tiled <- stations_render_mode %in% c("pmtiles", "mvt")

    if (use_tiled && isTRUE(pmtiles_layer_added())) {
      # Fast path: just update the filter expression on the existing layer
      apply_pmtiles_filter()
    } else if (use_tiled) {
      # PMTiles available but layer not yet on map (e.g. after style switch)
      session$sendCustomMessage("freezeUI", list(text = "Rendering on map..."))
      render_stations_to_map(filtered_stations())
      session$sendCustomMessage("unfreezeUI", list())
    } else {
      # GeoJSON fallback
      session$sendCustomMessage("freezeUI", list(text = "Filtering data..."))
      data <- filtered_stations()
      session$sendCustomMessage("updateFreezeMsg", list(text = "Rendering on map..."))
      render_stations_to_map(data)
      session$sendCustomMessage("unfreezeUI", list())
    }
  }, ignoreInit = TRUE)


  # Station Table
  output$station_table <- DT::renderDataTable({
    req(filtered_stations())
    data <- filtered_stations()

    display_cols <- c("station_name", "country", "continent", "region", "longitude", "latitude",
                      "elevation", "start_year", "end_year", "observed_variables")
    available_cols <- intersect(display_cols, names(data))

    DT::datatable(
      data[, available_cols, drop = FALSE],
      options = list(
        pageLength = 25,
        lengthMenu = c(10, 25, 50, 100),
        scrollX = TRUE,
        order = list(0, 'asc'),
        selection = "single"
      ),
      rownames = FALSE,
      class = 'cell-border stripe',
      filter = 'top',
      callback = JS("table.on('click', 'tr', function() {
        var data = table.row(this).data();
        Shiny.setInputValue('station_rows_selected', this._DT_RowIndex + 1);
      });")
    ) %>%
      DT::formatStyle(
        columns = names(data[, available_cols, drop = FALSE]),
        fontSize = '13px'
      )
  }, server = TRUE)

  # Export CSV
  output$export_csv <- downloadHandler(
    filename = function() {
      paste0("stations_subdaily_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- filtered_stations()
      display_cols <- c("station_name", "country", "continent", "region", "longitude", "latitude",
                        "elevation", "wmo_id", "station_id", "source_name", "data_type",
                        "data_policy", "update_status", "timestep", "obs_freq",
                        "platform_type", "start_year", "end_year", "observed_variables")
      available_cols <- intersect(display_cols, names(data))
      write.csv(data[, available_cols, drop = FALSE], file, row.names = FALSE)
    }
  )

  # Station detail modal
  observeEvent(input$station_table_rows_selected, {
    req(input$station_table_rows_selected)
    data <- filtered_stations()
    row_idx <- input$station_table_rows_selected
    station <- data[row_idx, ]

    vars <- station$observed_variables
    var_badges <- paste0(
      "<span style='display:inline-block;padding:2px 8px;margin:2px;border-radius:12px;",
      "background:#e8f4fd;color:#1976d2;font-size:12px;font-weight:600;'>", vars, "</span>"
    )

    showModal(modalDialog(
      title = tags$strong(station$station_name, style = "font-size:18px;"),
      size = "m",
      easyClose = TRUE,
      footer = modalButton("Close"),
      tags$div(
        style = "line-height:1.8;",
        tags$p(tags$strong("Station ID: "), station$station_id),
        tags$p(tags$strong("WMO ID: "), station$wmo_id),
        tags$p(tags$strong("Country: "), station$country),
        tags$p(tags$strong("Continent: "), station$continent),
        tags$p(tags$strong("WMO Region: "), station$region),
        tags$p(tags$strong("Coordinates: "), sprintf("%.4f, %.4f", station$longitude, station$latitude)),
        tags$p(tags$strong("Elevation: "), station$elevation, " m"),
        tags$p(tags$strong("Period: "), station$start_year, " \u2013 ", station$end_year),
        tags$p(tags$strong("Duration: "), station$end_year - station$start_year, " years"),
        tags$p(tags$strong("Variables: "), HTML(var_badges)),
        tags$p(tags$strong("Data Type: "), station$data_type),
        tags$p(tags$strong("Data Policy: "), station$data_policy),
        tags$p(tags$strong("Update Status: "), station$update_status),
        tags$p(tags$strong("Obs Frequency: "), station$obs_freq),
        tags$p(tags$strong("Timestep: "), station$timestep),
        tags$p(tags$strong("Platform: "), station$platform_type),
        tags$p(tags$strong("Source: "), station$source_name)
      )
    ))
  })

  # Dashboard charts
  dashboard_data <- reactive({
    req(filtered_stations())
    filtered_stations()
  })

  all_stations_selected <- reactive({
    req(
      input$country_filter,
      input$var_filter,
      input$year_range
    )

    default_year_range <- current_default_year_range()
    search_term <- station_search_debounced() %||% ""

    identical(input$country_filter, "All") &&
      nchar(trimws(search_term)) == 0 &&
      length(input$var_filter) == length(var_choices) &&
      setequal(input$var_filter, unname(var_choices)) &&
      length(input$year_range) == 2 &&
      all(input$year_range == default_year_range)
  })

  output$chart_year_dist <- renderPlotly({
    data <- base_filtered_stations()
    req(nrow(data) > 0)
    plot_year_dist(data, TRUE, input$year_range)
  })


  output$chart_elevation <- renderPlotly({
    data <- dashboard_data()
    req(nrow(data) > 0)
    plot_elevation(data)
  })

  output$chart_var_pie <- renderPlotly({
    data <- dashboard_data()
    req(nrow(data) > 0)
    plot_var_pie(data)
  })

  output$chart_var_coverage <- renderPlotly({
    data <- dashboard_data()
    req(nrow(data) > 0)
    plot_var_coverage(data)
  })

  output$chart_record_length <- renderPlotly({
    data <- dashboard_data()
    req(nrow(data) > 0)
    plot_record_length(data)
  })

}
