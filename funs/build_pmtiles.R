# funs/build_pmtiles.R
# Build a PMTiles file from the parquet inventory cache.
# Includes all filterable properties so that MapLibre can filter client-side.

build_pmtiles <- function(
  parquet_path = file.path("data", "api_inventory_cache.parquet"),
  output_path  = file.path("www", "stations.pmtiles"),
  tippecanoe_bin = NULL
) {
  if (!file.exists(parquet_path)) {
    message("build_pmtiles: parquet cache not found at ", parquet_path)
    return(invisible(FALSE))
  }

  # Locate tippecanoe binary
  if (is.null(tippecanoe_bin)) {
    tippecanoe_bin <- Sys.which("tippecanoe")
    if (tippecanoe_bin == "") {
      # Common pip3 install location on macOS
      candidates <- c(
        file.path(Sys.getenv("HOME"), "Library", "Python", "3.9", "bin", "tippecanoe"),
        file.path(Sys.getenv("HOME"), "Library", "Python", "3.10", "bin", "tippecanoe"),
        file.path(Sys.getenv("HOME"), "Library", "Python", "3.11", "bin", "tippecanoe"),
        file.path(Sys.getenv("HOME"), "Library", "Python", "3.12", "bin", "tippecanoe"),
        "/usr/local/bin/tippecanoe",
        "/opt/homebrew/bin/tippecanoe"
      )
      for (cand in candidates) {
        if (file.exists(cand)) {
          tippecanoe_bin <- cand
          break
        }
      }
    }
    if (tippecanoe_bin == "" || !file.exists(tippecanoe_bin)) {
      message("build_pmtiles: tippecanoe not found. Install it with: pip3 install tippecanoe")
      return(invisible(FALSE))
    }
  }

  message("build_pmtiles: reading parquet cache...")
  data <- arrow::read_parquet(parquet_path, as_data_frame = TRUE)

  # Keep only rows with valid coordinates

  data <- data[is.finite(data$longitude) & is.finite(data$latitude), ]
  if (nrow(data) == 0) {
    message("build_pmtiles: no valid coordinate rows")
    return(invisible(FALSE))
  }

  # Select filterable properties to include in the tiles
  tile_cols <- c(
    "station_name", "country", "continent", "elevation",
    "start_year", "end_year",
    "has_t", "has_pp", "has_sd", "has_ws", "has_wd",
    "has_slp", "has_dpt", "has_wbt", "has_rh",
    "has_sc", "has_snow", "has_cc", "has_sp",
    "observed_variables"
  )
  tile_cols <- intersect(tile_cols, names(data))

  # Convert booleans to integers (0/1) for MapLibre expression compatibility
  bool_cols <- grep("^has_", tile_cols, value = TRUE)
  for (bc in bool_cols) {
    data[[bc]] <- as.integer(data[[bc]])
  }

  # Convert integer year columns to numeric for safe JSON serialization
  if ("start_year" %in% tile_cols) data$start_year <- as.numeric(data$start_year)
  if ("end_year" %in% tile_cols) data$end_year <- as.numeric(data$end_year)

  # Build sf object
  sf_data <- sf::st_as_sf(
    data[, c("longitude", "latitude", tile_cols), drop = FALSE],
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  # Write temporary GeoJSON
  tmp_geojson <- tempfile(pattern = "stations_", fileext = ".geojson")
  on.exit(unlink(tmp_geojson), add = TRUE)

  message("build_pmtiles: writing temporary GeoJSON (", nrow(sf_data), " features)...")
  sf::st_write(sf_data, tmp_geojson, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)

  # Ensure output directory exists
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  # Run tippecanoe
  # -z12: max zoom 12 (points don't need higher)
  # -Z0:  min zoom 0
  # -r1:  no feature dropping — keep all points at all zoom levels
  # -l stations: source layer name
  # --no-tile-size-limit: don't drop features to fit tile size limits
  cmd <- sprintf(
    '%s -o %s -z12 -Z0 -r1 --no-tile-size-limit -l stations --force %s',
    shQuote(tippecanoe_bin),
    shQuote(output_path),
    shQuote(tmp_geojson)
  )

  message("build_pmtiles: running tippecanoe...")
  message("  ", cmd)
  exit_code <- system(cmd)

  if (exit_code != 0) {
    message("build_pmtiles: tippecanoe failed with exit code ", exit_code)
    return(invisible(FALSE))
  }

  file_size_mb <- round(file.info(output_path)$size / 1024 / 1024, 1)
  message(sprintf("build_pmtiles: successfully created %s (%.1f MB)", output_path, file_size_mb))
  return(invisible(TRUE))
}
