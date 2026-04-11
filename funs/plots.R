# funs/plots.R
# Helper functions to build Plotly charts for the dashboard.

library(plotly)
library(dplyr)

#' Generate the Station Start Year Distribution chart
plot_year_dist <- function(data, is_cumulative = FALSE, selected_year_range = NULL) {
  year_counts <- data %>%
    count(start_year, name = "stations_started") %>%
    arrange(start_year)

  # Determine true historical calculation bounds
  true_min_year <- if (nrow(year_counts) > 0) min(year_counts$start_year, na.rm = TRUE) else 1800
  true_max_year <- as.integer(format(Sys.Date(), "%Y"))
  if (is.infinite(true_min_year)) true_min_year <- 1800

  # Determine zoom / view bounds
  if (!is.null(selected_year_range) && length(selected_year_range) == 2) {
    xaxis_range <- c(selected_year_range[1], selected_year_range[2])
  } else {
    xaxis_range <- c(true_min_year - 5, true_max_year)
  }

  if (is_cumulative) {
    # Generate full unbroken timeline from absolute beginning to calculate true cumsum
    cumulative_counts <- data.frame(start_year = seq(true_min_year, true_max_year)) %>%
      left_join(year_counts, by = "start_year") %>%
      mutate(
        stations_started = coalesce(stations_started, 0L),
        cumulative_stations = cumsum(stations_started)
      )

    fig <- plot_ly(
      cumulative_counts,
      x = ~start_year,
      y = ~cumulative_stations,
      type = "scatter",
      mode = "lines",
      line = list(color = "#2ecc71", width = 3),
      fill = "tozeroy",
      fillcolor = "rgba(46, 204, 113, 0.18)",
      hovertemplate = "Year: %{x}<br>Cumulative stations: %{y}<extra></extra>"
    )

    fig %>%
      layout(
        title = list(text = "Cumulative Station Starts", font = list(size = 16)),
        xaxis = list(title = "Year", range = xaxis_range),
        yaxis = list(title = "Cumulative Stations Started", rangemode = "tozero"),
        margin = list(l = 60, r = 20, t = 50, b = 50)
      )
  } else {
    fig <- plot_ly(
      year_counts,
      x = ~start_year,
      y = ~stations_started,
      type = "bar",
      marker = list(color = "#2ecc71"),
      hovertemplate = "Year: %{x}<br>Stations: %{y}<extra></extra>"
    )

    fig %>%
      layout(
        title = list(text = "Station Start Year Distribution", font = list(size = 16)),
        xaxis = list(title = "Year", range = xaxis_range),
        yaxis = list(title = "Stations Started", rangemode = "tozero"),
        margin = list(l = 60, r = 20, t = 50, b = 50)
      )
  }
}

#' Generate the Elevation Distribution histogram
plot_elevation <- function(data) {
  # Filter out missing elevation values encoded as -9999, -999, or 9999
  plot_data <- data %>% filter(!is.na(elevation) & elevation > -500 & elevation < 9000)
  if (nrow(plot_data) == 0) return(plot_ly())

  fig <- plot_ly(x = ~plot_data$elevation, type = "histogram",
                 nbinsx = 30, marker = list(color = "#9b59b6"),
                 hovertemplate = "Elevation: %{x:.0f} m<br>Count: %{y}<extra></extra>")
  fig %>%
    layout(
      title = list(text = "Elevation Distribution", font = list(size = 16)),
      xaxis = list(title = "Elevation (m)"),
      yaxis = list(title = "Number of Stations", rangemode = "tozero"),
      margin = list(l = 60, r = 20, t = 50, b = 50)
    )
}

#' Generate the Variable Breakdown pie chart
plot_var_pie <- function(data) {
  var_labels <- c(
    "Temperature" = "has_t", "Precipitation" = "has_pp", "Sunshine Duration" = "has_sd",
    "Wind Speed" = "has_ws", "Wind Direction" = "has_wd", "Sea Level Pressure" = "has_slp",
    "Dew Point" = "has_dpt", "Wet Bulb" = "has_wbt", "Relative Humidity" = "has_rh",
    "Snow Cover" = "has_sc", "Snow Depth" = "has_snow", "Cloud Cover" = "has_cc",
    "Station Pressure" = "has_sp"
  )
  available <- var_labels[var_labels %in% names(data)]
  all_var_counts <- sapply(available, function(col) sum(data[[col]], na.rm = TRUE))
  
  # Filter out variables with 0 stations to prevent 0% slice overlap
  has_data <- all_var_counts > 0
  var_counts <- all_var_counts[has_data]
  var_names <- names(var_counts)
  
  if (length(var_counts) == 0) return(plot_ly())
  
  # Keep colors consistent with the original variables list
  all_colors <- c("#e74c3c", "#3498db", "#f39c12", "#2ecc71", "#1abc9c", "#9b59b6",
                  "#e67e22", "#34495e", "#16a085", "#c0392b", "#8e44ad", "#2980b9", "#27ae60")
  color_map <- setNames(all_colors[seq_along(available)], names(available))
  pie_colors <- unname(color_map[var_names])
  var_pct <- var_counts / sum(var_counts)

  # Keep tiny slices in the legend/hover so outside labels do not stack on top of each other.
  label_threshold <- 0.05
  pie_text <- ifelse(
    var_pct >= label_threshold,
    paste0(var_names, "<br>", sprintf("%.1f%%", 100 * var_pct)),
    ""
  )

  fig <- plot_ly(labels = var_names, values = var_counts, type = "pie", hole = 0.5,
                 marker = list(colors = pie_colors),
                 text = pie_text, textinfo = "text", textposition = "outside",
                 textfont = list(size = 11), sort = FALSE,
                 hovertemplate = "%{label}: %{value} stations (%{percent})<extra></extra>")
  fig %>%
    layout(
      title = list(text = "Variable Breakdown", font = list(size = 16)),
      margin = list(l = 20, r = 100, t = 120, b = 110),
      legend = list(orientation = "v", x = 1.05, y = 0.5, yanchor = "middle", font = list(size = 11))
    )
}

#' Generate the Variable Availability bar chart
plot_var_coverage <- function(data) {
  var_labels <- c(
    "Temperature" = "has_t", "Precipitation" = "has_pp", "Sunshine Duration" = "has_sd",
    "Wind Speed" = "has_ws", "Wind Direction" = "has_wd", "Sea Level Pressure" = "has_slp",
    "Dew Point" = "has_dpt", "Wet Bulb" = "has_wbt", "Relative Humidity" = "has_rh",
    "Snow Cover" = "has_sc", "Snow Depth" = "has_snow", "Cloud Cover" = "has_cc",
    "Station Pressure" = "has_sp"
  )
  available <- var_labels[var_labels %in% names(data)]
  var_counts <- sapply(available, function(col) sum(data[[col]], na.rm = TRUE))
  var_names <- names(var_labels)[names(var_labels) %in% names(available)]

  colors <- c("#e74c3c", "#3498db", "#f39c12", "#2ecc71", "#1abc9c", "#9b59b6",
              "#e67e22", "#34495e", "#16a085", "#c0392b", "#8e44ad", "#2980b9", "#27ae60")
  color_map <- setNames(colors[seq_along(var_names)], var_names)

  fig <- plot_ly(x = var_names, y = var_counts, type = "bar",
                 marker = list(color = color_map[var_names]),
                 text = var_counts, textposition = "outside", textfont = list(size = 16, color = "#333"),
                 hovertemplate = "%{x}: %{y} stations<extra></extra>")
  fig %>%
    layout(
      title = list(text = "Variable Availability", font = list(size = 16)),
      xaxis = list(title = ""),
      yaxis = list(title = "Number of Stations", range = c(0, max(var_counts, na.rm=TRUE) * 1.2)),
      margin = list(l = 60, r = 20, t = 50, b = 100)
    )
}

#' Generate the Station Record Length histogram
plot_record_length <- function(data) {
  plot_data <- data %>%
    mutate(record_length = end_year - start_year) %>%
    filter(!is.na(record_length) & record_length >= 0)
    
  if (nrow(plot_data) == 0) return(plot_ly())

  fig <- plot_ly(x = ~plot_data$record_length, type = "histogram",
                 nbinsx = 30, marker = list(color = "#e67e22"),
                 hovertemplate = "Length: %{x} years<br>Count: %{y}<extra></extra>")
  fig %>%
    layout(
      title = list(text = "Station Record Length Distribution", font = list(size = 16)),
      xaxis = list(title = "Years of Operation", rangemode = "tozero"),
      yaxis = list(title = "Number of Stations", rangemode = "tozero"),
      margin = list(l = 60, r = 20, t = 50, b = 50)
    )
}

#' Generate the Data Policy Breakdown pie chart
plot_data_policy_pie <- function(data) {
  policy_counts <- data %>%
    count(data_policy) %>%
    filter(!is.na(data_policy) & data_policy != "")

  if (nrow(policy_counts) == 0) return(plot_ly())

  policy_counts$data_policy <- as.character(policy_counts$data_policy)

  fig <- plot_ly(labels = ~policy_counts$data_policy, values = ~policy_counts$n, type = "pie", hole = 0.5,
                 textinfo = "label+percent", textfont = list(size = 12),
                 hovertemplate = "%{label}: %{value} stations (%{percent})<extra></extra>")
  fig %>%
    layout(
      title = list(text = "Data Policy Breakdown", font = list(size = 16)),
      margin = list(l = 20, r = 100, t = 80, b = 80),
      legend = list(orientation = "v", x = 1.05, y = 0.5, yanchor = "middle", font = list(size = 11))
    )
}
