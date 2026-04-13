# ui.R

# UI Definition with bslib and MapLibre
filter_label <- function(text, tooltip) {
  tagList(
    text,
    bslib::tooltip(
      tags$span(
        icon("circle-info"),
        style = "margin-left:6px; cursor:help; color:#6c757d;"
      ),
      tooltip,
      placement = "right"
    )
  )
}

ui <- page_navbar(
  theme = bs_theme(version = 5),
  title = "Station Inventory System",
  navbar_options = navbar_options(collapsible = TRUE),
  header = tagList(
    shinyjs::useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$script(src = "app.js")
    ),
    # Frozen overlay div
    div(
      class = "frozen-overlay",
      div(
        class = "frozen-overlay-content",
        div(class = "spinner-border text-primary", role = "status"),
        p(class = "frozen-overlay-message", "Synchronizing station data...")
      )
    )
  ),
  fillable_mobile = TRUE,
  sidebar = sidebar(
    title = "Inventory Controls",
    open = list(desktop = "open", mobile = "closed"),
    selectizeInput("country_filter", filter_label("Country", "Show only stations from the selected country."), choices = c("All Countries" = "All", country_choices), selected = "All", multiple = FALSE),
    selectizeInput(
      "station_search",
      filter_label("Find Station", "Click to browse stations or type to search by name. Selecting a station zooms the map to it."),
      choices = NULL,
      selected = NULL,
      multiple = FALSE,
      options = list(
        placeholder = "Click to browse or type station name...",
        openOnFocus = TRUE,
        selectOnTab = TRUE,
        maxOptions = 200,
        plugins = list("clear_button")
      )
    ),
    sliderInput("year_range", filter_label("Period Of Availability", "Keep stations whose records overlap the selected year range."), min = min_yr, max = max_yr, value = c(min_yr, max_yr), sep = "", round = TRUE),
    div(
      class = "data-info-box",
      div(class = "data-info-title", "Current Status"),
      div(class = "data-info-item", textOutput("status_msg"))
    ),
    checkboxGroupInput("var_filter", filter_label("Variables", "Keep stations that measure at least one selected variable."), choices = var_choices, selected = unname(var_choices))
  ),
  nav_panel(
    "Map Explorer",
    div(
      style = "position: relative; height: 100%; width: 100%;",
      maplibreOutput("map", height = "100%", width = "100%"),
      div(
        id = "map_loading_overlay",
        class = "map-loading-overlay",
        div(class = "map-loading-spinner")
      ),
      absolutePanel(
        top = 130, left = 10, right = "auto",
        class = "map-layer-control",
        style = "z-index: 1000;",
        div(class = "control-icon", icon("layer-group")),
        div(
          class = "control-content",
          radioButtons(
            inputId = "basemap",
            label = "Basemap Style",
            choices = c(
              "Positron" = "ofm_positron",
              "Bright" = "ofm_bright",
              "Satellite" = "sentinel"
            ),
            selected = "ofm_positron"
          ),
          hr(style = "margin: 8px 0;"),
          checkboxInput(
            inputId = "show_labels",
            label = "Show Labels",
            value = TRUE
          )
        )
      ),
      absolutePanel(
        id = "zoom_home_panel",
        top = 80, left = 10, right = "auto",
        actionButton("home_zoom", bsicons::bs_icon("house-fill"), class = "btn-home", title = "Zoom to Initial View")
      )
    )
  ),
  nav_panel(
    "Station List",
    div(
      style = "padding: 16px;",
      uiOutput("dynamic_title_table"),
      downloadButton("export_csv", "Export CSV", class = "btn-export-csv"),
      DT::dataTableOutput("station_table")
    )
  ),
  nav_panel(
    "Dashboard",
    div(
      style = "padding: 16px; overflow-y: auto;",
      uiOutput("dynamic_title_dashboard"),
      fluidRow(
        column(6, plotlyOutput("chart_year_dist", height = "400px")),
        column(6, plotlyOutput("chart_elevation", height = "400px"))
      ),
      fluidRow(
        column(6, plotlyOutput("chart_var_pie", height = "400px")),
        column(6, plotlyOutput("chart_var_coverage", height = "400px"))
      ),
      fluidRow(
        column(6, plotlyOutput("chart_record_length", height = "400px"))
      )
    )
  )
)
