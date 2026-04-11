# Station Timeseries Inventory Facility

The **Station Timeseries Inventory Facility** is an interactive web dashboard developed to explore, filter, and analyze global meteorological and climatological station metadata. Built with R and Shiny, this tool synchronizes an extensive database of observation stations directly from the GeoNetwork API and visualizes them on robust, dynamic maps.

## 🚀 Live Demo

You can view and interact with the application live at:
**[https://stationhub.geo.meteoromania.ro/stationinventory/](https://stationhub.geo.meteoromania.ro/stationinventory/)**

---

## 🌟 Key Features

* **Interactive Map Explorer**: View station distribution on modern, highly responsive basemaps (Positron, Bright, and high-resolution Satellite layers via MapLibre).
* **Dynamic Search & Filtering**: 
  * Filter stations geographically by **Country**.
  * Use the autocomplete **Find Station** input to zoom straight to a specific station.
  * Filter by **Period of Availability** (time boundaries).
  * Filter by specific **Observed Variables** (e.g., Temperature, Precipitation, Wind Speed, Sea Level Pressure, Snow Cover, and more).
* **Comprehensive Analytics Dashboard**: Auto-generated Plotly charts provide visual insights into your filtered subset:
  * Temporal distribution histograms showing station start/end years.
  * Elevation profiles.
  * Variable coverage statistics and pie charts.
  * Station record length distribution.
* **Detailed Data Table & Export**: Browse the raw metadata of your filtered stations in a responsive table, view pop-up modals for individual station breakdowns, and easily export the data to CSV.
* **High-Performance Architecture**: Efficiently processes and displays datasets containing tens of thousands of records without freezing or slowing down the map interface.

---

## 📡 Data Source

All station metadata is served dynamically via the GeoNetwork REST API:
**[https://stationhub.geo.meteoromania.ro/geonetwork/](https://stationhub.geo.meteoromania.ro/geonetwork/)**
