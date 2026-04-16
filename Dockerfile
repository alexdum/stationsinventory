# Use the official Rocker image for Shiny
FROM rocker/shiny:latest

# Install system dependencies your R packages might need
RUN apt-get update && apt-get install -y \
    cmake \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libsqlite3-dev \
    python3 \
    python3-pip \
    && rm -rf /var/lib/apt/lists/*

# Install tippecanoe (Python version) for PMTiles generation
RUN pip3 install tippecanoe || pip3 install --break-system-packages tippecanoe
# Install required R packages used by the app.
# Rocker images already configure a Linux binary-friendly default CRAN mirror,
# so avoid overriding repos here or installs may fall back to slower source builds.
RUN install2.r --error --skipinstalled \
    shiny \
    shinyjs \
    bslib \
    mapgl \
    magrittr \
    bsicons \
    dplyr \
    sf \
    DT \
    plotly \
    readxl \
    jsonlite \
    httr \
    later \
    countrycode \
    arrow

# Remove the default Shiny apps that come with the base image
RUN rm -rf /srv/shiny-server/*

# Copy your app file(s) into the container's Shiny server directory
COPY *.R /srv/shiny-server/
# If you have a data folder or other scripts, copy them too:
COPY funs/ /srv/shiny-server/funs/
COPY data/ /srv/shiny-server/data/
COPY www/ /srv/shiny-server/www/

# Ensure the shiny user has permissions to the app directory
RUN chown -R shiny:shiny /srv/shiny-server

# Expose the default Shiny Server port
EXPOSE 3838

# Start the Shiny server when the container starts
CMD ["/usr/bin/shiny-server"]
