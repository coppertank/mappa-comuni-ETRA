library(tidyverse)
library(sf)
library(leaflet)

comuni_etra <- st_read("cache/comuni_etra.geojson")


#bin_pal = colorBin('YlOrRd', comuni_etra$area)

bin_pal = colorBin('YlGnBu', comuni_etra$area)

labels = sprintf(
  "<strong>Drive Alone Rate</strong><br/>%s",
  comuni_etra$nomcom
) |>
  lapply(htmltools::HTML)

leaflet() |>
  #addTiles() |>
  addProviderTiles(providers$CartoDB.Positron) |>
  addPolygons(
    data = comuni_etra,
    fillColor = ~ bin_pal(comuni_etra$area),
    color = 'grey',
    weight = 1,
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 3,
      color = 'black'
    ),
    label = labels
  ) |>
  addLegend(
    data = comuni_etra,
    pal = bin_pal,
    title = 'Drive Alone Rate',
    values = ~ (comuni_etra$area),
    position = 'bottomright'
  )
