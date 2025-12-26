library(tidyverse)
library(sf)

# https://download.geofabrik.de/europe/italy/nord-est.html

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
bordi_etra <- st_read("cache/geojson/bordi_etra.geojson")
strade <- st_read("input/shp/strade/gis_osm_roads_free_1.shp")

strade_etra <- strade |> sf::st_intersection(bordi_etra)

unique(strade_etra$fclass)

strade_etra <- strade_etra |>
  filter(
    !(fclass %in%
      c(
        "footway",
        "cycleway",
        "path",
        "pedestrian",
        "steps",
        "corridor",
        "crossing",
        "bridleway",
        "emergency_bay",
        "rest_area"
      ))
  )

strade_etra <- strade_etra |> st_cast("LINESTRING")

# st_write(strade_etra, "cache/geojson/strade_etra.geojson")
