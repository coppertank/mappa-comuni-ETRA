library(tidyverse)
library(sf)

# https://download.geofabrik.de/europe/italy/nord-est.html

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
bordi_etra <- st_read("cache/geojson/bordi_etra.geojson")
idrografia <- st_read("input/shp/acque/gis_osm_waterways_free_1.shp")

fiumi <- idrografia %>%
  filter(fclass == "river")

fiumi_etra <- fiumi |> sf::st_intersection(bordi_etra)
fiumi_etra <- fiumi_etra %>% st_cast("LINESTRING")

# st_write(fiumi_etra, "cache/geojson/fiumi_etra.geojson")
