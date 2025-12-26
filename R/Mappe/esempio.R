library(tidyverse)
library(sf)
library(leaflet)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
strade <- st_read("input/nord-est-251225-free.shp/gis_osm_roads_free_1.shp")
comuni_union <- comuni_etra |> sf::st_union()
strade_etra <- strade |> sf::st_intersection(comuni_union)

ggplot() +
  geom_sf(
    data = strade_etra,
    linewidth = 0.3,
    alpha = 0.4
  )

graph <- weight_streetnet(
  strade_etra,
  wt_profile = "motorcar",
  type_col = "fclass"
)