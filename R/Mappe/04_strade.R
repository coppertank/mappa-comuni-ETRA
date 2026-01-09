library(tidyverse)
library(sf)

# https://download.geofabrik.de/europe/italy/nord-est.html

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
bordi_etra <- st_read("cache/geojson/bordi_etra.geojson")
strade <- st_read("input/shp/strade/gis_osm_roads_free_1.shp")

unique(strade$fclass)

strade <- strade |>
  filter(
    !(fclass %in%
      c(
        "footway",
        "cycleway",
        "steps",
        "corridor",
        "bridleway",
        "track",
        "track_grade1",
        "track_grade2",
        "track_grade3",
        "track_grade4",
        "track_grade5",
        "busway",
        "pedestrian",
        "unknown",
        "path"
      ))
  )

strade <- strade %>%
  mutate(oneway = ifelse(oneway == "B", FALSE, TRUE))

# strade_etra <- strade |> sf::st_intersection(bordi_etra)
# strade_etra <- strade_etra |> st_cast("LINESTRING")
# st_write(strade_etra, "cache/geojson/strade_etra.geojson")

bbox_etra <- st_bbox(comuni_etra)
strade_complete <- strade[
  st_intersects(strade, st_as_sfc(bbox_etra), sparse = FALSE),
]


# strade_complete <- st_crop(strade, bbox_etra)
# strade_complete <- strade_complete |> st_cast("LINESTRING")
# st_write(strade_complete, "cache/geojson/strade_complete.geojson")

ggplot() +
  geom_sf(data = strade_complete, color = "gray40", size = 0.2) +
  theme_minimal()
