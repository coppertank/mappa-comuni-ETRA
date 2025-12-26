library(sf)
library(tidyverse)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
griglia_comuni_etra <- st_read("cache/geojson/griglia_densità_2km.geojson")


# centroidi --------------------------------------------------------------

centroidi_comuni <- st_centroid(griglia_comuni_etra)
comuni_union <- comuni_etra |> sf::st_union()
centroidi_comuni <- centroidi_comuni |> sf::st_intersection(comuni_union)
centroidi_comuni <- centroidi_comuni |>
  filter(TOT_P_2021 > 0)

# st_write(centroidi_comuni, "cache/geojson/centroidi_comuni.geojson")

# centri simulati --------------------------------------------------------

set.seed(126) # per riproducibilità
pts <- st_sample(st_union(comuni_etra), size = 15)
centri_simulati <- st_sf(
  id = seq_along(pts),
  geometry = pts
)

# st_write(centri_simulati, "cache/geojson/centri_simulati.geojson")
