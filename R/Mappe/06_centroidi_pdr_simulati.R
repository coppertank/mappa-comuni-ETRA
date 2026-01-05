library(sf)
library(tidyverse)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
bordi_etra <- st_read("cache/geojson/bordi_etra.geojson")
comuni_montagna <- st_read("cache/geojson/comuni_montagna.geojson")
comuni_pianura <- st_read("cache/geojson/comuni_pianura.geojson")
griglia_comuni_etra <- st_read("cache/geojson/griglia_densità_2km.geojson")


# centroidi --------------------------------------------------------------

centroidi_comuni <- st_centroid(griglia_comuni_etra)
centroidi_comuni <- centroidi_comuni |> sf::st_intersection(bordi_etra)
centroidi_comuni <- centroidi_comuni |>
  filter(TOT_P_2021 > 0)

centroidi_comuni_montagna <- centroidi_comuni |>
  sf::st_intersection(comuni_montagna)
centroidi_comuni_pianura <- centroidi_comuni |>
  sf::st_intersection(comuni_pianura)

# st_write(centroidi_comuni, "cache/geojson/centroidi_comuni_2km.geojson")
# st_write(
#   centroidi_comuni_montagna,
#   "cache/geojson/centroidi_comuni_montagna_2km.geojson"
# )
# st_write(
#   centroidi_comuni_pianura,
#   "cache/geojson/centroidi_comuni_pianura_2km.geojson"
# )

# centri simulati --------------------------------------------------------

set.seed(126) # per riproducibilità
pts <- st_sample(st_union(comuni_etra), size = 15)
centri_simulati <- st_sf(
  id = seq_along(pts),
  geometry = pts
)

centri_simulati_montagna <- centri_simulati |>
  sf::st_intersection(comuni_montagna)
centri_simulati_pianura <- centri_simulati |>
  sf::st_intersection(comuni_pianura)

# st_write(centri_simulati, "cache/geojson/centri_simulati.geojson")
# st_write(centri_simulati_montagna, "cache/geojson/centri_simulati_montagna.geojson")
# st_write(centri_simulati_pianura, "cache/geojson/centri_simulati_pianura.geojson")
