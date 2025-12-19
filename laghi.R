library(tidyverse)
library(sf)
library(leaflet)
library(lwgeom)

idrografia <- st_read("input/geonodevidrografiarete/v_idrografia_rete.shp")
comuni_etra <- st_read("cache/comuni_etra.geojson")

fiumi <- idrografia %>% 
  filter(tipo_fiume == "FIUME")

comuni_proj <- comuni_etra |> sf::st_transform(sf::st_crs(fiumi))
comuni_union <- comuni_proj |> sf::st_union()
rete_clipped <- fiumi |> sf::st_intersection(comuni_union)

# rete_clipped is the resulting sf object
rete_clipped

# st_write(rete_clipped, "cache/fiumi.geojson")

library(ggplot2)

ggplot() +
  geom_sf(
    data = comuni_proj,
    linewidth = 0.3,
    alpha = 0.4
  ) +
  geom_sf(
    data = rete_clipped,
    color = "dodgerblue3",
    linewidth = 0.4
  ) +
  theme_minimal() +
  labs(
    title = "Rete idrografica nei comuni ETra",
    subtitle = "Fiumi e comuni",
    caption = "Fonte: dati geonode"
  )
