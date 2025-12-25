library(tidyverse)
library(sf)
library(leaflet)
library(lwgeom)

idrografia <- st_read("input/shp/geonodevidrografiarete/v_idrografia_rete.shp")
strade <- st_read(
  "input/shp/rvcelementostradale/c01070240012_elementostradale.shp"
)
comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")

idrografia <- st_zm(idrografia, drop = TRUE, what = "ZM")
idrografia <- st_transform(idrografia, 4326)

strade <- st_zm(strade, drop = TRUE, what = "ZM")
strade <- st_transform(strade, 4326)


fiumi <- idrografia %>%
  filter(tipo_fiume == "FIUME")

# comuni_proj <- comuni_etra |> sf::st_transform(sf::st_crs(fiumi))
comuni_union <- comuni_etra |> sf::st_union()
fiumi_etra <- fiumi |> sf::st_intersection(comuni_union)
strade_etra <- strade |> sf::st_intersection(comuni_union)

# fiumi_etra is the resulting sf object
fiumi_etra
strade_etra

# st_write(fiumi_etra, "cache/geojson/fiumi_etra.geojson")
# st_write(strade_etra, "cache/geojson/strade_etra.geojson")

library(ggplot2)

ggplot() +
  geom_sf(
    data = comuni_etra,
    linewidth = 0.3,
    alpha = 0.4
  ) +
  geom_sf(
    data = fiumi_etra,
    color = "dodgerblue3",
    linewidth = 0.4
  ) +
  theme_minimal() +
  labs(
    title = "Rete idrografica nei comuni ETra",
    subtitle = "Fiumi e comuni",
    caption = "Fonte: dati geonode"
  )
