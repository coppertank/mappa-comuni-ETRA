library(sf)
library(ggplot2)
library(tidyverse)

# griglia_europa <- st_read("input/grid_1km_surf.gpkg")
comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
strade_etra <- st_read("cache/geojson/strade_etra.geojson")

# griglia_europa <- st_transform(griglia_europa, st_crs(comuni_etra))

# idx <- st_intersects(griglia_europa, comuni_etra, sparse = FALSE)
# griglia_comuni_etra <- griglia_europa[apply(idx, 1, any), ]

# st_write(griglia_comuni_etra, "cache/geojson/griglia_densità.geojson")

griglia_comuni_etra <- st_read("cache/geojson/griglia_densità.geojson")

ggplot(griglia_comuni_etra) +
  geom_sf(aes(fill = TOT_P_2021), color = NA) + # niente bordo per effetto "heatmap"
  scale_fill_viridis_c(
    option = "magma", # o "plasma", "viridis", ecc.
    trans = "sqrt", # opzionale: attenua i valori molto alti
    name = "Ab/km²"
  ) +
  coord_sf() +
  theme_minimal()


ggplot() +
  geom_sf(data = griglia_comuni_etra, aes(fill = TOT_P_2021), color = NA) +
  geom_sf(data = comuni_etra, fill = NA, color = "white", size = 0.2) +
  scale_fill_viridis_c(option = "magma", trans = "sqrt", name = "Ab/km²") +
  coord_sf() +
  theme_minimal()

ggplot() +
  # 1) celle con 0 abitanti: grigio fisso
  geom_sf(
    data = griglia_comuni_etra |> filter(TOT_P_2021 == 0),
    fill = "grey",
    color = NA
  ) +
  # 2) celle con >0 abitanti: stessa heatmap di prima
  geom_sf(
    data = griglia_comuni_etra |> filter(TOT_P_2021 > 0),
    aes(fill = TOT_P_2021),
    color = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    trans = "sqrt",
    name = "Ab/km²"
  ) +
  geom_sf(
    data = strade_etra,
    color = "gold",
    linewidth = 0.4
  ) +
  coord_sf() +
  theme_minimal()
