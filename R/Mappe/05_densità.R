library(sf)
library(ggplot2)
library(tidyverse)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
strade_etra <- st_read("cache/geojson/strade_etra.geojson")


# 1 Km -------------------------------------------------------------------

# griglia_europa_1km <- st_read("input/grid_1km_surf.gpkg")
# griglia_europa_1km <- st_transform(griglia_europa_1km, st_crs(comuni_etra))
# idx_1km <- st_intersects(griglia_europa_1km, comuni_etra, sparse = FALSE)
# griglia_comuni_etra_1km <- griglia_europa_1km[apply(idx_1km, 1, any), ]
# st_write(griglia_comuni_etra_1km, "cache/geojson/griglia_densità_1km.geojson")

# 2 Km -------------------------------------------------------------------

# griglia_europa_2km <- st_read("input/grid_2km_surf.gpkg")
# griglia_europa_2km <- st_transform(griglia_europa_2km, st_crs(comuni_etra))
# idx_2km <- st_intersects(griglia_europa_2km, comuni_etra, sparse = FALSE)
# griglia_comuni_etra_2km <- griglia_europa_2km[apply(idx_2km, 1, any), ]
# st_write(griglia_comuni_etra_2km, "cache/geojson/griglia_densità_2km.geojson")

griglia_comuni_etra <- st_read("cache/geojson/griglia_densità_2km.geojson")

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
