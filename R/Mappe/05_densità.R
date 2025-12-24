library(sf)
library(ggplot2)

# griglia_europa <- st_read("input/grid_1km_surf.gpkg")
comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")

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
