library(sf)
library(tidyverse)
library(ggnewscale)
library(tidyterra)
library(rayshader)
library(colorspace)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")

comuni_montani <- c(
  "Rotzo",
  "Roana",
  "Asiago",
  "Gallio",
  "Foza",
  "Enego",
  "Lusiana Conco",
  "Valbrenta",
  "Solagna",
  "Pove del Grappa"
)

comuni_montagna <- comuni_etra %>%
  filter(nomcom %in% comuni_montani)

comuni_pianura <- comuni_etra %>%
  filter(!nomcom %in% comuni_montani)

# st_write(comuni_montagna, "cache/geojson/comuni_montagna.geojson")
# st_write(comuni_pianura, "cache/geojson/comuni_pianura.geojson")

comuni_etra <- comuni_etra %>%
  mutate(
    montano = as.factor(if_else(
      nomcom %in% comuni_montani,
      "Montano",
      "Non montano"
    ))
  )


divisione_comuni <- ggplot() +
  geom_raster(
    data = hillshade_df_gg,
    aes(x = x, y = y, fill = hillshade_val),
    show.legend = FALSE
  ) +
  scale_fill_gradientn(
    colors = hcl.colors(12, "Light Grays", rev = TRUE),
    na.value = NA
  ) +
  ggnewscale::new_scale_fill() + # new scale for elevation
  geom_raster(
    data = elev_df_gg,
    aes(x = x, y = y, fill = elevation),
    alpha = 0.5
  ) +
  tidyterra::scale_fill_hypso_tint_c(
    palette = "dem_poster",
    limits = as.vector(limits)
  ) +
  ggnewscale::new_scale_fill() + # NEW: separate scale for discrete fills
  geom_sf(
    data = fiumi_etra,
    color = "cornflowerblue",
    linewidth = 0.5,
    fill = NA
  ) +
  geom_sf(
    data = comuni_etra,
    aes(color = montano, fill = montano),
    size = 0.3,
    alpha = 0.5
  ) +
  scale_color_manual(
    values = c("Montano" = "#A03636", "Non montano" = "#92BF54")
  ) +
  scale_fill_manual(
    values = c("Montano" = "#A03636", "Non montano" = "#92BF54")
  ) +
  coord_sf(crs = terra::crs(ita_elev_raster)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.position = "none",
    axis.title = element_blank()
  )

divisione_comuni

ggsave(
  "images/divisione_comuni_etra.png",
  divisione_comuni,
  width = 10,
  height = 10,
  dpi = 300
)
