library(terra)
library(geodata)
library(sf)
library(tidyverse)
library(viridis)
library(ggnewscale)
library(tidyterra)

theme_set(theme_minimal())

comuni_etra <- st_read("cache/comuni_etra.geojson")
fiumi <- st_read("cache/fiumi.geojson")

country_iso <- "ITA"

ita_elev_1 <- geodata::elevation_3s(
  lon = 11,
  lat = 45,
  path = tempdir()
)

ita_elev_2 <- geodata::elevation_3s(
  lon = 11,
  lat = 46,
  path = tempdir()
)

ita_elev_raster <- terra::merge(ita_elev_1, ita_elev_2)

rm(ita_elev_1, ita_elev_2)

terra::plot(
  ita_elev_raster,
  main = paste("Elevation (SRTM 30s) -", country_iso)
)

ita_elev_raster <- terra::crop(ita_elev_raster, comuni_etra)
ita_elev_raster <- terra::mask(ita_elev_raster, comuni_etra)

terra::plot(
  ita_elev_raster,
  main = paste("Elevation (SRTM 30s) -", country_iso)
)

terrain_attributes <- terra::terrain(
  ita_elev_raster,
  v = c("slope", "aspect"),
  unit = "radians"
)

print(terrain_attributes)

terra::plot(
  terrain_attributes$slope,
  main = "Slope (Radians)",
  col = viridis(100)
)

terra::plot(
  terrain_attributes$aspect,
  main = "Aspect (Radians from North)",
  col = viridis(100)
)

hillshade <- terra::shade(
  terrain_attributes[[1]],
  terrain_attributes[[2]],
  angle = 45,
  direction = 270
)

print(hillshade)

terra::plot(
  hillshade,
  col = grey(0:100 / 100),
  legend = FALSE,
  main = "Hillshade (Sun from West)"
)


# Shaded Relief -----------------------------------------------------------

# Rename layers for clarity BEFORE converting
names(ita_elev_raster) <- "elevation"
names(hillshade) <- "hillshade_val"

# Use as.data.frame with xy=TRUE to get coordinates
# Use na.rm=TRUE to potentially reduce data frame size
elev_df_gg <- terra::as.data.frame(
  ita_elev_raster,
  xy = TRUE,
  na.rm = TRUE
)

hillshade_df_gg <- terra::as.data.frame(
  hillshade,
  xy = TRUE,
  na.rm = TRUE
)

limits <- terra::minmax(ita_elev_raster)

shaded_relief_map <- ggplot() +
  geom_raster(
    data = hillshade_df_gg,
    aes(x = x, y = y, fill = hillshade_val),
    show.legend = FALSE
  ) +
  # Choose a light gray palette for the hillshade layer.
  # scale_fill_gradientn() lets us define exactly how
  # numeric values map to colors.
  scale_fill_gradientn(
    colors = hcl.colors(
      12,
      "Light Grays",
      rev = TRUE
    ), # A light gray palette
    na.value = NA # Transparent for any missing cells
  ) +
  ggnewscale::new_scale_fill() +
  # Second layer: draw the elevation raster on top of the hillshade.
  geom_raster(
    data = elev_df_gg,
    aes(x = x, y = y, fill = elevation),
    alpha = 0.5 # decrease alpha to
    # make hillshade more visible
  ) +
  # Choose a continuous hypso palette for elevation.
  tidyterra::scale_fill_hypso_tint_c(
    palette = "dem_poster",
    limits = as.vector(limits)
  ) +
  geom_sf(
    data = comuni_etra,
    color = "white",
    linewidth = 0.1
  ) +
  geom_sf(
    data = fiumi,
    color = "dodgerblue",
    linewidth = 0.5
  ) +
  # Add labels and theme
  # labs(
  #   title = "Elevazione comuni ETRA",
  #   fill = "Elevation (m)", # Legend title
  #   caption = paste(
  #     "Data: SRTM 3s via geodata.",
  #     "Hillshade: Sun from West (270 deg)"
  #   )
  # ) +
  # Use coord_sf to ensure correct aspect ratio for maps
  coord_sf(crs = terra::crs(ita_elev_raster)) + # Use
  # raster's CRS
  theme_void() + # theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white"),
    legend.position = "none", # "right",
    axis.title = element_blank()
  )

shaded_relief_map

ggsave(
  "plots/elevation_comuni_etra.png",
  shaded_relief_map,
  width = 10,
  height = 10,
  dpi = 300
)
