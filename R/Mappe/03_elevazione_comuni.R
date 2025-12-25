library(terra)
library(geodata)
library(sf)
library(tidyverse)
library(viridis)
library(ggnewscale)
library(tidyterra)
library(elevatr)
library(colorspace)
library(metR)
library(ggspatial)
library(rayshader)


comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
fiumi_etra <- st_read("cache/geojson/fiumi_etra.geojson")
contorni <- st_union(comuni_etra)

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
  ita_elev_raster
)

veneto_elev_raster <- terra::crop(ita_elev_raster, comuni_etra)
ita_elev_raster <- terra::mask(veneto_elev_raster, comuni_etra)

terra::plot(
  ita_elev_raster
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
  # geom_sf(
  #   data = comuni_etra,
  #   color = "white",
  #   linewidth = 0.1
  # ) +
  geom_sf(
    data = contorni,
    color = "black",
    linewidth = 0.4
  ) +
  geom_sf(
    data = fiumi_etra,
    color = "cornflowerblue",
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
  "images/elevazione_comuni_etra.png",
  shaded_relief_map,
  width = 10,
  height = 10,
  dpi = 300
)

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


# 3. Download & prepare DEM
#--------------------------

# Target projection (keep EPSG:4326 or change if needed)
proj <- "EPSG:4326"

# 4. Compute breaks & limits
# --------------------------
limits <- range(elev_df_gg$elevation, na.rm = TRUE)
breaks <- seq(
  floor(limits[1] / 50) * 50,
  ceiling(limits[2] / 50) * 50,
  by = 200
)

# 5. Build your hypsometric palette
# ---------------------------------
pal_vec <- tidyterra::hypso.colors2(
  n = 12,
  palette = "dem_poster",
  alpha = 1,
  rev = FALSE
)

pie(rep(1, length(pal_vec)), col = pal_vec)
pal <- pal_vec[c(1:6)]
pie(rep(1, length(pal)), col = pal)

light_col <- colorspace::lighten(pal[2], amount = 0.15)
dark_col <- colorspace::darken(pal[5], amount = 0.25)


# 7. Build the 2D Tanaka‐style map
# --------------------------------
gg_tanaka_hypso <- ggplot(
  data = elev_df_gg,
  aes(x = x, y = y, z = elevation)
) +
  geom_contour_fill(
    breaks = breaks
  ) +
  scale_fill_gradientn(
    name = "Elevation",
    colors = pal,
    breaks = breaks,
    labels = round(breaks, 0),
    limits = limits,
    guide = guide_colourbar(
      title.position = "top",
      title.hjust = .5,
      ticks = FALSE,
      barheight = unit(5, "cm"),
      frame.colour = NA
    )
  ) +
  metR::geom_contour_tanaka(
    breaks = breaks,
    sun.angle = 45,
    light = light_col,
    dark = dark_col,
    range = c(0.01, 0.3),
    smooth = 0.8
  ) +
  geom_sf(
    data = fiumi_etra,
    inherit.aes = FALSE, # <- prevents looking for x,y in fiumi_etra
    color = "#1f78b4",
    linewidth = 0.8
  ) +
  coord_sf(crs = proj) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white",
      color = NA
    ),
    plot.margin = unit(
      c(
        t = .1,
        r = .1,
        l = .1,
        b = .1
      ),
      "lines"
    ),
    legend.position = "none"
  )

ggsave(
  "images/etra-tanaka-2d.png",
  gg_tanaka_hypso,
  width = 7,
  height = 7,
  # dpi = 300,
  bg = "white"
)

# 8. 3D extrusion & high‐quality render
# -------------------------------------
# rgl::close3d()
# rgl::rgl.close()

rayshader::plot_gg(
  ggobj = gg_tanaka_hypso,
  width = 7,
  height = 7,
  scale = 140,
  shadow = TRUE,
  shadow_intensity = 1,
  windowsize = c(700, 700),
  zoom = 0.50,
  phi = 60,
  theta = 0,
  background = "white",
  multicore = TRUE
)


# 10. Final high‐quality PNG
# --------------------------
rayshader::render_highquality(
  filename = "images/comuni-etra-tanaka-3d-test.png",
  preview = TRUE,
  light = FALSE,
  environment_light = "R/Mappe/lights/venice_sunrise_4k.hdr",
  intensity = 3,
  rotate_env = 90,
  parallel = TRUE,
  width = 1800,
  height = 1800,
  interactive = FALSE
)
