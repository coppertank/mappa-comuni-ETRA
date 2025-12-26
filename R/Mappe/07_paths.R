library(sf)
library(tidyverse)
library(dodgr)

comuni_etra <- st_read("cache/geojson/comuni_etra.geojson")
griglia_comuni_etra <- st_read("cache/geojson/griglia_densità_2km.geojson")
centroidi_comuni <- st_read("cache/geojson/centroidi_comuni.geojson")
centri_simulati <- st_read("cache/geojson/centri_simulati.geojson")
strade_etra <- st_read("cache/geojson/strade_etra.geojson")

graph <- weight_streetnet(
  strade_etra,
  wt_profile = "motorcar",
  type_col = "fclass"
)

vertices <- dodgr_vertices(graph)

from_v <- match_pts_to_graph(
  graph,
  centroidi_comuni
)

to_v <- match_pts_to_graph(
  graph,
  centri_simulati
)

from_edge_rows <- graph[from_v, ]
to_edge_rows <- graph[to_v, ]

from_xy <- as.matrix(from_edge_rows[, c("from_lon", "from_lat")])
colnames(from_xy) <- c("x", "y")
to_xy <- as.matrix(to_edge_rows[, c("to_lon", "to_lat")])
colnames(to_xy) <- c("x", "y")
D <- dodgr_dists(graph, from = from_xy, to = to_xy, shortest = TRUE)

# from_xy <- vertices[match(from_v, vertices$id), c("x", "y")]
# to_xy <- vertices[match(to_v, vertices$id), c("x", "y")]

# from_xy <- st_coordinates(centroidi_comuni) # origini
# to_xy <- st_coordinates(centri_simulati)

# # 3) Matrice distanze (righe = centroidi, colonne = centri)
# D <- dodgr_dists(
#   graph,
#   from = from_xy,
#   to = to_xy,
#   shortest = TRUE
# )

i <- 2 # primo centroide
j <- 1 # primo centro

paths_sf <- dodgr_paths(
  graph = graph,
  from = from_xy[i, ], # primo centroide
  to = to_xy[j, ]
)

verts <- dodgr_vertices(graph)
path1 <- verts[match(paths_sf[[1]][[1]], verts$id), ]
# head(path1)

# ...existing code...

# convert dodgr vertices (path1) into an sf LINESTRING for plotting
path_line <- path1 |>
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(strade_etra)) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("LINESTRING")

ggplot() +
  geom_sf(
    data = strade_etra, # pass full sf, not st_geometry()
    color = "grey",
    size = 0.3,
    alpha = 0.7
  ) +
  geom_sf(
    data = centroidi_comuni,
    color = "blue",
    size = 2,
    pch = 21,
    fill = "lightblue"
  ) +
  geom_sf(
    data = centri_simulati,
    color = "darkgreen",
    size = 4,
    pch = 22,
    fill = "green"
  ) +
  geom_sf(
    data = path_line, # now an sf LINESTRING
    color = "red",
    linewidth = 1,
    alpha = 0.8
  ) +
  coord_sf() +
  theme_minimal()
