library(tidyverse)
library(sf)
library(dodgr)

# https://download.geofabrik.de/europe/italy/nord-est.html

strade_complete <- st_read("cache/geojson/strade_complete.geojson")
comuni_montagna <- st_read("cache/geojson/comuni_montagna.geojson")
centroidi_comuni_montagna <- st_read(
  "cache/geojson/centroidi_comuni_montagna_2km.geojson"
)
centri_simulati_montagna <- st_read(
  "cache/geojson/centri_simulati_montagna.geojson"
)

bbox_montagna <- st_bbox(comuni_montagna)

strade_montagna <- strade_complete |>
  st_crop(bbox_montagna)
strade_montagna <- st_cast(strade_montagna, "LINESTRING")

graph <- weight_streetnet(
  strade_montagna,
  wt_profile = "motorcar",
  type_col = "fclass"
)
graph <- graph[graph$component == 1, ]

vertices <- dodgr_vertices(graph)

from_v <- match_pts_to_graph(
  graph,
  centroidi_comuni_montagna
)

to_v <- match_pts_to_graph(
  graph,
  centri_simulati_montagna
)

from_edge_rows <- graph[from_v, ]
to_edge_rows <- graph[to_v, ]

from_xy <- as.matrix(from_edge_rows[, c("from_lon", "from_lat")])
colnames(from_xy) <- c("x", "y")
to_xy <- as.matrix(to_edge_rows[, c("to_lon", "to_lat")])
colnames(to_xy) <- c("x", "y")
D <- dodgr_dists(graph, from = from_xy, to = to_xy, shortest = TRUE)

centroidi_comuni_montagna <- centroidi_comuni_montagna |>
  mutate(orig_id = row_number())

pop <- centroidi_comuni_montagna$TOT_P_2021
length(pop) == nrow(D) # deve essere TRUE
costo_pesato <- sweep(D, 1, pop, `*`)

ggplot() +
  geom_sf(data = comuni_montagna, fill = NA, color = "grey70") +
  geom_sf(data = centroidi_comuni_montagna, color = "red", size = 2) +
  geom_sf_text(
    data = centroidi_comuni_montagna,
    aes(label = orig_id),
    size = 3,
    color = "black"
  ) +
  theme_minimal()

i <- 37 # primo centroide
j <- 4 # primo centro

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
  sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(strade_montagna)) |>
  dplyr::summarise(geometry = sf::st_combine(geometry)) |>
  sf::st_cast("LINESTRING")

ggplot() +
  geom_sf(
    data = strade_montagna, # pass full sf, not st_geometry()
    color = "grey",
    size = 0.4,
    alpha = 0.7
  ) +
  geom_sf(
    data = comuni_montagna,
    color = "black",
    size = 0.4,
    alpha = 0.1
  ) +
  geom_sf(
    data = centroidi_comuni_montagna,
    color = "blue",
    size = 2,
    pch = 21,
    fill = "lightblue"
  ) +
  geom_sf(
    data = centri_simulati_montagna,
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


# Formulazione problema --------------------------------------------------

library(ompr)
library(ompr.roi)
library(ROI.plugin.glpk)

n_i <- nrow(D) # punti domanda
n_j <- ncol(D) # centri

pop <- centroidi_comuni_montagna$TOT_P_2021 # lunghezza n_i
f <- centri_simulati_montagna$area # lunghezza n_j

model <- MIPModel() %>%

  # variabili
  add_variable(x[i, j], i = 1:n_i, j = 1:n_j, type = "binary") %>%
  add_variable(y[j], j = 1:n_j, type = "binary") %>%

  # funzione obiettivo
  set_objective(
    sum_expr(D[i, j] * pop[i] * x[i, j], i = 1:n_i, j = 1:n_j) +
      sum_expr(f[j] * y[j], j = 1:n_j),
    sense = "min"
  ) %>%

  # ogni punto assegnato a un solo centro
  add_constraint(
    sum_expr(x[i, j], j = 1:n_j) == 1,
    i = 1:n_i
  ) %>%

  # assegnazione solo se centro aperto
  add_constraint(
    x[i, j] <= y[j],
    i = 1:n_i,
    j = 1:n_j
  )

result <- solve_model(
  model,
  with_ROI(solver = "glpk")
)

centri_aperti <- get_solution(result, y[j]) |>
  dplyr::filter(value > 0.5)

centri_aperti

assegnazioni <- get_solution(result, x[i, j]) |>
  dplyr::filter(value > 0.5)

assegnazioni

objective_value(result)
