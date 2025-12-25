library(tidyverse)
library(sf)
library(leaflet)

comuni <- st_read("input/shp/rvccomuni/c0104011_comuni.shp")
# st_crs(comuni)
comuni <- st_transform(comuni, crs = "EPSG:4326")

comuni_pv <- comuni |>
  filter(provincia == "PD" | provincia == "VI" | provincia == "TV")

comuni_etra <- c(
  "Asiago",
  "Bassano del Grappa",
  "Battaglia Terme",
  "Borgoricco",
  "Cadoneghe",
  "Campodarsego",
  "Campodoro",
  "Campo San Martino",
  "Camposampiero",
  "Carmignano di Brenta",
  "Cartigliano",
  "Cassola",
  "Cervarese Santa Croce",
  "Cittadella",
  "Colceresa",
  "Curtarolo",
  "Enego",
  "Foza",
  "Fontaniva",
  "Galzignano Terme",
  "Galliera Veneta",
  "Gallio",
  "Gazzo",
  "Grantorto",
  "Limena",
  "Loreggia",
  "Lusiana Conco",
  "Marostica",
  "Massanzago",
  "Mestrino",
  "Montegrotto Terme",
  "Mussolente",
  "Nove",
  "Noventa Padovana",
  "Pianezze",
  "Piombino Dese",
  "Piazzola sul Brenta",
  "Pove del Grappa",
  "Pozzoleone",
  "Resana",
  "Roana",
  "Romano d'Ezzelino",
  "Rosa'",
  "Rossano Veneto",
  "Rotzo",
  "Rovolon",
  "Rubano",
  "Saccolongo",
  "San Giorgio delle Pertiche",
  "San Giorgio in Bosco",
  "San Martino di Lupari",
  "San Pietro in Gu'",
  "Santa Giustina in Colle",
  "Saonara",
  "Schiavon",
  "Selvazzano Dentro",
  "Solagna",
  "Teolo",
  "Tezze sul Brenta",
  "Tombolo",
  "Torreglia",
  "Trebaseleghe",
  "Valbrenta",
  "Veggiano",
  "Vigodarzere",
  "Villafranca Padovana",
  "Villa del Conte",
  "Villanova di Camposampiero",
  "Vigonza"
)

comuni_etra_shp <- comuni_pv |>
  filter(nomcom %in% comuni_etra)

comuni_etra_shp <- st_cast(comuni_etra_shp, "POLYGON")

# st_write(comuni_etra_shp, "cache/geojson/comuni_etra.geojson")
