library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# ============================================================
# ------ DATASET DI ESEMPIO (sostituire con i propri dati) ------
# Struttura attesa:
#   codice_servizio : ID della fornitura
#   subentrato      : codice fornitura sostituito da questo (predecessore)
#   subentrante     : codice fornitura che sostituisce questo (successore)
# ============================================================
df <- tribble(
  ~codice_servizio, ~subentrato, ~subentrante,
  "A001", NA,       "A002",
  "A002", "A001",   "A003",
  "A003", "A002",   NA,
  "B001", NA,       NA,
  "C001", NA,       "C002",
  "C002", "C001",   NA
)

# Per usare i tuoi dati reali, commenta il blocco sopra e usa ad es.:
# df <- read.csv("dati_forniture.csv", stringsAsFactors = FALSE)


# ============================================================
# 1. Identificazione dei codici "iniziali" (radici delle catene)
# ============================================================
# Un codice è "iniziale" se:
#   - ha un subentrante (cioè è stato sostituito da qualcosa), E
#   - NON compare a sua volta come "subentrante" di nessun'altra riga
#     (altrimenti sarebbe già un anello intermedio di un'altra catena,
#      e comparirebbe come sostituzione_N della fornitura che lo precede)

subentranti_esistenti <- df$subentrante[!is.na(df$subentrante)]

radici <- df %>%
  filter(!is.na(subentrante), !(codice_servizio %in% subentranti_esistenti)) %>%
  pull(codice_servizio)


# ============================================================
# 2. Funzione che ricostruisce la catena di sostituzioni
#    a partire da un codice_servizio iniziale
# ============================================================
costruisci_catena <- function(start, df) {
  catena   <- character(0)
  attuale  <- start
  visitati <- start   # per protezione da eventuali cicli nei dati

  repeat {
    riga <- df %>% filter(codice_servizio == attuale)
    if (nrow(riga) == 0) break

    successivo <- riga$subentrante[1]
    if (is.na(successivo)) break

    if (successivo %in% visitati) {
      warning(paste("Ciclo rilevato a partire da", start,
                     "- catena interrotta prima di duplicare", successivo))
      break
    }

    catena   <- c(catena, successivo)
    visitati <- c(visitati, successivo)
    attuale  <- successivo
  }

  catena
}

# Costruisco la lista di catene, una per ogni codice radice
liste_catene <- map(radici, costruisci_catena, df = df)
names(liste_catene) <- radici


# ============================================================
# 3. Dataset 1: "storia" delle sostituzioni
#    codice_servizio_iniziale | sostituzione_1 | sostituzione_2 | ...
# ============================================================
n_max <- if (length(liste_catene) > 0) max(map_int(liste_catene, length)) else 0

storia <- map2_dfr(radici, liste_catene, function(id_iniziale, catena) {
  valori <- c(catena, rep(NA_character_, n_max - length(catena)))
  nomi   <- paste0("sostituzione_", seq_len(n_max))
  as_tibble(setNames(as.list(valori), nomi)) %>%
    mutate(codice_servizio_iniziale = id_iniziale, .before = 1)
})


# ============================================================
# 4. Dataset 2: versione "finale"
#    codice_servizio_iniziale | codice_servizio_finale
# ============================================================
finale <- map2_dfr(radici, liste_catene, function(id_iniziale, catena) {
  tibble(
    codice_servizio_iniziale = id_iniziale,
    codice_servizio_finale = if (length(catena) == 0) id_iniziale else tail(catena, 1)
  )
})


# ============================================================
# Output
# ============================================================
print(storia)
print(finale)

# Se vuoi salvare i risultati:
# write.csv(storia, "storia_forniture.csv", row.names = FALSE, na = "")
# write.csv(finale, "finale_forniture.csv", row.names = FALSE, na = "")