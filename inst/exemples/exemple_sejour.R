# Séjour avec mutation : entrée par les urgences en cardiologie (UM 1201), puis mutation
# en médecine (UM 3405), sortie au domicile. Un RSS de deux RUM, écrit sans namespace,
# d'abord en un fichier par séjour, puis en RUM unitaires (un fichier par RUM).
library(hprimxml)
library(tibble)

ts <- function(x) as.POSIXct(x, tz = "UTC")

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "DIM",
  destinataire = "DPI",
  date_heure_production = ts("2024-03-10 08:00:00")   # par défaut : maintenant
)

# Une ligne par séjour
evenements <- tibble(
  evt_id = "S240001",                   # -> S240001.xml, identifiantMessage S240001
  ipp = "190101337",
  sexe = "F",
  date_naissance = as.Date("1958-03-12"),
  nda = "240000123",
  venue_date_entree = as.Date("2024-03-04"),
  venue_heure_entree = hms::as_hms("10:20:00"),
  venue_date_sortie = as.Date("2024-03-09"),
  rss_emetteur = "240000123"
)

# Une ligne par RUM
rum <- tibble(
  evt_id = "S240001",
  rum_id = 1:2,
  rum_emetteur = c("S240001_1", "S240001_2"),   # identifiant XML du RUM
  um = c("1201", "3405"),
  um_date_entree = as.Date(c("2024-03-04", "2024-03-06")),
  um_heure_entree = hms::as_hms(c("10:20:00", "14:00:00")),
  um_mode_entree = c("8", "6"),         # domicile, puis mutation
  um_provenance = c("5", "1"),          # urgences, puis MCO
  um_date_sortie = as.Date(c("2024-03-06", "2024-03-09")),
  um_mode_sortie = c("6", "8"),         # mutation, puis domicile
  um_destination = c("1", NA),          # vers MCO
  `medecinResponsable/noRPPS` = c("10003456789", "10009876543")
)

# Une ligne par diagnostic (le point des codes CIM-10 est retiré à l'écriture)
diagnostics <- tibble(
  evt_id = "S240001",
  rum_id = c(1L, 1L, 1L, 1L, 2L, 2L, 2L),
  type = c("DP", "DAS", "DAS", "DAS", "DP", "DAS", "DAS"),
  code = c("I21.0", "E11.9", "I10", "E78.0", "I50.1", "N18.3", "E11.9")
)

# Une ligne par acte CCAM
actes <- tibble(
  evt_id = "S240001",
  rum_id = c(1L, 1L, 2L),
  code = c("DEQP003", "ZBQK002", "DEQP003"),
  phase = 0L,
  activite = 1L,
  date_realisation = ts(c("2024-03-04 10:45:00", "2024-03-04 11:30:00", "2024-03-06 15:00:00"))
)

# Un fichier par séjour : sortie/sejour/S240001.xml
write_evenements_pmsi(
  evenements, rum, diagnostics, actes,
  entete = entete, out_dir = "sortie/sejour", xmlns = NULL
)

# RUM unitaires, avec marqueur de dépôt complet :
# sortie/rum/S240001_1.xml puis S240001_1.ok, sortie/rum/S240001_2.xml puis S240001_2.ok
write_evenements_pmsi(
  evenements, rum, diagnostics, actes,
  entete = entete, out_dir = "sortie/rum", xmlns = NULL, par_rum = TRUE, fichier_ok = TRUE
)

# Relecture : les mêmes tables (ici, diagnostics en colonnes de `rum`)
relu <- read_evenements_pmsi("sortie/sejour", format_diagnostics = "colonnes")
rum_relu <- relu$rum[, c("evt_id", "rum_id", "rum_emetteur", "um", "dp", "das")]
