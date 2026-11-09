# Atomicité : un message par unité atomique, l'objet qui porte l'attribut `action` et un
# identifiant. Le RUM pour le PMSI, l'acte pour le serveur d'actes, le diagnostic pour les
# états patient. Le contexte (patient, venue…) est répété dans chaque message.
library(hprimxml)
library(tibble)

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "DIM",
  destinataire = "DPI",
  date_heure_production = as.POSIXct("2024-03-10 08:00:00", tz = "UTC")
)
# contexte commun : patient et venue
contexte <- function(evt_id, ...) {
  tibble(evt_id = evt_id, ipp = "190101337", sexe = "F", date_naissance = as.Date("1958-03-12"),
         nda = "240000123", venue_date_entree = as.Date("2024-03-04"), ...)
}

# PMSI : un séjour, deux RUM -> deux messages (par_rum)
sejours <- contexte("S1")
rum <- tibble(
  evt_id = "S1", rum_id = 1:2, rum_emetteur = c("S1_1", "S1_2"),
  um = c("1201", "3405"), um_date_entree = as.Date(c("2024-03-04", "2024-03-06")),
  um_mode_entree = c("8", "6"), dp = c("I21.0", "I50.1")
)
write_evenements_pmsi(sejours, rum, entete = entete, out_dir = "sortie/rum", par_rum = TRUE)

# Serveur d'actes : une intervention, trois actes -> trois messages (par_acte)
interventions <- contexte("A1", intervention_id = "INT0001", intervention_uf = "1201")
actes <- tibble(
  evt_id = "A1", acte_id = c("ACT01", "ACT02", "ACT03"),
  code = c("DEQP003", "ZBQK002", "YYYY033"), activite = 1L, phase = 0L,
  date_execution = as.Date("2024-03-04"), medecin = "01234567"
)
write_evenements_serveur_actes(interventions, actes, entete = entete, out_dir = "sortie/actes",
                               par_acte = TRUE, filename = "{evt_origine}_{acte_id}.xml")

# États patient : une observation, deux diagnostics -> deux messages (par_diagnostic)
observations <- contexte("D1", recueil_id = "REC01", recueil_date = as.Date("2024-03-04"),
                         recueil_uf = "1201")
diagnostics <- tibble(
  evt_id = "D1", type = c("dp", "da"), code = c("I21.0", "E11.9"),
  date_action = as.POSIXct("2024-03-04 12:00:00", tz = "UTC")
)
write_evenements_etat_patient(observations, diagnostics, entete = entete,
                              out_dir = "sortie/diagnostics", par_diagnostic = TRUE)

# Bilan : un fichier par unité atomique
compte <- function(fichier, balise) {
  x <- paste(readLines(file.path("sortie", fichier), warn = FALSE), collapse = "\n")
  lengths(regmatches(x, gregexpr(paste0("<", balise), x, fixed = TRUE)))
}
fichiers <- sort(list.files("sortie", pattern = "\\.xml$", recursive = TRUE))
produits <- tibble(
  fichier = fichiers,
  RUM = vapply(fichiers, compte, integer(1), balise = "rum ", USE.NAMES = FALSE),
  actes = vapply(fichiers, compte, integer(1), balise = "acteCCAM", USE.NAMES = FALSE),
  diagnostics = vapply(fichiers, compte, integer(1), balise = "diagnostic ", USE.NAMES = FALSE)
)
