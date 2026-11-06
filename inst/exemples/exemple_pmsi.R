# Régénère data/xml_tests/pmsi/00001.xml : un séjour (RSS) à deux RUM.
library(hprimxml)
library(tibble)

ts <- function(x) as.POSIXct(x, tz = "UTC")

# En-tête du message (chaque clé peut aussi être une colonne de `evenements`)
entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "TEST",
  destinataire = NULL,                  # agent destinataire sans code, comme le fichier d'origine
  mode_traitement = "reel",
  acquittement_attendu = "oui",
  commentaire = "HPRIM-TEST",
  date_heure_production = ts("2024-01-01 01:01:01")
)

# Une ligne par séjour, donc par fichier : patient, venue, identifiant du RSS
evenements <- tibble(
  evt_id = "00001_pmsi",                # -> fichier 00001_pmsi.xml, identifiantMessage 00001_pmsi
  ipp = "123456789", ipp_recepteur = "123456789",
  sexe = "M", nom_usuel = "SANDWICK", nom_naissance = "SANDWICK", prenom = "JOHN",
  date_naissance = as.Date("1970-01-01"),
  nda = "987654321", nda_recepteur = "987654321",
  venue_date_entree = as.Date("2024-01-01"), venue_heure_entree = hms::as_hms("08:00:00"),
  venue_mode_entree = "8", venue_uf_responsable = "4321",
  rss_emetteur = "0", rss_recepteur = "000",
  # colonnes-chemins : attributs sans colonne dédiée
  `patient/@anonyme` = "non", `patient/@confidentiel` = "non", `venue/@prive` = "non"
)

# Une ligne par RUM ; rum_id = rang du RUM dans le RSS (clé de jointure)
rum <- tibble(
  evt_id = "00001_pmsi",
  rum_id = 1:2,
  rum_emetteur = "pmsi_00001",          # identifiant XML du RUM
  rum_recepteur = c("98765", "98766"),
  date_action = ts(c("2024-01-01 08:10:00", "2024-01-01 09:00:00")),
  um = c("4321", "5000"),
  um_date_entree = as.Date("2024-01-01"),
  um_heure_entree = hms::as_hms(c("08:00:00", "09:00:00")),
  um_mode_entree = "8",
  `@classant` = "non",
  `@prestationInterEtablissementA` = "non",
  `medecinResponsable/identification/code` = "01234567",
  `medecinResponsable/personne/nomUsuel` = "HOBBES",
  `medecinResponsable/personne/prenoms/prenom` = "MARC-JACQUES"
)

# Format long : une ligne par diagnostic, type dans DP, DR, DAS, DAD
diagnostics <- tibble(
  evt_id = "00001_pmsi",
  rum_id = c(1L, 1L, 2L),
  type = c("DP", "DR", "DP"),
  code = c("Z511", "C10", "Z743")
)

# Une ligne par acte CCAM
actes <- tibble(
  evt_id = "00001_pmsi",
  rum_id = c(1L, 1L, 1L, 1L, 2L),
  code = c("EBLA003", "EPLF002", "NFKA006", "YYYY033", "QZRP004"),
  phase = 0L, activite = 1L, quantite = 1L,
  date_realisation = ts(c(rep("2024-01-01 08:10:00", 4), "2024-01-01 09:10:00")),
  externe = "non", remboursement_exceptionnel = "non"
)

fichiers <- write_evenements_pmsi(
  evenements, rum, diagnostics, actes,
  entete = entete,
  out_dir = "sortie/pmsi",              # -> sortie/pmsi/00001_pmsi.xml
  validate_xsd = "warn"                 # destinataire sans code : non conforme au XSD
)
