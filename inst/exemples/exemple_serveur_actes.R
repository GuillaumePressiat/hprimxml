# Régénère data/xml_tests/actes/00001.xml : une intervention et quatre actes CCAM.
library(hprimxml)
library(tibble)

ts <- function(x) as.POSIXct(x, tz = "UTC")

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "TEST",
  destinataire = NULL,
  acquittement_attendu = "oui",
  commentaire = "HPRIM-TEST",
  date_heure_production = ts("2024-01-01 01:01:01")
)

# Une ligne par événement : patient, venue, intervention et demande
evenements <- tibble(
  evt_id = "00001_acte",
  ipp = "123456789", ipp_recepteur = "123456789",
  sexe = "M", nom_usuel = "SANDWICK", prenom = "JOHN",
  date_naissance = as.Date("1970-01-01"),
  nda = "987654321", nda_recepteur = "987654321",
  venue_date_entree = as.Date("2024-01-01"), venue_heure_entree = hms::as_hms("08:00:00"),
  venue_uf_responsable = "4321",
  `venue/@prive` = "non",
  intervention_id = "000000000001",
  intervention_date_debut = as.Date("2024-01-01"), intervention_heure_debut = hms::as_hms("08:10:00"),
  intervention_date_fin = as.Date("2024-01-01"), intervention_heure_fin = hms::as_hms("09:00:00"),
  intervention_uf = "4321",
  demande_date = as.Date("2023-12-25"), demande_heure = hms::as_hms("02:30:00"), demande_uf = "4321"
)

# Une ligne par acte CCAM
actes <- tibble(
  evt_id = "00001_acte",
  acte_id = c("121212", "121213", "121214", "121215"),
  code = c("EBLA003", "YYYY033", "EPLF002", "NFKA006"),
  activite = 1L, phase = 0L,
  date_execution = as.Date("2024-01-01"), heure_execution = hms::as_hms("08:10:00"),
  medecin = "01234567", uf = "4321",
  quantite = "1",                       # nombre5 : chaîne de chiffres dans le XSD
  facturable = "oui", valide = "oui"
)

fichiers <- write_evenements_serveur_actes(
  evenements, actes,
  entete = entete,
  out_dir = "sortie/actes",             # -> sortie/actes/00001_acte.xml
  validate_xsd = "warn"
)
