# Régénère data/xml_tests/etat_patient/00001.xml : diagnostics DP et DR.
library(hprimxml)
library(tibble)

ts <- function(x) as.POSIXct(x, tz = "UTC")

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "TEST",
  destinataire = NULL,
  acquittement_attendu = "non",
  commentaire = "HPRIM-TEST",
  date_heure_production = ts("2024-01-01 01:01:01")
)

# Une ligne par événement : patient, venue, intervention
# (ici, les colonnes-chemins sont relatives à la racine du message)
evenements <- tibble(
  evt_id = "00001_esep",
  ipp = "123456789", ipp_recepteur = "123456789",
  sexe = "M", nom_usuel = "SANDWICK", prenom = "JOHN",
  date_naissance = as.Date("1970-01-01"),
  nda = "987654321", nda_recepteur = "987654321",
  venue_date_entree = as.Date("2024-01-01"), venue_heure_entree = hms::as_hms("08:00:00"),
  venue_uf_responsable = "4321",
  intervention_id = "000000000001", intervention_uf = "4321",
  `patient/@confidentiel` = "non", `venue/@prive` = "non",
  `patient/identifiant/emetteur/@etat` = "permanent",
  `patient/identifiant/emetteur/@portee` = "local",
  `patient/identifiant/emetteur/@referent` = "non",
  `patient/identifiant/recepteur/@etat` = "permanent",
  `patient/identifiant/recepteur/@portee` = "local",
  `patient/identifiant/recepteur/@referent` = "non"
)

# Une ligne par diagnostic ; type dans dp, dr, da, ds, dd, fpc, mm, ae
diagnostics <- tibble(
  evt_id = "00001_esep",
  type = c("dp", "dr"),
  code = c("Z511", "C10"),
  date_action = ts("2024-01-01 08:10:00")
)

fichiers <- write_evenements_etat_patient(
  evenements, diagnostics,
  entete = entete,
  out_dir = "sortie/etat_patient",      # -> sortie/etat_patient/00001_esep.xml
  validate_xsd = "warn"
)
