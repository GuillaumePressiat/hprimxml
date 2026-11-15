# Actes NGAP dans un message serveur d'actes : une consultation, un forfait technique et un
# acte de biologie avec ses codes NABM. Lettres-clés, coefficients, codes et montants sont
# illustratifs : à remplacer par ceux de vos référentiels.
library(hprimxml)
library(tibble)

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "DIM",
  destinataire = "DPI",
  date_heure_production = as.POSIXct("2024-03-10 08:00:00", tz = "UTC")
)

# Contexte : une ligne par événement (patient, venue)
evenements <- tibble(
  evt_id = "EXT0001",
  ipp = "190101337", sexe = "F", date_naissance = as.Date("1958-03-12"),
  nda = "240000125", venue_date_entree = as.Date("2024-03-08")
)

# Actes NGAP : une ligne par acte
actes_ngap <- tibble(
  evt_id = "EXT0001",
  acte_id = c("NG01", "NG02", "NG03"),
  lettre_cle = c("CS", "FTN", "B"),               # consultation, forfait technique, biologie
  coefficient = c("1", "1", "26"),
  date_execution = as.Date("2024-03-08"),
  heure_execution = hms::as_hms(c("09:30:00", "10:15:00", "11:00:00")),
  medecin = "10003456789",
  uf = c("2101", "3301", "5201"),
  montant_total = c("30.00", "70.00", "7.02"),    # montants en euros (illustratifs)
  numero_forfait_technique = c(NA, "1", NA),      # forfait technique
  numero_agrement_appareil = c(NA, "12345678901234", NA),
  nabm = list(NULL, NULL, c(1104L, 593L))         # codes NABM de l'acte de biologie
)

# Un message avec les trois actes : sortie/ngap/EXT0001.xml
write_evenements_serveur_actes(evenements, actes_ngap = actes_ngap, entete = entete,
                               out_dir = "sortie/ngap")

# Un message par acte (unité atomique) : sortie/ngap_atomique/EXT0001_1.xml… _3.xml
write_evenements_serveur_actes(evenements, actes_ngap = actes_ngap, entete = entete,
                               out_dir = "sortie/ngap_atomique", par_acte = TRUE)

# Relecture : la table actes_ngap, codes NABM compris
relu <- read_evenements_serveur_actes("sortie/ngap")$actes_ngap
