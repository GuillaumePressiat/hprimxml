# Anonymiser des fichiers HPRIM XML pour des tests locaux : identifiants et noms
# pseudonymisés (même longueur, même nature de caractères), dates décalées d'un même nombre
# de jours, codes médicaux et attributs intacts.
library(hprimxml)
library(tibble)

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "DIM",
  destinataire = "DPI",
  date_heure_production = as.POSIXct("2024-03-10 08:00:00", tz = "UTC")
)
evenements <- tibble(
  evt_id = "S240001", ipp = "190101337", sexe = "F",
  nom_usuel = "MARTIN", prenom = "JEANNE", date_naissance = as.Date("1958-03-12"),
  nda = "240000123", venue_date_entree = as.Date("2024-03-04")
)
rum <- tibble(
  evt_id = "S240001", rum_id = 1L, rum_emetteur = "S240001_1", um = "1201",
  um_date_entree = as.Date("2024-03-04"), um_mode_entree = "8", dp = "I21.0",
  `medecinResponsable/noRPPS` = "10003456789"
)
write_evenements_pmsi(evenements, rum, entete = entete, out_dir = "sortie/original", xmlns = NULL,
                      fichier_ok = TRUE)

# Anonymisation : chaque fichier reçoit sa propre transformation aléatoire. La graine ne sert
# qu'à rendre cet exemple reproductible (doc, tests) ; en pratique, on l'omet.
anonymes <- hprim_anonymiser("sortie/original", "sortie/anonyme", graine = "exemple")

# Avant / après, à partir des tables relues
extra <- list(rum = "medecinResponsable/noRPPS")
avant <- read_evenements_pmsi("sortie/original", extra = extra)
apres <- read_evenements_pmsi("sortie/anonyme", extra = extra)
champs <- list(
  evenements = c("evt_id", "ipp", "nom_usuel", "prenom", "sexe", "date_naissance", "nda",
                 "venue_date_entree"),
  rum = c("rum_emetteur", "um", "um_date_entree", "medecinResponsable/noRPPS"),
  diagnostics = "code"
)
avant_apres <- do.call(rbind, lapply(names(champs), function(t) {
  tibble(champ = champs[[t]],
         original = vapply(champs[[t]], function(c) format(avant[[t]][[c]][1]), ""),
         anonymise = vapply(champs[[t]], function(c) format(apres[[t]][[c]][1]), ""))
}))
