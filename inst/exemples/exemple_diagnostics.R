# Trois façons équivalentes de fournir les diagnostics d'un RUM avec plusieurs DAS.
# Les trois écritures produisent exactement le même fichier XML.
library(hprimxml)
library(tibble)

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "DIM",
  destinataire = "DPI",
  date_heure_production = as.POSIXct("2024-03-10 08:00:00", tz = "UTC")
)

evenements <- tibble(
  evt_id = "S240002", ipp = "190101338", sexe = "M",
  date_naissance = as.Date("1951-07-02"), nda = "240000124",
  venue_date_entree = as.Date("2024-03-04"), rss_emetteur = "240000124"
)

rum <- tibble(
  evt_id = "S240002", rum_id = 1L, um = "1201",
  um_date_entree = as.Date("2024-03-04"), um_mode_entree = "8"
)

# 1. Format long : une table `diagnostics`, une ligne par code
diagnostics <- tibble(
  evt_id = "S240002",
  rum_id = 1L,
  type = c("DP", "DAS", "DAS", "DAS", "DAS"),
  code = c("I21.0", "E11.9", "I10", "E78.0", "N18.3")
)
write_evenements_pmsi(evenements, rum, diagnostics, entete = entete, out_dir = "sortie/long")

# 2. Colonnes dans `rum`, DAS en colonne liste
rum_liste <- rum
rum_liste$dp <- "I21.0"
rum_liste$das <- list(c("E11.9", "I10", "E78.0", "N18.3"))
write_evenements_pmsi(evenements, rum_liste, entete = entete, out_dir = "sortie/liste")

# 3. Colonnes dans `rum`, DAS en chaîne séparée par des virgules
rum_chaine <- rum
rum_chaine$dp <- "I21.0"
rum_chaine$das <- "E11.9, I10, E78.0, N18.3"
write_evenements_pmsi(evenements, rum_chaine, entete = entete, out_dir = "sortie/chaine")

# Une chaîne se convertit aussi en liste d'une ligne : strsplit(rum_chaine$das, ", ")
fichiers <- lapply(c("long", "liste", "chaine"), function(f)
  readBin(file.path("sortie", f, "S240002.xml"), "raw", 1e6))
stopifnot(identical(fichiers[[1]], fichiers[[2]]), identical(fichiers[[1]], fichiers[[3]]))
