# IGS2 détaillé dans un message états patient (evenementsServeurEtatsPatient).
# Des mesures brutes (pires valeurs des 24 premières heures), on déduit pour chaque item
# la classe attendue par le XSD (attribut de <igs2>) et ses points, puis le score global.
# Un fichier par patient : l'élément <igs2> n'est pas répétable dans un message.
library(hprimxml)
library(tibble)

ts <- function(x) as.POSIXct(x, tz = "UTC")

entete <- list(
  version = "2.00",                     # version HPRIM XML des messages
  emetteur = "REA",
  destinataire = "DIM",
  date_heure_production = ts("2024-03-06 09:00:00")
)

# Pires valeurs des 24 premières heures, une ligne par patient
mesures <- tibble(
  evt_id = c("IGS240001", "IGS240002"),
  age = c(67, 82),
  frequence_cardiaque = c(128, 105),    # /min
  pas = c(85, 210),                     # pression artérielle systolique, mmHg
  temperature = c(38.2, 39.4),          # °C
  ventile = c(TRUE, FALSE),             # ventilation mécanique ou CPAP
  pao2_fio2 = c(180, NA),               # mmHg, si ventilé
  diurese = c(0.8, 1.6),                # L/24 h
  uree = c(18, 8),                      # mmol/L
  leucocytes = c(14, 22),               # 10³/mm³
  kaliemie = c(4.1, 5.4),               # mmol/L
  natremie = c(132, 147),               # mmol/L
  hco3 = c(17, 22),                     # mmol/L
  bilirubine = c(25, 110),              # µmol/L
  ictere = c(FALSE, TRUE),              # la bilirubine ne compte que si ictère
  glasgow = c(13, 15),
  maladie_chronique = c("aucune", "cancerMetastatique"),
  mode_admission = c("medecine", "chirUrgente")
)

# Barème IGS2 : attribut XSD -> mesure, bornes, points par classe XSD
# Source : grille et définitions de la SFAR, https://sfar.org/scores/igs2.php
# (Le Gall JR et coll., JAMA 1993; 270: 2957-2963). Recueil sur les 24 heures suivant
# l'admission : valeur la plus défavorable de chaque item ; Glasgow estimé avant sédation ;
# diurèse ramenée à 24 h si le séjour est plus court ; en cas d'arrêt cardiaque, FC 0,
# PAS 0 et Glasgow < 6. Unités : urée en mmol/L (seuils SFAR en g/L : 0,6 et 1,8),
# leucocytes en 10³/mm³, bilirubine en µmol/L (seuils en mg/L : 40 et 60).
# Les bornes sont fermées à gauche : 40 tombe dans « 40A59 ».
BAREME <- list(
  age = list("age", c(40, 60, 70, 75, 80),
             c(inferieurA40 = 0, `40A59` = 7, `60A69` = 12, `70A74` = 15, `75A79` = 16,
               superieurEgalA80 = 18)),
  frequenceCardiaque = list("frequence_cardiaque", c(40, 70, 120, 160),
             c(inferieurA40 = 11, `40A69` = 2, `70A119` = 0, `120A159` = 4, superieurEgalA160 = 7)),
  tensionArterielleSystolique = list("pas", c(70, 100, 200),
             c(inferieurA70 = 13, `70A99` = 5, `100A199` = 0, superieurEgalA200 = 2)),
  temperature = list("temperature", 39, c(inferieurA39 = 0, superieurEgalA39 = 3)),
  paO2_FIO2 = list("pao2_fio2", c(100, 200), c(inferieurA100 = 11, `100A199` = 9, superieurEgalA200 = 6)),
  diurese = list("diurese", c(0.5, 1), c(`inferieurA0,5` = 11, `0,5A0,999` = 4, superieurA1 = 0)),
  ureeSanguine = list("uree", c(10, 30), c(inferieurA10 = 0, `10A29,9` = 6, superieurA30 = 10)),
  leucocytes = list("leucocytes", c(1, 20), c(inferieurA1 = 12, `1A19,9` = 0, superieurEgalA20 = 3)),
  kaliemie = list("kaliemie", c(3, 5), c(inferieurA3 = 3, `3A4,9` = 0, superieurEgalA5 = 3)),
  natremie = list("natremie", c(125, 145), c(inferieurA125 = 5, `125A144` = 0, superieurEgalA145 = 1)),
  HCO3 = list("hco3", c(15, 20), c(inferieurA15 = 6, `15A19` = 3, superieurEgalA20 = 0)),
  bilirubine = list("bilirubine", c(68.4, 102.6),
             c(`inferieurA68,4` = 0, `68,4A102,5` = 4, `superieurA102,6` = 9)),
  scoreGlasgow = list("glasgow", c(6, 9, 11, 14),
             c(inferieurA6 = 26, `6A8` = 13, `9A10` = 7, `11A13` = 5, `14A18` = 0))
)
POINTS_QUALITATIFS <- list(
  maladieChronique = list("maladie_chronique",
             c(aucune = 0, cancerMetastatique = 9, maladieHematologiqueMaligne = 10, sida = 17)),
  modeAdmission = list("mode_admission", c(chirProgrammee = 0, medecine = 6, chirUrgente = 8))
)

# Classe d'une mesure : libelles[i] si bornes[i-1] <= mesure < bornes[i]
classer <- function(x, bornes, libelles) {
  as.character(cut(x, c(-Inf, bornes, Inf), labels = libelles, right = FALSE))
}

igs2 <- mesures
for (attr in names(BAREME)) {
  b <- BAREME[[attr]]
  classe <- classer(mesures[[b[[1]]]], b[[2]], names(b[[3]]))
  if (attr == "paO2_FIO2") classe <- ifelse(mesures$ventile, classe, "rien")  # ni ventilé ni CPAP
  if (attr == "bilirubine") classe <- ifelse(mesures$ictere, classe, "inferieurA68,4")  # non ictérique
  igs2[[attr]] <- classe
  igs2[[paste0("points_", attr)]] <- as.integer(c(b[[3]], rien = 0)[classe])
}
for (attr in names(POINTS_QUALITATIFS)) {
  q <- POINTS_QUALITATIFS[[attr]]
  igs2[[attr]] <- mesures[[q[[1]]]]
  igs2[[paste0("points_", attr)]] <- as.integer(q[[2]][mesures[[q[[1]]]]])
}
igs2$valeurGlobale <- as.integer(rowSums(igs2[grep("^points_", names(igs2))]))
attributs <- c(names(BAREME), names(POINTS_QUALITATIFS))

# Patient et venue, une ligne par message
evenements <- tibble(
  evt_id = c("IGS240001", "IGS240002"),
  ipp = c("190101337", "190101338"), sexe = c("M", "F"),
  date_naissance = as.Date(c("1956-05-14", "1941-11-02")),
  nda = c("240000123", "240000124"), venue_date_entree = as.Date("2024-03-05"),
  # repère du message : le recueil, avec sa date et l'UF de réanimation où les états ont
  # été observés (le récepteur retrouve ainsi le RUM, même un jour de mutation)
  recueil_id = c("REC240001", "REC240002"),
  recueil_date = as.Date("2024-03-06"),
  recueil_uf = "4501"
)

# L'IGS2 passe par des colonnes-chemins relatives à la racine du message
evenements$`igs2/@action` <- "creation"
for (a in attributs) evenements[[paste0("igs2/@", a)]] <- igs2[[a]]
evenements$`igs2/dateAction` <- ts("2024-03-06 08:00:00")
evenements$`igs2/valeurGlobale` <- igs2$valeurGlobale

fichiers <- write_evenements_etat_patient(
  evenements, NULL,                     # pas de table de diagnostics ici
  entete = entete,
  out_dir = "sortie/igs2",              # -> sortie/igs2/IGS240001.xml, IGS240002.xml
  xmlns = NULL
)

# Contrôle : une ligne par item, avec la classe et les points de chaque patient
controle <- tibble(item = attributs)
for (i in seq_len(nrow(igs2))) {
  controle[[paste0("classe_", igs2$evt_id[i])]] <- unlist(igs2[i, attributs])
}
for (i in seq_len(nrow(igs2))) {
  controle[[paste0("points_", igs2$evt_id[i])]] <- unlist(igs2[i, paste0("points_", attributs)])
}
