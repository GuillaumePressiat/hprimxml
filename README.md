---
output: github_document
---

<!-- README.md est généré à partir de README.Rmd : modifier README.Rmd puis lancer
     knitr::knit("README.Rmd"). Le code affiché est lu dans inst/exemples/ et exécuté. -->



# hprimxml

Écriture et lecture de fichiers [HPRIM XML](https://www.interopsante.org/hprim), standard
d'interopérabilité français, depuis et vers des **data.frames** ou **tibbles**, guidées par
les schémas XSD officiels (versions 1.03a à 2.4, embarqués dans le package).

C'est le portage R du package Python
[pyhprimxml](https://github.com/GuillaumePressiat/pyhprimxml) : mêmes tables, mêmes
fonctions, mêmes options, et **fichiers identiques octet pour octet**. Les tests du package
le vérifient sur tous les exemples ci-dessous.

> **Projet en cours de développement.** À ce stade sans garantie : ce code n'engage que
> celui qui l'utilise.

| Message | Écriture | Lecture |
|---|---|---|
| `evenementsPMSI` (RSS MCO) | `write_evenements_pmsi()` | `read_evenements_pmsi()` |
| `evenementsServeurActes` (actes CCAM) | `write_evenements_serveur_actes()` | `read_evenements_serveur_actes()` |
| `evenementsServeurEtatsPatient` (diagnostics, IGS2…) | `write_evenements_etat_patient()` | `read_evenements_etat_patient()` |
| Tout message HPRIM XML, sous forme de liste | `hprim_to_xml()`, `hprim_write_xml()` | `hprim_parse_xml()` |

Et aussi : `hprim_validate()` (contrôle d'un message contre son XSD) et `suivi_depots()`
(état des dépôts : quels `.xml` ont leur marqueur `.ok`).

Ce que permet le package :

- **des messages atomiques** : un message par unité que le standard sait créer, modifier ou
  supprimer, le **RUM** (`par_rum = TRUE`), l'**acte** (`par_acte = TRUE`) ou le **diagnostic**
  (`par_diagnostic = TRUE`), pour traiter, corriger et renvoyer chacune séparément ;
- **identifiant du message = nom du fichier**, règle contrôlée à l'écriture ;
- **dépôt sécurisé** : chaque `.xml` est écrit de façon atomique, et un marqueur `.ok` vide est
  déposé une fois le fichier complet (`fichier_ok = TRUE`) ; à la lecture, `exiger_ok = TRUE`
  ignore les dépôts inachevés ;
- **diagnostics** au choix en format long (une ligne par code), en colonne liste ou en chaîne
  séparée par des virgules ;
- **namespace** HPRIM, absent (`xmlns = NULL`) ou vide (`xmlns = ""`), et encodage UTF-8 ou
  ISO-8859-1 ;
- **validation XSD** de chaque fichier écrit, en mode erreur par défaut ;
- **colonnes-chemins** (`` `patient/@anonyme` ``, `` `medecinResponsable/noRPPS` ``…) pour
  atteindre n'importe quel élément du XSD ;
- **relecture** vers les mêmes tables, avec des colonnes typées (`Date`, `POSIXct`, `hms`,
  entiers).

## Guide

Cinq vignettes, publiées aussi comme articles du site :

1. **Découvrir hprimxml** : le tour du package en quelques minutes, et la carte des fonctions ;
2. **Écriture** : ePMSI, serveur d'actes, états patient (dont l'IGS2), RUM unitaires, options ;
3. **Lecture** : les mêmes messages, colonnes typées, `extra`, dépôts `.ok`, fichiers d'autres
   producteurs ;
4. **Validation** : `hprim_validate()`, `validate_xsd`, versions, erreurs courantes ;
5. **Arbres intermédiaires et listes** : `iter_evenements_*()`, `hprim_parse_xml()`,
   `hprim_to_xml()`.

```r
vignette("decouverte", package = "hprimxml")
```

Les pages d'aide des fonctions d'écriture contiennent les scripts d'exemple complets, à
exécuter avec `example()` : par exemple `example(write_evenements_etat_patient)` calcule un IGS2
détaillé à partir de mesures brutes et écrit les messages correspondants. Les scripts sont
aussi livrés dans le package : `list.files(system.file("exemples", package = "hprimxml"))`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("GuillaumePressiat/hprimxml", build_vignettes = TRUE)
```

Dépendances : `xml2`, `hms`, `tibble`.

Tous les exemples écrivent en **version 2.00**, déclarée une fois dans l'en-tête
(`entete <- list(version = "2.00", ...)`). Sans précision, le package écrit en 2.4, la
dernière version publiée ; l'argument `version` d'un appel l'emporte sur l'en-tête.

## 1. Écriture : des tables vers HPRIM XML

### Atomicité des messages

Chaque type de message a une **unité atomique** : l'objet que le standard sait créer,
modifier ou supprimer, parce qu'il porte l'attribut `action` et un identifiant. Un message
atomique transporte une seule de ces unités : il peut être accepté, rejeté ou renvoyé
indépendamment des autres.

| message | unité atomique | argument |
|---|---|---|
| ePMSI (`evenementsPMSI`) | le **RUM** | `par_rum = TRUE` |
| serveur d'actes (`evenementsServeurActes`) | l'**acte** (`acteCCAM`) | `par_acte = TRUE` |
| états patient (`evenementsServeurEtatsPatient`) | le **diagnostic** | `par_diagnostic = TRUE` |

Par défaut (`FALSE`), un message porte tout l'événement : le séjour et ses RUM, l'intervention
et ses actes, l'observation et ses diagnostics. En mode atomique, le contexte (patient, venue…)
est répété dans chaque message, `evt_id` devient `{evt_id}_{rang}` (`{evt_id}_{rum_id}` pour le
PMSI), et le gabarit `filename` dispose aussi de `{evt_origine}`, `{rang}` et `{acte_id}`.

Pour le PMSI, les diagnostics et les actes d'un RUM n'ont ni action ni identifiant : un message
par diagnostic ou par acte n'y aurait pas de sens propre. Pour transmettre des diagnostics un par
un, le canal naturel est le message états patient ; pour des actes, le serveur d'actes.

Exemple : un séjour à deux RUM, une intervention à trois actes et une observation à deux
diagnostics, écrits en messages atomiques (`inst/exemples/exemple_atomicite.R`).


```r
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
```

Les fichiers produits, un par unité atomique :



|fichier              |RUM |actes |diagnostics |
|:--------------------|:---|:-----|:-----------|
|actes/A1_ACT01.xml   |0   |1     |0           |
|actes/A1_ACT02.xml   |0   |1     |0           |
|actes/A1_ACT03.xml   |0   |1     |0           |
|diagnostics/D1_1.xml |0   |0     |1           |
|diagnostics/D1_2.xml |0   |0     |1           |
|rum/S1_1.xml         |1   |0     |0           |
|rum/S1_2.xml         |1   |0     |0           |



### Un séjour avec mutation, par séjour puis par RUM

Une patiente entre par les urgences en cardiologie (UM 1201), est mutée en médecine
(UM 3405), puis rentre à domicile : un RSS de deux RUM, avec de vrais codes CIM-10 et CCAM.
Le script (`inst/exemples/exemple_sejour.R`) écrit le séjour en un fichier, puis en RUM
unitaires avec leurs marqueurs `.ok`, et le relit.


```r
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
```

Les quatre tables, reliées par `evt_id` (le séjour, donc le fichier) et `rum_id` (le RUM) :

**`evenements`** : une ligne par séjour.



|evt_id  |ipp       |sexe |date_naissance |nda       |venue_date_entree |venue_heure_entree |venue_date_sortie |rss_emetteur |
|:-------|:---------|:----|:--------------|:---------|:-----------------|:------------------|:-----------------|:------------|
|S240001 |190101337 |F    |1958-03-12     |240000123 |2024-03-04        |10:20:00           |2024-03-09        |240000123    |



**`rum`** : une ligne par RUM ; `rum_emetteur` est l'identifiant XML du RUM, la colonne
`` `medecinResponsable/noRPPS` `` est une colonne-chemin.



|evt_id  |rum_id |rum_emetteur |um   |um_date_entree |um_heure_entree |um_mode_entree |um_provenance |um_date_sortie |um_mode_sortie |um_destination |medecinResponsable/noRPPS |
|:-------|:------|:------------|:----|:--------------|:---------------|:--------------|:-------------|:--------------|:--------------|:--------------|:-------------------------|
|S240001 |1      |S240001_1    |1201 |2024-03-04     |10:20:00        |8              |5             |2024-03-06     |6              |1              |10003456789               |
|S240001 |2      |S240001_2    |3405 |2024-03-06     |14:00:00        |6              |1             |2024-03-09     |8              |               |10009876543               |



**`diagnostics`** : format long, `type` dans DP, DR, DAS, DAD ; le point des codes CIM-10 est
retiré à l'écriture.



|evt_id  |rum_id |type |code  |
|:-------|:------|:----|:-----|
|S240001 |1      |DP   |I21.0 |
|S240001 |1      |DAS  |E11.9 |
|S240001 |1      |DAS  |I10   |
|S240001 |1      |DAS  |E78.0 |
|S240001 |2      |DP   |I50.1 |
|S240001 |2      |DAS  |N18.3 |
|S240001 |2      |DAS  |E11.9 |



**`actes`** : une ligne par acte CCAM ; `quantite` vaut 1 par défaut.



|evt_id  |rum_id |code    |phase |activite |date_realisation    |
|:-------|:------|:-------|:-----|:--------|:-------------------|
|S240001 |1      |DEQP003 |0     |1        |2024-03-04 10:45:00 |
|S240001 |1      |ZBQK002 |0     |1        |2024-03-04 11:30:00 |
|S240001 |2      |DEQP003 |0     |1        |2024-03-06 15:00:00 |



Les RUM relus, diagnostics en colonnes (`rum_relu`) :



|evt_id  |rum_id |rum_emetteur |um   |dp   |das               |
|:-------|:------|:------------|:----|:----|:-----------------|
|S240001 |1      |S240001_1    |1201 |I210 |[E119, I10, E780] |
|S240001 |2      |S240001_2    |3405 |I501 |[N183, E119]      |



Le second RUM, écrit en RUM unitaire (`sortie/rum/S240001_2.xml`) :

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<evenementsPMSI version="2.00">
  <enteteMessage>
    <identifiantMessage>S240001_2</identifiantMessage>
    <dateHeureProduction>2024-03-10T08:00:00</dateHeureProduction>
    <emetteur>
      <agents>
        <agent categorie="application">
          <code>DIM</code>
        </agent>
      </agents>
    </emetteur>
    <destinataire>
      <agents>
        <agent categorie="application">
          <code>DPI</code>
        </agent>
      </agents>
    </destinataire>
  </enteteMessage>
  <evenementPMSI>
    <patient>
      <identifiant>
        <emetteur>
          <valeur>190101337</valeur>
        </emetteur>
      </identifiant>
      <personnePhysique sexe="F">
        <dateNaissance>
          <date>1958-03-12</date>
        </dateNaissance>
      </personnePhysique>
    </patient>
    <venue>
      <identifiant>
        <emetteur>
          <valeur>240000123</valeur>
        </emetteur>
      </identifiant>
      <entree>
        <dateHeureOptionnelle>
          <date>2024-03-04</date>
          <heure>10:20:00</heure>
        </dateHeureOptionnelle>
      </entree>
      <sortie>
        <dateHeureOptionnelle>
          <date>2024-03-09</date>
        </dateHeureOptionnelle>
      </sortie>
    </venue>
    <rss>
      <identifiantRSS>
        <emetteur>240000123</emetteur>
      </identifiantRSS>
      <rum action="creation">
        <dateAction>2024-03-06T00:00:00</dateAction>
        <identifiant>
          <emetteur>S240001_2</emetteur>
        </identifiant>
        <medecinResponsable>
          <noRPPS>10009876543</noRPPS>
        </medecinResponsable>
        <uniteMedicale>
          <code>3405</code>
          <entree mode="6" complementMode="1">
            <date>2024-03-06</date>
            <heure>14:00:00</heure>
          </entree>
          <sortie mode="8">
            <date>2024-03-09</date>
          </sortie>
        </uniteMedicale>
        <diagnostics>
          <diagnosticPrincipal>
            <codeCim10>I501</codeCim10>
          </diagnosticPrincipal>
          <diagnosticsSignificatifs>
            <diagnosticSignificatif>
              <codeCim10>N183</codeCim10>
            </diagnosticSignificatif>
            <diagnosticSignificatif>
              <codeCim10>E119</codeCim10>
            </diagnosticSignificatif>
          </diagnosticsSignificatifs>
        </diagnostics>
        <actes>
          <acte>
            <CCAM>
              <codeActe>DEQP003</codeActe>
              <codePhase>0</codePhase>
              <codeActivite>1</codeActivite>
              <quantite>1</quantite>
              <dateRealisation>2024-03-06T15:00:00</dateRealisation>
            </CCAM>
          </acte>
        </actes>
      </rum>
    </rss>
  </evenementPMSI>
</evenementsPMSI>
```

### Diagnostics : format long, liste ou chaîne

Les diagnostics d'un RUM se donnent au choix en format long (table `diagnostics`), ou en
colonnes `dp`, `dr`, `das`, `dad` de `rum`, `das` et `dad` étant une colonne liste ou une chaîne
séparée par des virgules. Les trois formes produisent le même fichier ; le script le vérifie.


```r
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
```

### Règle : identifiant du message = nom du fichier

L'identifiant du message est **exactement le nom du fichier sans extension** : `{evt_id}.xml`
par défaut, `{evt_id}_{rum_id}.xml` avec `par_rum = TRUE`, ou le nom produit par le gabarit
`filename` (qui dispose aussi de `{rss_id}` et `{rum_id}`). Une colonne `identifiant_message`
contradictoire fait échouer l'écriture ; le XSD limite l'identifiant à 12 caractères (lettres,
chiffres, `-`, `,`, `_`) ; deux messages de même nom font échouer l'écriture.
`identifiant_egal_nom_fichier = FALSE` rend identifiant et nom de fichier indépendants.

### États patient : IGS2 détaillé

Chaque item de l'IGS2 est un attribut de `<igs2>` qui porte une **classe** (`age="60A69"`…),
plus le score global `valeurGlobale`. Le script part des pires valeurs des 24 premières heures
et applique la grille de la SFAR (<https://sfar.org/scores/igs2.php>, d'après Le Gall JR et
coll., *JAMA* 1993; 270: 2957-2963). Le XSD impose un repère : ici le `recueil` (colonnes
`recueil_id`, `recueil_date`, `recueil_uf`), qui donne la date **et l'UF** où les états ont été
observés, pour que le récepteur retrouve le RUM même un jour de mutation. Le contenu est
exclusif : un message porte soit des diagnostics, soit un IGS2.


```r
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
```

La table de contrôle produite par le script :



|item                        |classe_IGS240001 |classe_IGS240002   |points_IGS240001 |points_IGS240002 |
|:---------------------------|:----------------|:------------------|:----------------|:----------------|
|age                         |60A69            |superieurEgalA80   |12               |18               |
|frequenceCardiaque          |120A159          |70A119             |4                |0                |
|tensionArterielleSystolique |70A99            |superieurEgalA200  |5                |2                |
|temperature                 |inferieurA39     |superieurEgalA39   |0                |3                |
|paO2_FIO2                   |100A199          |rien               |9                |0                |
|diurese                     |0,5A0,999        |superieurA1        |4                |0                |
|ureeSanguine                |10A29,9          |inferieurA10       |6                |0                |
|leucocytes                  |1A19,9           |superieurEgalA20   |0                |3                |
|kaliemie                    |3A4,9            |superieurEgalA5    |0                |3                |
|natremie                    |125A144          |superieurEgalA145  |0                |1                |
|HCO3                        |15A19            |superieurEgalA20   |3                |0                |
|bilirubine                  |inferieurA68,4   |superieurA102,6    |0                |9                |
|scoreGlasgow                |11A13            |14A18              |5                |0                |
|maladieChronique            |aucune           |cancerMetastatique |0                |9                |
|modeAdmission               |medecine         |chirUrgente        |6                |8                |



Le fichier produit pour le premier patient :

```xml
<?xml version="1.0" encoding="UTF-8" standalone="yes"?>
<evenementsServeurEtatsPatient version="2.00">
  <enteteMessage>
    <identifiantMessage>IGS240001</identifiantMessage>
    <dateHeureProduction>2024-03-06T09:00:00</dateHeureProduction>
    <emetteur>
      <agents>
        <agent categorie="application">
          <code>REA</code>
        </agent>
      </agents>
    </emetteur>
    <destinataire>
      <agents>
        <agent categorie="application">
          <code>DIM</code>
        </agent>
      </agents>
    </destinataire>
  </enteteMessage>
  <patient>
    <identifiant>
      <emetteur>
        <valeur>190101337</valeur>
      </emetteur>
    </identifiant>
    <personnePhysique sexe="M">
      <dateNaissance>
        <date>1956-05-14</date>
      </dateNaissance>
    </personnePhysique>
  </patient>
  <venue>
    <identifiant>
      <emetteur>
        <valeur>240000123</valeur>
      </emetteur>
    </identifiant>
    <entree>
      <dateHeureOptionnelle>
        <date>2024-03-05</date>
      </dateHeureOptionnelle>
    </entree>
  </venue>
  <recueil>
    <identifiant>
      <emetteur>REC240001</emetteur>
    </identifiant>
    <date>2024-03-06</date>
    <uniteFonctionnelle>
      <code>4501</code>
    </uniteFonctionnelle>
  </recueil>
  <igs2 action="creation" modeAdmission="medecine" maladieChronique="aucune" scoreGlasgow="11A13" age="60A69" tensionArterielleSystolique="70A99" frequenceCardiaque="120A159" temperature="inferieurA39" paO2_FIO2="100A199" diurese="0,5A0,999" ureeSanguine="10A29,9" leucocytes="1A19,9" kaliemie="3A4,9" natremie="125A144" HCO3="15A19" bilirubine="inferieurA68,4">
    <dateAction>2024-03-06T08:00:00</dateAction>
    <valeurGlobale>54</valeurGlobale>
  </igs2>
</evenementsServeurEtatsPatient>
```

Les règles de recueil (pires valeurs, Glasgow avant sédation, bilirubine seulement en cas
d'ictère…) restent celles de votre service.

### Régénérer les fichiers d'exemple du projet

Les scripts `exemple_pmsi.R`, `exemple_serveur_actes.R` et `exemple_etat_patient.R` de
`inst/exemples/` régénèrent les trois fichiers de référence du projet (PMSI, serveur d'actes,
états patient), en version 2.00 :

```r
source(system.file("exemples", "exemple_pmsi.R", package = "hprimxml"))
```

## 2. Lecture : des fichiers HPRIM XML vers les tables

Les fonctions `read_*()` sont l'inverse exact des fonctions `write_*()` : elles renvoient
les mêmes tables (tibbles), avec les mêmes noms de colonnes.

```r
t <- read_evenements_pmsi("dossier/")          # fichier(s), dossier, motif glob
t$evenements; t$rum; t$diagnostics; t$actes
```

- **lecture guidée par le XSD** : un élément répétable est toujours une liste, même unique ;
- **colonnes typées** : `xsd:date` → `Date`, `xsd:dateTime` → `POSIXct` (UTC, heure du
  fichier conservée), `xsd:time` → `hms`, entiers → `integer`, le reste en texte ;
- **clés reconstruites** : `evt_id` est le nom du fichier, `rum_id` le rang du RUM ;
- **en-tête** relu dans des colonnes de `evenements`, qui priment à la réécriture ;
- `extra = list(rum = "@classant")` : colonnes-chemins supplémentaires ;
- `format_diagnostics = "colonnes"` : `dp`, `dr`, `das`, `dad` dans `rum` ;
- `exiger_ok = TRUE` : ne lire que les dépôts terminés.

## Options d'écriture

| argument | valeurs | effet |
|---|---|---|
| `xmlns` | `HPRIM_NS` (défaut), `NULL`, `""` | namespace HPRIM ; aucune déclaration ; `xmlns=""` littéral |
| `validate_xsd` | `"error"` (défaut), `"warn"`, `FALSE` | contrôle de chaque fichier contre son XSD |
| `version` | `"2.4"`, `"2.00"`… | version du schéma ; sinon `entete$version`, sinon 2.4 |
| `filename` | gabarit ou fonction | nom de chaque fichier |
| `par_rum`, `par_acte`, `par_diagnostic` | `FALSE` (défaut), `TRUE` | un message par unité atomique : RUM, acte, diagnostic |
| `fichier_ok` | `FALSE` (défaut), `TRUE` | dépose un `.ok` vide une fois chaque `.xml` complet |
| `identifiant_egal_nom_fichier` | `TRUE` (défaut), `FALSE` | identifiantMessage = nom du fichier |
| `colmap` | `list(ancien = "nouveau")` | renommage de colonnes existantes |
| `encoding` | `"UTF-8"` (défaut), `"ISO-8859-1"` | encodage des fichiers |

## Cohérence avec pyhprimxml

`tests/testthat/test-exemples.R` exécute chaque script de `inst/exemples/` et vérifie que les
fichiers produits (`.xml` et `.ok`) sont identiques, octet pour octet, à ceux du script Python
équivalent. Les fichiers de référence sont produits par :

```bash
PYTHONPATH=../pyhprimxml python tools/fixtures_python.py
```

La couche de base (`hprim_parse_xml()`, `hprim_to_xml()`) est vérifiée de la même façon sur
les fichiers d'exemple, pour six variantes d'options.

Les deux packages écrivent le XML avec libxml2 : lxml en Python, xml2 en R. La structure (ordre
des éléments, attributs, formats, validation) vient du XSD ; la mise en forme du texte final
(indentation de deux espaces, échappement, encodage) est celle de libxml2, commune aux deux.

## Versions et releases

Les versions sont publiées comme **releases GitHub** (rien sur le CRAN), par le workflow
`.github/workflows/release.yaml`, à chaque étiquette de version poussée :

```bash
git tag 0.2.0+alpha1.0 && git push origin 0.2.0+alpha1.0    # version de test (pré-release)
git tag v0.2.0 && git push origin v0.2.0                     # version finale
```

La partie avant `+` doit être la version de `DESCRIPTION` : sinon le workflow s'arrête. Il
lance ensuite `R CMD check` (tests, exemples, vignettes), construit le `.tar.gz` et l'attache à
la release, avec des notes générées à partir des commits. Une étiquette avec suffixe donne une
**pré-release**.

R n'accepte que des chiffres dans un numéro de version : le `.tar.gz` garde la version de
`DESCRIPTION` (`hprimxml_0.2.0.tar.gz`), et l'étiquette complète est inscrite dans un champ
dédié, lisible une fois le package installé :

```r
packageDescription("hprimxml")[["Config/hprimxml/release"]]    # "0.2.0+alpha1.0"
```

Le `.tar.gz` d'une release contient les **vignettes déjà construites** : l'installer depuis la
release installe aussi les vignettes. Après l'avoir téléchargé depuis la page de la release
(ou avec `gh release download 0.2.0+alpha1.0`) :

```r
install.packages("hprimxml_0.2.0.tar.gz", repos = NULL, type = "source")
```

## Documentation

Le site (README, vignettes et référence des fonctions) est construit avec pkgdown et publié sur GitHub
Pages par `.github/workflows/pkgdown.yaml`, qui vérifie au passage que ce README est à jour.

```r
knitr::knit("README.Rmd")        # après toute modification d'un exemple
pkgdown::build_site()
```
