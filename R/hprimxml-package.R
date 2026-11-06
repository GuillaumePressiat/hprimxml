#' hprimxml : écriture et lecture de fichiers HPRIM XML
#'
#' Écriture et lecture de messages HPRIM XML depuis et vers des tables (data.frames,
#' tibbles) : PMSI (RSS MCO), serveur d'actes, états patient (diagnostics, IGS2…), guidées
#' par les schémas XSD officiels embarqués (versions 1.03a à 2.4).
#'
#' @section Pour commencer :
#' Cinq vignettes forment le guide du package :
#' * `vignette("decouverte", package = "hprimxml")` : le tour du package ;
#' * `vignette("ecriture", package = "hprimxml")` : ePMSI, serveur d'actes, états patient et IGS2 ;
#' * `vignette("lecture", package = "hprimxml")` ;
#' * `vignette("validation", package = "hprimxml")` ;
#' * `vignette("arbres", package = "hprimxml")` : arbres intermédiaires et listes.
#'
#' Les vignettes ne sont installées qu'avec
#' `remotes::install_github("GuillaumePressiat/hprimxml", build_vignettes = TRUE)` ; elles sont
#' aussi publiées sur le site du package.
#'
#' @section Scripts d'exemple :
#' Les scripts complets sont livrés dans le package et servent d'exemples aux pages d'aide :
#' `list.files(system.file("exemples", package = "hprimxml"))`. Par exemple, l'IGS2 détaillé
#' (mesures brutes, classes, points, score, message états patient) :
#' `file.show(system.file("exemples", "exemple_igs2.R", package = "hprimxml"))`, ou
#' `example(write_evenements_etat_patient)` pour l'exécuter.
#'
#' @keywords internal
"_PACKAGE"
