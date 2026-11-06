#' @include common.R
NULL

#' État des dépôts : quels .xml ont leur marqueur .ok
#'
#' Protocole : l'émetteur dépose le `.xml`, puis, une fois celui-ci écrit intégralement,
#' un `.ok` vide de même nom (`fichier_ok = TRUE` des fonctions d'écriture). Le
#' destinataire ne traite un `.xml` qu'une fois son `.ok` présent (`exiger_ok = TRUE` des
#' fonctions de lecture).
#'
#' @param fichiers Dossier, motif glob ou vecteur de `.xml`.
#' @param extension_ok Extension des marqueurs.
#' @return Tibble : `evt_id`, `fichier`, `date_xml`, `complet`, `date_ok`. Les `.ok` sans
#'   `.xml` correspondant sont ajoutés avec `fichier` manquant et `complet` faux.
#' @examples
#' ancien <- setwd(tempdir())
#' evenements <- data.frame(evt_id = "S1", ipp = "123", sexe = "F", date_naissance = as.Date("1960-01-01"),
#'                          nda = "240000001", venue_date_entree = as.Date("2024-05-02"))
#' rum <- data.frame(evt_id = "S1", rum_id = 1:2, um = c("1201", "3405"),
#'                   um_date_entree = as.Date(c("2024-05-02", "2024-05-04")), um_mode_entree = c("8", "6"),
#'                   dp = c("I21.0", "I50.1"))
#' write_evenements_pmsi(evenements, rum, entete = list(emetteur = "DIM", destinataire = "DPI"),
#'                       out_dir = "depot", par_rum = TRUE, fichier_ok = TRUE)
#' file.remove("depot/S1_2.ok")            # dépôt « en cours »
#' suivi_depots("depot")[, c("evt_id", "complet")]
#' read_evenements_pmsi("depot", exiger_ok = TRUE)$evenements$evt_id
#' setwd(ancien)
#' @export
suivi_depots <- function(fichiers, extension_ok = ".ok") {
  srcs <- iter_sources(fichiers)
  rows <- list(); vus <- character(); dossiers <- character()
  for (s in srcs) {
    ok <- chemin_ok(s$src, extension_ok)
    present <- file.exists(ok)
    vus <- c(vus, normalizePath(ok, mustWork = FALSE))
    dossiers <- union(dossiers, dirname(s$src))
    rows[[length(rows) + 1L]] <- list(evt_id = s$id, fichier = s$src, date_xml = file.mtime(s$src),
                                      complet = present, date_ok = if (present) file.mtime(ok) else NULL)
  }
  for (d in sort(dossiers)) {
    for (ok in sort(list.files(d, pattern = paste0(gsub(".", "\\.", extension_ok, fixed = TRUE), "$"),
                               full.names = TRUE))) {
      if (!normalizePath(ok) %in% vus) {
        rows[[length(rows) + 1L]] <- list(evt_id = tools::file_path_sans_ext(basename(ok)), fichier = NULL,
                                          date_xml = NULL, complet = FALSE, date_ok = file.mtime(ok))
      }
    }
  }
  out <- frame(rows, c("evt_id", "fichier", "date_xml", "complet", "date_ok"))
  out
}
