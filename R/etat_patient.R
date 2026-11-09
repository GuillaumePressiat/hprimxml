#' @include serveur_actes.R common.R
NULL

recueil_tree <- function(e) {
  if (is.null(g(e, "recueil_date")) && is.null(g(e, "recueil_id"))) return(NULL)
  list(
    identifiant = list(emetteur = g(e, "recueil_id")),
    date = g(e, "recueil_date"),
    heure = g(e, "recueil_heure"),
    uniteFonctionnelle = code_node(g(e, "recueil_uf"))
  )
}

verifier_repere <- function(e) {
  reperes <- c(intervention = !is.null(g(e, "intervention_id")),
               dateObservation = !is.null(g(e, "date_observation")),
               recueil = !is.null(g(e, "recueil_id")) || !is.null(g(e, "recueil_date")))
  if (sum(reperes) > 1) {
    stop(sprintf(paste0("\u00c9v\u00e9nement %s : un seul rep\u00e8re possible (intervention, ",
                        "dateObservation ou recueil), re\u00e7u : %s. La date d'un recueil se donne ",
                        "dans recueil_date (et recueil_heure)."),
                 e$evt_id, paste(names(reperes)[reperes], collapse = ", ")), call. = FALSE)
  }
}

diagnostic_etat_node <- function(d) {
  node <- list(
    action = g(d, "action", "creation"),
    type = tolower(as.character(d$type)),
    dateAction = g(d, "date_action"),
    codeCim10 = gsub(".", "", trimws(as.character(d$code)), fixed = TRUE)
  )
  apply_paths(node, d)
}

#' Arbres des messages evenementsServeurEtatsPatient, un par événement
#' @inheritParams write_evenements_etat_patient
#' @export
iter_evenements_etat_patient <- function(evenements, diagnostics = NULL, entete = NULL, version = NULL,
                                         colmap = NULL) {
  evt <- rows_of(as_table(evenements, "evenements", c("evt_id", "ipp", "sexe", "date_naissance"), colmap))
  dia <- rows_of(as_table(diagnostics, "diagnostics", c("evt_id", "type", "code"), colmap))
  dia_by <- rows_by(dia, "evt_id")
  lapply(evt, function(e) {
    verifier_repere(e)
    diags <- dia_by[[as.character(e$evt_id)]] %||% list()
    contenu <- c(root_attrs(entete, e, version), list(
      enteteMessage = entete_tree(entete, e),
      patient = patient_tree(e), venue = venue_tree(e), intervention = intervention_tree(e),
      dateObservation = date_heure(e, "date_observation", "heure_observation"),
      recueil = recueil_tree(e),
      Diagnostics = if (length(diags)) list(diagnostic = lapply(diags, diagnostic_etat_node)) else NULL,
      # obligatoire (même vide) en 2.4, facultatif avant : supprimé s'il est vide et facultatif
      divers = named_list()
    ))
    list(evt = e, tree = list(evenementsServeurEtatsPatient = apply_paths(contenu, e)))
  })
}

#' Écrire des messages evenementsServeurEtatsPatient depuis des tables
#'
#' Un fichier par événement. `evenements` : patient, venue, intervention, et le repère :
#' `date_observation`, `heure_observation` (élément `dateObservation`), ou
#' `recueil_id`, `recueil_date`, `recueil_heure`, `recueil_uf` (élément `recueil` : date et
#' UF où les états ont été observés, le repère le plus précis) ; les colonnes-chemins
#' sont relatives à la racine du message (ex. `igs2/@age`). `diagnostics` : une ligne par
#' diagnostic, `evt_id`, `type` (dp, dr, da, ds, dd, fpc, mm, ae), `code`, et en option
#' `date_action`, `action`.
#'
#' Le XSD impose un repère (intervention, dateObservation ou recueil) et un contenu exclusif :
#' diagnostics, ou IGS2, ou dépendances… En 2.4, un élément `<divers/>` est émis.
#'
#' @param evenements,diagnostics Tables.
#' @inheritParams write_evenements_pmsi
#' @return Les chemins écrits (invisible).
#' @examples
#' # Les exemples écrivent dans un dossier temporaire
#' ancien <- setwd(tempdir())
#' @example inst/exemples/exemple_igs2.R
#' @example inst/exemples/exemple_etat_patient.R
#' @examples
#' # IGS2 : classe et points de chaque item, par patient
#' print(controle, width = Inf)
#' igs2$valeurGlobale
#' setwd(ancien)
#' @export
write_evenements_etat_patient <- function(evenements, diagnostics = NULL, entete = NULL, out_dir,
                                          filename = "{evt_id}.xml", version = NULL, xmlns = HPRIM_NS,
                                          encoding = "UTF-8", validate_xsd = "error", colmap = NULL,
                                          identifiant_egal_nom_fichier = TRUE, fichier_ok = FALSE) {
  trees <- iter_evenements_etat_patient(evenements, diagnostics, entete, version, colmap)
  write_trees(trees, out_dir, filename, xmlns, encoding, validate_xsd, entete,
              identifiant_egal_nom_fichier, fichier_ok)
}

EVT_COLS_EP <- c(EVT_COLS_SA, "date_observation", "heure_observation",
                 "recueil_id", "recueil_date", "recueil_heure", "recueil_uf")
DIAG_EP_COLS <- c("evt_id", "type", "code", "date_action", "action")

#' Lire des messages evenementsServeurEtatsPatient vers des tables
#' @inheritParams read_evenements_pmsi
#' @return Liste de tibbles `evenements`, `diagnostics`.
#' @examples
#' f <- system.file("extdata", "etat_patient_00001.xml", package = "hprimxml")
#' t <- read_evenements_etat_patient(f)
#' t$diagnostics
#'
#' # un IGS2 se relit par des colonnes-chemins (voir l'exemple de write_evenements_etat_patient)
#' # read_evenements_etat_patient("sortie/igs2", extra = list(evenements = c("igs2/@age", "igs2/valeurGlobale")))
#' @export
read_evenements_etat_patient <- function(sources, extra = NULL, version = NULL, exiger_ok = FALSE) {
  evts <- list(); diags <- list()
  for (s in iter_sources(sources, exiger_ok)) {
    root <- hprim_parse_xml(s$src, version = version)$evenementsServeurEtatsPatient
    evts[[length(evts) + 1L]] <- c(list(evt_id = s$id), entete_cols(root), patient_cols(root),
                                   venue_cols(root), intervention_cols(root),
                                   list(date_observation = pick(root, "dateObservation/date"),
                                        heure_observation = pick(root, "dateObservation/heure"),
                                        recueil_id = pick(root, "recueil/identifiant/emetteur"),
                                        recueil_date = pick(root, "recueil/date"),
                                        recueil_heure = pick(root, "recueil/heure"),
                                        recueil_uf = pick(root, "recueil/uniteFonctionnelle/code")),
                                   extra_cols(root, extra$evenements))
    for (d in pick_list(root, "Diagnostics/diagnostic")) {
      diags[[length(diags) + 1L]] <- c(list(evt_id = s$id, type = g(d, "type"), code = g(d, "codeCim10"),
                                            date_action = g(d, "dateAction"), action = g(d, "action")),
                                       extra_cols(d, extra$diagnostics))
    }
  }
  list(evenements = frame(evts, EVT_COLS_EP), diagnostics = frame(diags, DIAG_EP_COLS))
}
