#' @include common.R
NULL

acte_ccam_node <- function(a) {
  modifs <- g(a, "modificateurs")
  node <- list(
    action = g(a, "action", "creation"),
    facturable = g(a, "facturable"),
    valide = g(a, "valide"),
    identifiant = list(emetteur = a$acte_id),
    codeActe = a$code,
    codeActeExtensionPMSI = g(a, "extension_pmsi"),
    codeActivite = a$activite,
    codePhase = a$phase,
    execute = date_heure(a, "date_execution", "heure_execution"),
    executant = list(
      medecins = list(medecinExecutant = list(
        principal = g(a, "medecin_principal", "oui"),
        medecin = list(identification = list(code = a$medecin))
      )),
      uniteFonctionnelle = code_node(g(a, "uf"))
    ),
    modificateurs = if (is.null(modifs)) NULL else list(modificateur = strsplit(as.character(modifs), "")[[1]]),
    codeAssociationNonPrevue = g(a, "association_non_prevue"),
    codeExtensionDocumentaire = g(a, "extension_documentaire"),
    quantite = g(a, "quantite")
  )
  apply_paths(node, a)
}

#' Arbres des messages evenementsServeurActes, un par événement
#' @inheritParams write_evenements_serveur_actes
#' @export
iter_evenements_serveur_actes <- function(evenements, actes, entete = NULL, version = NULL, colmap = NULL,
                                          par_acte = FALSE) {
  evt <- rows_of(as_table(evenements, "evenements", c("evt_id", "ipp", "sexe", "date_naissance"), colmap))
  act <- rows_of(as_table(actes, "actes", c("evt_id", "acte_id", "code", "activite", "phase",
                                            "date_execution", "medecin"), colmap))
  act_by <- rows_by(act, "evt_id")
  arbre <- function(e, acts) {
    evenement <- list(
      patient = patient_tree(e), venue = venue_tree(e), intervention = intervention_tree(e),
      actesCCAM = if (length(acts)) list(acteCCAM = lapply(acts, acte_ccam_node)) else NULL
    )
    list(evenementsServeurActes = c(
      root_attrs(entete, e, version),
      list(enteteMessage = entete_tree(entete, e), evenementServeurActe = apply_paths(evenement, e))
    ))
  }
  out <- list()
  for (e in evt) {
    acts <- act_by[[as.character(e$evt_id)]] %||% list()
    if (!par_acte) {
      out[[length(out) + 1L]] <- list(evt = e, tree = arbre(e, acts))
      next
    }
    for (x in decouper(e, acts, extra = function(a) list(acte_id = a$acte_id))) {
      out[[length(out) + 1L]] <- list(evt = x$evt, tree = arbre(x$evt, list(x$objet)))
    }
  }
  out
}

#' Écrire des messages evenementsServeurActes (actes CCAM) depuis des tables
#'
#' Un fichier par événement. `evenements` : patient, venue (`nda`, `venue_*`), intervention
#' (`intervention_*`, `demande_*`). `actes` : une ligne par acte CCAM, avec `evt_id`,
#' `acte_id`, `code`, `activite`, `phase`, `date_execution`, `medecin` et en option
#' `heure_execution`, `uf`, `medecin_principal`, `quantite`, `action`, `facturable`, `valide`,
#' `modificateurs`, `extension_pmsi`, `extension_documentaire`, `association_non_prevue`.
#'
#' **Unité atomique du serveur d'actes : l'acte** (`acteCCAM`), porteur de l'attribut
#' `action` et d'un identifiant. `par_acte = TRUE` écrit un message par acte.
#'
#' @param evenements,actes Tables.
#' @param par_acte Un message par acte (unité atomique) au lieu d'un par événement. Le
#'   contexte (patient, venue, intervention) est répété ; `evt_id` vaut `{evt_id}_{rang}`, et le
#'   gabarit `filename` dispose aussi de `{evt_origine}`, `{rang}` et `{acte_id}`.
#' @inheritParams write_evenements_pmsi
#' @return Les chemins écrits (invisible).
#' @examples
#' # Les exemples écrivent dans un dossier temporaire
#' ancien <- setwd(tempdir())
#' @example inst/exemples/exemple_serveur_actes.R
#' @examples
#' fichiers
#' setwd(ancien)
#' @export
write_evenements_serveur_actes <- function(evenements, actes, entete = NULL, out_dir,
                                           filename = "{evt_id}.xml", version = NULL, xmlns = HPRIM_NS,
                                           encoding = "UTF-8", validate_xsd = "error", colmap = NULL,
                                           identifiant_egal_nom_fichier = TRUE, fichier_ok = FALSE,
                                           par_acte = FALSE) {
  trees <- iter_evenements_serveur_actes(evenements, actes, entete, version, colmap, par_acte)
  write_trees(trees, out_dir, filename, xmlns, encoding, validate_xsd, entete,
              identifiant_egal_nom_fichier, fichier_ok)
}

EVT_COLS_SA <- c("evt_id", ENTETE_KEYS, "ipp", "ipp_recepteur", "sexe", "nom_usuel", "nom_naissance",
                 "prenom", "date_naissance", "nda", "nda_recepteur", "venue_date_entree",
                 "venue_heure_entree", "venue_mode_entree", "venue_uf_responsable",
                 "venue_date_sortie", "venue_heure_sortie", "intervention_id",
                 "intervention_date_debut", "intervention_heure_debut", "intervention_date_fin",
                 "intervention_heure_fin", "intervention_uf", "demande_date", "demande_heure",
                 "demande_uf")
ACTE_SA_COLS <- c("evt_id", "acte_id", "code", "extension_pmsi", "activite", "phase", "date_execution",
                  "heure_execution", "medecin", "medecin_principal", "uf", "quantite", "action",
                  "facturable", "valide", "modificateurs", "association_non_prevue",
                  "extension_documentaire")

#' Lire des messages evenementsServeurActes vers des tables
#' @inheritParams read_evenements_pmsi
#' @return Liste de tibbles `evenements`, `actes`.
#' @examples
#' f <- system.file("extdata", "actes_00001.xml", package = "hprimxml")
#' t <- read_evenements_serveur_actes(f)
#' t$evenements[, c("evt_id", "ipp", "nda", "intervention_id")]
#' t$actes[, c("evt_id", "acte_id", "code", "date_execution", "medecin")]
#' @export
read_evenements_serveur_actes <- function(sources, extra = NULL, version = NULL, exiger_ok = FALSE) {
  evts <- list(); actes <- list()
  for (s in iter_sources(sources, exiger_ok)) {
    root <- hprim_parse_xml(s$src, version = version)$evenementsServeurActes
    head <- entete_cols(root)
    events <- as_list_of(root$evenementServeurActe)
    for (i in seq_along(events)) {
      ev <- events[[i]]
      evt_id <- if (length(events) == 1) s$id else paste0(s$id, "_", i)
      evts[[length(evts) + 1L]] <- c(list(evt_id = evt_id), head, patient_cols(ev), venue_cols(ev),
                                     intervention_cols(ev), extra_cols(ev, extra$evenements))
      for (a in pick_list(ev, "actesCCAM/acteCCAM")) {
        actes[[length(actes) + 1L]] <- c(list(
          evt_id = evt_id, acte_id = pick(a, "identifiant/emetteur"),
          code = g(a, "codeActe"), extension_pmsi = g(a, "codeActeExtensionPMSI"),
          activite = g(a, "codeActivite"), phase = g(a, "codePhase"),
          date_execution = pick(a, "execute/date"), heure_execution = pick(a, "execute/heure"),
          medecin = pick(a, "executant/medecins/medecinExecutant/medecin/identification/code"),
          medecin_principal = pick(a, "executant/medecins/medecinExecutant/@principal"),
          uf = pick(a, "executant/uniteFonctionnelle/code"),
          quantite = g(a, "quantite"), action = g(a, "action"),
          facturable = g(a, "facturable"), valide = g(a, "valide"),
          modificateurs = modificateurs_str(a),
          association_non_prevue = g(a, "codeAssociationNonPrevue"),
          extension_documentaire = g(a, "codeExtensionDocumentaire")
        ), extra_cols(a, extra$actes))
      }
    }
  }
  list(evenements = frame(evts, EVT_COLS_SA), actes = frame(actes, ACTE_SA_COLS))
}
