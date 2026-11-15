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

acte_ngap_node <- function(a) {
  nabm <- liste_codes(a$nabm)
  node <- list(
    action = g(a, "action", "creation"),
    facturable = g(a, "facturable"),
    valide = g(a, "valide"),
    identifiant = list(emetteur = a$acte_id),
    lettreCle = a$lettre_cle,
    coefficient = a$coefficient,
    denombrement = g(a, "denombrement"),
    quantite = g(a, "quantite"),
    execute = date_heure(a, "date_execution", "heure_execution"),
    prestataire = list(
      medecins = list(medecin = list(identification = list(code = a$medecin))),
      uniteFonctionnelle = code_node(g(a, "uf"))
    ),
    NABMs = if (is.null(nabm)) NULL else list(code = nabm),
    montant = list(
      montantTotal = g(a, "montant_total"),
      numeroForfaitTechnique = g(a, "numero_forfait_technique"),
      numeroAgrementAppareil = g(a, "numero_agrement_appareil")
    )
  )
  apply_paths(node, a)
}

#' Arbres des messages evenementsServeurActes, un par événement
#' @inheritParams write_evenements_serveur_actes
#' @export
iter_evenements_serveur_actes <- function(evenements, actes_ccam = NULL, actes_ngap = NULL, entete = NULL,
                                          version = NULL, colmap = NULL, par_acte = FALSE) {
  evt <- rows_of(as_table(evenements, "evenements", c("evt_id", "ipp", "sexe", "date_naissance"), colmap))
  act <- rows_of(as_table(actes_ccam, "actes_ccam", c("evt_id", "acte_id", "code", "activite", "phase",
                                            "date_execution", "medecin"), colmap))
  ngap <- rows_of(as_table(actes_ngap, "actes_ngap", c("evt_id", "acte_id", "lettre_cle", "coefficient",
                                                        "date_execution", "medecin"), colmap))
  act_by <- rows_by(act, "evt_id")
  ngap_by <- rows_by(ngap, "evt_id")
  arbre <- function(e, acts, ngaps = list()) {
    evenement <- list(
      patient = patient_tree(e), venue = venue_tree(e), intervention = intervention_tree(e),
      actesCCAM = if (length(acts)) list(acteCCAM = lapply(acts, acte_ccam_node)) else NULL,
      actesNGAP = if (length(ngaps)) list(acteNGAP = lapply(ngaps, acte_ngap_node)) else NULL
    )
    list(evenementsServeurActes = c(
      root_attrs(entete, e, version),
      list(enteteMessage = entete_tree(entete, e), evenementServeurActe = apply_paths(evenement, e))
    ))
  }
  out <- list()
  for (e in evt) {
    acts <- act_by[[as.character(e$evt_id)]] %||% list()
    ngaps <- ngap_by[[as.character(e$evt_id)]] %||% list()
    if (!par_acte) {
      out[[length(out) + 1L]] <- list(evt = e, tree = arbre(e, acts, ngaps))
      next
    }
    # un message par acte, CCAM puis NGAP ; le rang court sur l'ensemble
    objets <- c(lapply(acts, function(a) c(a, list(.sorte = "ccam"))),
                lapply(ngaps, function(a) c(a, list(.sorte = "ngap"))))
    for (x in decouper(e, objets, extra = function(a) list(acte_id = a$acte_id))) {
      o <- x$objet
      tree <- if (identical(o$.sorte, "ccam")) arbre(x$evt, list(o)) else arbre(x$evt, list(), list(o))
      out[[length(out) + 1L]] <- list(evt = x$evt, tree = tree)
    }
  }
  out
}

#' Écrire des messages evenementsServeurActes (actes CCAM) depuis des tables
#'
#' Un fichier par événement. `evenements` : patient, venue (`nda`, `venue_*`), intervention
#' (`intervention_*`, `demande_*`). `actes_ccam` : une ligne par acte CCAM, avec `evt_id`,
#' `acte_id`, `code`, `activite`, `phase`, `date_execution`, `medecin` et en option
#' `heure_execution`, `uf`, `medecin_principal`, `quantite`, `action`, `facturable`, `valide`,
#' `modificateurs`, `extension_pmsi`, `extension_documentaire`, `association_non_prevue`.
#'
#' **Unité atomique du serveur d'actes : l'acte** (`acteCCAM`), porteur de l'attribut
#' `action` et d'un identifiant. `par_acte = TRUE` écrit un message par acte.
#'
#' `actes_ngap` : une ligne par acte NGAP (consultation, forfait technique, biologie), avec
#' `evt_id`, `acte_id`, `lettre_cle`, `coefficient`, `date_execution`, `medecin` et en option
#' `heure_execution`, `uf`, `quantite`, `denombrement`, `nabm` (codes NABM : colonne liste ou
#' chaîne séparée par des virgules), `montant_total`, `numero_forfait_technique`,
#' `numero_agrement_appareil`, `action`, `facturable`, `valide`. Un message peut porter des
#' actes CCAM, des actes NGAP, ou les deux ; `par_acte` découpe les deux.
#'
#' @param evenements,actes_ccam,actes_ngap Tables (`actes_ccam` : CCAM ; `actes_ngap` : NGAP).
#' @param actes Ancien nom de `actes_ccam`, encore accepté avec un avertissement.
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
write_evenements_serveur_actes <- function(evenements, actes_ccam = NULL, actes_ngap = NULL, entete = NULL, out_dir,
                                           filename = "{evt_id}.xml", version = NULL, xmlns = HPRIM_NS,
                                           encoding = "UTF-8", validate_xsd = "error", colmap = NULL,
                                           identifiant_egal_nom_fichier = TRUE, fichier_ok = FALSE,
                                           par_acte = FALSE, actes = NULL) {
  actes_ccam <- ancien_nom_actes(actes_ccam, actes)
  trees <- iter_evenements_serveur_actes(evenements, actes_ccam, actes_ngap, entete, version, colmap, par_acte)
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
ACTE_NGAP_COLS <- c("evt_id", "acte_id", "lettre_cle", "coefficient", "quantite", "denombrement",
                    "date_execution", "heure_execution", "medecin", "uf", "nabm", "montant_total",
                    "numero_forfait_technique", "numero_agrement_appareil", "action", "facturable",
                    "valide")
ACTE_SA_COLS <- c("evt_id", "acte_id", "code", "extension_pmsi", "activite", "phase", "date_execution",
                  "heure_execution", "medecin", "medecin_principal", "uf", "quantite", "action",
                  "facturable", "valide", "modificateurs", "association_non_prevue",
                  "extension_documentaire")

#' Lire des messages evenementsServeurActes vers des tables
#' @inheritParams read_evenements_pmsi
#' @return Liste de tibbles `evenements`, `actes_ccam` (CCAM), `actes_ngap` (NGAP, avec une colonne
#'   liste `nabm`).
#' @examples
#' f <- system.file("extdata", "actes_00001.xml", package = "hprimxml")
#' t <- read_evenements_serveur_actes(f)
#' t$evenements[, c("evt_id", "ipp", "nda", "intervention_id")]
#' t$actes_ccam[, c("evt_id", "acte_id", "code", "date_execution", "medecin")]
#' @export
read_evenements_serveur_actes <- function(sources, extra = NULL, version = NULL, exiger_ok = FALSE) {
  evts <- list(); actes <- list(); ngaps <- list()
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
        ), extra_cols(a, extra$actes_ccam %||% extra$actes))
      }
      for (a in pick_list(ev, "actesNGAP/acteNGAP")) {
        nabm <- unlist(pick_list(a, "NABMs/code"))
        ngaps[[length(ngaps) + 1L]] <- c(list(
          evt_id = evt_id, acte_id = pick(a, "identifiant/emetteur"),
          lettre_cle = g(a, "lettreCle"), coefficient = g(a, "coefficient"),
          quantite = g(a, "quantite"), denombrement = g(a, "denombrement"),
          date_execution = pick(a, "execute/date"), heure_execution = pick(a, "execute/heure"),
          medecin = pick(a, "prestataire/medecins/medecin/identification/code"),
          uf = pick(a, "prestataire/uniteFonctionnelle/code"),
          nabm = if (length(nabm)) list(nabm) else list(NULL),
          montant_total = pick(a, "montant/montantTotal"),
          numero_forfait_technique = pick(a, "montant/numeroForfaitTechnique"),
          numero_agrement_appareil = pick(a, "montant/numeroAgrementAppareil"),
          action = g(a, "action"), facturable = g(a, "facturable"), valide = g(a, "valide")
        ), extra_cols(a, extra$actes_ngap))
      }
    }
  }
  list(evenements = frame(evts, EVT_COLS_SA), actes_ccam = frame(actes, ACTE_SA_COLS),
       actes_ngap = frame(ngaps, ACTE_NGAP_COLS))
}
