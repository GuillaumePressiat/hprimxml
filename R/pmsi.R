#' @include common.R write.R xsd.R
NULL

DIAG_SLOTS <- c(DP = "diagnosticPrincipal", DR = "diagnosticRelie")
DIAG_COLONNES <- c(dp = "DP", dr = "DR", das = "DAS", dad = "DAD")

codes_cellule <- function(v) {
  if (is.null(v) || (is.atomic(v) && length(v) == 1 && is.na(v))) return(character())
  if (is.list(v)) v <- unlist(v)
  if (is.character(v) && length(v) == 1) v <- strsplit(v, ",", fixed = TRUE)[[1]]
  v <- trimws(as.character(v))
  v[!is.na(v) & nzchar(v)]
}

# Diagnostics au format long à partir des colonnes dp, dr, das, dad de `rum`
diagnostics_depuis_rum <- function(rum_rows) {
  out <- list()
  for (r in rum_rows) {
    for (c in intersect(names(DIAG_COLONNES), names(r))) {
      codes <- codes_cellule(r[[c]])
      if (c %in% c("dp", "dr") && length(codes) > 1) {
        stop(sprintf("RUM %s : un seul %s attendu, %d re\u00e7us (%s)", r$rum_id, DIAG_COLONNES[[c]],
                     length(codes), paste(codes, collapse = ", ")), call. = FALSE)
      }
      for (code in codes) {
        out[[length(out) + 1L]] <- list(evt_id = r$evt_id, rum_id = r$rum_id,
                                         type = DIAG_COLONNES[[c]], code = code)
      }
    }
  }
  out
}

diagnostics_node <- function(diags) {
  out <- named_list()
  das <- list()
  dad <- list()
  for (d in diags) {
    t <- toupper(as.character(d$type))
    node <- apply_paths(list(codeCim10 = gsub(".", "", trimws(as.character(d$code)), fixed = TRUE)), d)
    if (t %in% names(DIAG_SLOTS)) {
      if (!is.null(out[[DIAG_SLOTS[[t]]]])) {
        stop(sprintf("RUM %s : plusieurs %s", d$rum_id, t), call. = FALSE)
      }
      out[[DIAG_SLOTS[[t]]]] <- node
    } else if (t == "DAS") {
      das[[length(das) + 1L]] <- node
    } else if (t == "DAD") {
      dad[[length(dad) + 1L]] <- node
    } else {
      stop(sprintf("Type de diagnostic inconnu : '%s' (DP, DR, DAS, DAD)", t), call. = FALSE)
    }
  }
  if (length(das)) out$diagnosticsSignificatifs <- list(diagnosticSignificatif = das)
  if (length(dad)) out$diagnosticsDocumentaires <- list(diagnosticDocumentaire = dad)
  out
}

acte_rum_node <- function(a) {
  modifs <- g(a, "modificateurs")
  node <- list(
    externe = g(a, "externe"),
    CCAM = list(
      remboursementExceptionnel = g(a, "remboursement_exceptionnel"),
      codeActe = a$code,
      codeActeExtensionPMSI = g(a, "extension_pmsi"),
      codePhase = a$phase,
      codeActivite = a$activite,
      quantite = g(a, "quantite", 1L),
      dateRealisation = g(a, "date_realisation"),
      codeExtensionDocumentaire = g(a, "extension_documentaire"),
      modificateurs = if (is.null(modifs)) NULL else list(modificateur = strsplit(as.character(modifs), "")[[1]]),
      codeAssociationNonPrevue = g(a, "association_non_prevue")
    )
  )
  apply_paths(node, a)
}

rum_node <- function(r, diags, actes) {
  sortie <- NULL
  if (!is.null(g(r, "um_date_sortie"))) {
    sortie <- list(mode = g(r, "um_mode_sortie"), complementMode = g(r, "um_destination"),
                   date = r$um_date_sortie, heure = g(r, "um_heure_sortie"))
  }
  node <- list(
    action = g(r, "action", "creation"),
    dateAction = g(r, "date_action", r$um_date_entree),
    identifiant = list(emetteur = g(r, "rum_emetteur", r$rum_id), recepteur = g(r, "rum_recepteur")),
    uniteMedicale = list(
      code = r$um,
      entree = list(mode = r$um_mode_entree, complementMode = g(r, "um_provenance"),
                    date = r$um_date_entree, heure = g(r, "um_heure_entree")),
      sortie = sortie
    ),
    diagnostics = diagnostics_node(diags),           # obligatoire dans le XSD, même vide
    actes = if (length(actes)) list(acte = lapply(actes, acte_rum_node)) else NULL,
    igs2 = g(r, "igs2"),
    seance = g(r, "nb_seances")
  )
  apply_paths(node, r)
}

evenement_pmsi_tree <- function(evt, rums, diags, actes, entete, version) {
  rum_nodes <- lapply(rums, function(r) {
    k <- as.character(r$rum_id)
    rum_node(r, diags[[k]] %||% list(), actes[[k]] %||% list())
  })
  evenement <- list(
    patient = patient_tree(evt),
    venue = venue_tree(evt),
    rss = list(
      version = g(evt, "rss_version"),
      identifiantRSS = list(emetteur = g(evt, "rss_emetteur"), recepteur = g(evt, "rss_recepteur")),
      rum = rum_nodes
    ),
    numeroFiness = g(evt, "numero_finess")
  )
  list(evenementsPMSI = c(
    root_attrs(entete, evt, version),
    list(enteteMessage = entete_tree(entete, evt),
         evenementPMSI = apply_paths(evenement, evt))
  ))
}

par_cle <- function(rows, key) {
  out <- list()
  for (r in rows) {
    k <- as.character(r[[key]])
    out[[k]] <- c(out[[k]], list(r))
  }
  out
}

#' Arbres (format liste) des messages evenementsPMSI, un par événement ou par RUM
#'
#' @inheritParams write_evenements_pmsi
#' @return Liste d'éléments `list(evt = ligne, tree = arbre)`.
#' @examples
#' evenements <- data.frame(evt_id = "S1", ipp = "123", sexe = "F", date_naissance = as.Date("1960-01-01"),
#'                          nda = "240000001", venue_date_entree = as.Date("2024-05-02"))
#' rum <- data.frame(evt_id = "S1", rum_id = 1L, um = "1201", um_date_entree = as.Date("2024-05-02"),
#'                   um_mode_entree = "8", dp = "I21.0")
#' arbres <- iter_evenements_pmsi(evenements, rum, entete = list(emetteur = "DIM", destinataire = "DPI"))
#' str(arbres[[1]]$tree$evenementsPMSI$evenementPMSI$rss$rum[[1]], max.level = 1)
#' cat(hprim_to_xml(arbres[[1]]$tree, xmlns = NULL))
#' @export
iter_evenements_pmsi <- function(evenements, rum, diagnostics = NULL, actes = NULL, entete = NULL,
                                 version = NULL, colmap = NULL, par_rum = FALSE) {
  evt <- rows_of(as_table(evenements, "evenements",
                          c("evt_id", "ipp", "nda", "sexe", "date_naissance", "venue_date_entree"), colmap))
  rum_rows <- rows_of(as_table(rum, "rum", c("evt_id", "rum_id", "um", "um_date_entree", "um_mode_entree"),
                               colmap))
  dia <- rows_of(as_table(diagnostics, "diagnostics", c("evt_id", "rum_id", "type", "code"), colmap))
  act <- rows_of(as_table(actes, "actes", c("evt_id", "rum_id", "code", "phase", "activite"), colmap))
  dia <- c(dia, diagnostics_depuis_rum(rum_rows))

  rum_by <- rows_by(rum_rows, "evt_id")
  dia_by <- rows_by(dia, "evt_id")
  act_by <- rows_by(act, "evt_id")
  out <- list()
  for (e in evt) {
    k <- as.character(e$evt_id)
    if (is.null(rum_by[[k]])) stop(sprintf("\u00c9v\u00e9nement %s : aucun RUM", k), call. = FALSE)
    diags <- par_cle(dia_by[[k]] %||% list(), "rum_id")
    acts <- par_cle(act_by[[k]] %||% list(), "rum_id")
    if (!par_rum) {
      out[[length(out) + 1L]] <- list(evt = e, tree = evenement_pmsi_tree(e, rum_by[[k]], diags, acts,
                                                                          entete, version))
      next
    }
    for (r in rum_by[[k]]) {
      e_rum <- e
      e_rum$rss_id <- e$evt_id
      e_rum$evt_origine <- e$evt_id
      e_rum$rum_id <- r$rum_id
      e_rum$evt_id <- paste0(k, "_", r$rum_id)
      if (!is.null(g(r, "identifiant_message"))) e_rum$identifiant_message <- r$identifiant_message
      out[[length(out) + 1L]] <- list(evt = e_rum, tree = evenement_pmsi_tree(e_rum, list(r), diags, acts,
                                                                              entete, version))
    }
  }
  out
}

#' Écrire des messages evenementsPMSI (RSS MCO) depuis des tables
#'
#' Un fichier par séjour, ou par RUM avec `par_rum = TRUE`. **Unité atomique du PMSI : le
#' RUM**, seul porteur de l'attribut `action` et d'un identifiant. Mêmes tables, mêmes colonnes
#' et même sortie (octet pour octet) que `write_evenements_pmsi()` du package Python
#' pyhprimxml.
#'
#' Tables (data.frame ou tibble) :
#' * `evenements` : une ligne par séjour. Obligatoires : `evt_id`, `ipp`, `sexe`,
#'   `date_naissance`, `nda`, `venue_date_entree`.
#' * `rum` : une ligne par RUM. Obligatoires : `evt_id`, `rum_id`, `um`, `um_date_entree`,
#'   `um_mode_entree`. Diagnostics possibles en colonnes `dp`, `dr`, `das`, `dad` (`das` et
#'   `dad` : colonne liste ou chaîne séparée par des virgules).
#' * `diagnostics` : format long, `evt_id`, `rum_id`, `type` (DP, DR, DAS, DAD), `code`.
#' * `actes` : une ligne par acte CCAM, `evt_id`, `rum_id`, `code`, `phase`, `activite`.
#'
#' Toute colonne dont le nom contient `/` ou commence par `@` est une colonne-chemin,
#' relative au noeud que décrit la table (ex. `patient/@anonyme`, `@classant`).
#'
#' @param evenements,rum,diagnostics,actes Tables décrites ci-dessus.
#' @param entete Liste : `emetteur`, `destinataire`, et en option `version`,
#'   `mode_traitement`, `acquittement_attendu`, `commentaire`, `date_heure_production`,
#'   `emetteur_categorie`, `destinataire_categorie`. Chaque clé peut aussi être une colonne
#'   de `evenements` (prioritaire).
#' @param out_dir Dossier de sortie.
#' @param filename Gabarit (`"{evt_id}.xml"`) ou fonction ligne -> nom.
#' @param version Version du schéma ; sinon `entete$version` ou colonne `version`, sinon 2.4.
#' @param xmlns `HPRIM_NS` (défaut), `NULL` (pas de namespace) ou `""` (`xmlns=""`).
#' @param encoding Encodage des fichiers.
#' @param validate_xsd `"error"` (défaut), `"warn"` ou `FALSE`.
#' @param colmap Renommage de colonnes `list(ancien = "nouveau")`.
#' @param par_rum Un fichier par RUM ; `evt_id` vaut alors `{evt_id}_{rum_id}` et le gabarit
#'   dispose aussi de `{rss_id}` et `{rum_id}`.
#' @param identifiant_egal_nom_fichier L'identifiantMessage est le nom du fichier sans
#'   extension (défaut) ; une colonne `identifiant_message` contradictoire est une erreur.
#' @param fichier_ok Dépose un `.ok` vide une fois chaque `.xml` écrit intégralement.
#' @return Les chemins écrits (invisible).
#' @examples
#' # Les exemples écrivent dans un dossier temporaire
#' ancien <- setwd(tempdir())
#' @example inst/exemples/exemple_sejour.R
#' @example inst/exemples/exemple_diagnostics.R
#' @examples
#' list.files("sortie", recursive = TRUE)
#' rum_relu
#' setwd(ancien)
#' @export
write_evenements_pmsi <- function(evenements, rum, diagnostics = NULL, actes = NULL, entete = NULL,
                                  out_dir, filename = "{evt_id}.xml", version = NULL, xmlns = HPRIM_NS,
                                  encoding = "UTF-8", validate_xsd = "error", colmap = NULL,
                                  par_rum = FALSE, identifiant_egal_nom_fichier = TRUE,
                                  fichier_ok = FALSE) {
  trees <- iter_evenements_pmsi(evenements, rum, diagnostics, actes, entete, version, colmap, par_rum)
  write_trees(trees, out_dir, filename, xmlns, encoding, validate_xsd, entete,
              identifiant_egal_nom_fichier, fichier_ok)
}

# ---- lecture ----------------------------------------------------------------

EVT_COLS_PMSI <- c("evt_id", ENTETE_KEYS, "ipp", "ipp_recepteur", "sexe", "nom_usuel", "nom_naissance",
                   "prenom", "date_naissance", "nda", "nda_recepteur", "venue_date_entree",
                   "venue_heure_entree", "venue_mode_entree", "venue_uf_responsable",
                   "venue_date_sortie", "venue_heure_sortie", "rss_emetteur", "rss_recepteur",
                   "rss_version", "numero_finess")
RUM_COLS <- c("evt_id", "rum_id", "rum_emetteur", "rum_recepteur", "action", "date_action", "um",
              "um_date_entree", "um_heure_entree", "um_mode_entree", "um_provenance",
              "um_date_sortie", "um_heure_sortie", "um_mode_sortie", "um_destination", "igs2",
              "nb_seances")
DIAG_COLS <- c("evt_id", "rum_id", "type", "code")
ACTE_COLS <- c("evt_id", "rum_id", "code", "phase", "activite", "quantite", "extension_pmsi",
               "date_realisation", "extension_documentaire", "modificateurs", "association_non_prevue",
               "remboursement_exceptionnel", "externe")
DIAG_READ <- c(DP = "diagnostics/diagnosticPrincipal", DR = "diagnostics/diagnosticRelie",
               DAS = "diagnostics/diagnosticsSignificatifs/diagnosticSignificatif",
               DAD = "diagnostics/diagnosticsDocumentaires/diagnosticDocumentaire")

#' Lire des messages evenementsPMSI (RSS MCO) vers des tables
#'
#' Inverse de [write_evenements_pmsi()] : mêmes tables, mêmes colonnes. `evt_id` est le nom
#' du fichier sans extension ; `rum_id` le rang du RUM dans le RSS.
#'
#' @param sources Fichier(s), dossier, motif glob ou vecteur raw.
#' @param extra Colonnes-chemins supplémentaires, par table :
#'   `list(evenements = "patient/@anonyme", rum = "@classant")`.
#' @param version Forcer une version de schéma.
#' @param exiger_ok Ne lire que les `.xml` accompagnés de leur `.ok`.
#' @param format_diagnostics `"long"` (table `diagnostics`) ou `"colonnes"` (`dp`, `dr`, `das`,
#'   `dad` dans `rum`, la table `diagnostics` étant alors vide).
#' @return Liste de tibbles `evenements`, `rum`, `diagnostics`, `actes`.
#' @examples
#' f <- system.file("extdata", "pmsi_00001.xml", package = "hprimxml")
#' t <- read_evenements_pmsi(f)
#' t$rum[, c("evt_id", "rum_id", "rum_emetteur", "um", "um_date_entree")]
#' t$diagnostics
#'
#' # diagnostics en colonnes de `rum`, et un attribut sans colonne dédiée
#' t <- read_evenements_pmsi(f, format_diagnostics = "colonnes", extra = list(rum = "@classant"))
#' t$rum[, c("rum_id", "dp", "dr", "das", "@classant")]
#' @export
read_evenements_pmsi <- function(sources, extra = NULL, version = NULL, exiger_ok = FALSE,
                                 format_diagnostics = c("long", "colonnes")) {
  format_diagnostics <- match.arg(format_diagnostics)
  evts <- list(); rums <- list(); diags <- list(); actes <- list()
  for (s in iter_sources(sources, exiger_ok)) {
    root <- hprim_parse_xml(s$src, version = version)$evenementsPMSI
    head <- entete_cols(root)
    events <- as_list_of(root$evenementPMSI)
    for (i in seq_along(events)) {
      ev <- events[[i]]
      evt_id <- if (length(events) == 1) s$id else paste0(s$id, "_", i)
      rss <- ev$rss
      if (is.null(rss)) warning(sprintf("%s : seul le RSS (MCO) est lu pour l'instant", evt_id), call. = FALSE)
      evts[[length(evts) + 1L]] <- c(list(evt_id = evt_id), head, patient_cols(ev), venue_cols(ev), list(
        rss_emetteur = pick(rss, "identifiantRSS/emetteur"),
        rss_recepteur = pick(rss, "identifiantRSS/recepteur"),
        rss_version = pick(rss, "@version"),
        numero_finess = g(ev, "numeroFiness")
      ), extra_cols(ev, extra$evenements))
      rl <- as_list_of(rss$rum)
      for (n in seq_along(rl)) {
        r <- rl[[n]]
        rums[[length(rums) + 1L]] <- c(list(
          evt_id = evt_id, rum_id = n,
          rum_emetteur = pick(r, "identifiant/emetteur"), rum_recepteur = pick(r, "identifiant/recepteur"),
          action = g(r, "action"), date_action = g(r, "dateAction"),
          um = pick(r, "uniteMedicale/code"),
          um_date_entree = pick(r, "uniteMedicale/entree/date"),
          um_heure_entree = pick(r, "uniteMedicale/entree/heure"),
          um_mode_entree = pick(r, "uniteMedicale/entree/@mode"),
          um_provenance = pick(r, "uniteMedicale/entree/@complementMode"),
          um_date_sortie = pick(r, "uniteMedicale/sortie/date"),
          um_heure_sortie = pick(r, "uniteMedicale/sortie/heure"),
          um_mode_sortie = pick(r, "uniteMedicale/sortie/@mode"),
          um_destination = pick(r, "uniteMedicale/sortie/@complementMode"),
          igs2 = g(r, "igs2"), nb_seances = g(r, "seance")
        ), extra_cols(r, extra$rum))
        for (t in names(DIAG_READ)) {
          for (d in pick_list(r, DIAG_READ[[t]])) {
            diags[[length(diags) + 1L]] <- c(list(evt_id = evt_id, rum_id = n, type = t,
                                                  code = pick(d, "codeCim10")),
                                             extra_cols(d, extra$diagnostics))
          }
        }
        for (a in pick_list(r, "actes/acte")) {
          cc <- a$CCAM
          if (is.null(cc)) next
          actes[[length(actes) + 1L]] <- c(list(
            evt_id = evt_id, rum_id = n,
            code = g(cc, "codeActe"), phase = g(cc, "codePhase"), activite = g(cc, "codeActivite"),
            quantite = g(cc, "quantite"), extension_pmsi = g(cc, "codeActeExtensionPMSI"),
            date_realisation = g(cc, "dateRealisation"),
            extension_documentaire = g(cc, "codeExtensionDocumentaire"),
            modificateurs = modificateurs_str(cc),
            association_non_prevue = g(cc, "codeAssociationNonPrevue"),
            remboursement_exceptionnel = g(cc, "remboursementExceptionnel"),
            externe = g(a, "externe")
          ), extra_cols(a, extra$actes))
        }
      }
    }
  }
  out <- list(evenements = frame(evts, EVT_COLS_PMSI), rum = frame(rums, RUM_COLS),
              diagnostics = frame(diags, DIAG_COLS), actes = frame(actes, ACTE_COLS))
  if (format_diagnostics == "colonnes") {
    d <- out$diagnostics
    cle <- paste(out$rum$evt_id, out$rum$rum_id)
    dk <- paste(d$evt_id, d$rum_id)
    premier <- function(t) vapply(cle, function(k) { v <- d$code[dk == k & d$type == t]
                                                      if (length(v)) v[[1]] else NA_character_ }, "")
    tous <- function(t) unname(lapply(cle, function(k) d$code[dk == k & d$type == t]))
    out$rum$dp <- unname(premier("DP"))
    out$rum$dr <- unname(premier("DR"))
    out$rum$das <- tous("DAS")
    out$rum$dad <- tous("DAD")
    out$diagnostics <- out$diagnostics[0, ]
  }
  out
}
