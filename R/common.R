#' @include write.R xsd.R
NULL

# Briques partagées par les couches tabulaires (PMSI, serveur d'actes, états patient).
#
# Colonnes-chemins : toute colonne dont le nom contient "/" ou commence par "@" est un
# chemin XML relatif au noeud que décrit la table ; "@nom" en dernier segment désigne un
# attribut (ex. "patient/@anonyme", "medecinResponsable/identification/code").

ENTETE_KEYS <- c("version", "acquittement_attendu", "mode_traitement", "identifiant_message",
                 "date_heure_production", "emetteur", "emetteur_categorie", "destinataire",
                 "destinataire_categorie", "commentaire")

named_list <- function() structure(list(), names = character(0))

# ---- tables -----------------------------------------------------------------

as_table <- function(df, name, required, colmap = NULL) {
  if (is.null(df)) {
    df <- as.data.frame(stats::setNames(replicate(length(required), character(), simplify = FALSE),
                                        required), stringsAsFactors = FALSE)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, optional = TRUE)
  if (!is.null(colmap)) {
    i <- match(names(colmap), names(df))
    names(df)[i[!is.na(i)]] <- unname(unlist(colmap))[!is.na(i)]
  }
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop(sprintf("Table %s : colonnes obligatoires manquantes : %s", name,
                 paste(missing, collapse = ", ")), call. = FALSE)
  }
  df
}

# Lignes d'une table : liste de listes nommées, classes conservées (Date, POSIXct, hms)
rows_of <- function(df) {
  n <- nrow(df)
  if (!n) return(list())
  lapply(seq_len(n), function(i) {
    lapply(df, function(col) if (is.list(col)) col[[i]] else col[i])
  })
}

rows_by <- function(rows, key) {
  out <- list()
  for (r in rows) {
    k <- as.character(r[[key]])
    out[[k]] <- c(out[[k]], list(r))
  }
  out
}

# Valeur d'une cellule : NULL si absente ou manquante
g <- function(row, col, default = NULL) {
  v <- row[[col]]
  if (is.null(v) || (is.atomic(v) && length(v) == 1 && is.na(v))) default else v
}

# ---- colonnes-chemins -------------------------------------------------------

is_path <- function(col) grepl("/", col, fixed = TRUE) | startsWith(col, "@")

set_path <- function(node, parts, value) {
  key <- parts[[1]]
  if (length(parts) == 1) {
    node[[sub("^@", "", key)]] <- value
    return(node)
  }
  child <- node[[key]]
  if (is.null(child)) child <- named_list()
  else if (!is_node(child)) child <- list(`#text` = child)
  node[[key]] <- set_path(child, parts[-1], value)
  node
}

apply_paths <- function(node, row) {
  for (col in names(row)[is_path(names(row))]) {
    v <- g(row, col)
    if (is.null(v)) next
    parts <- strsplit(col, "/", fixed = TRUE)[[1]]
    node <- set_path(node, parts[nzchar(parts)], v)
  }
  node
}

# ---- petits constructeurs ---------------------------------------------------

date_heure <- function(row, date_col, heure_col) {
  d <- g(row, date_col)
  if (is.null(d)) NULL else list(date = d, heure = g(row, heure_col))
}

code_node <- function(v) if (is.null(v)) NULL else list(code = v)

ent <- function(evt, entete, key, default = NULL) {
  v <- g(evt, key)
  if (is.null(v)) v <- entete[[key]]
  if (is.null(v) || (length(v) == 1 && is.na(v))) default else v
}

check_identifiant_message <- function(v, evt_id, fichier = NULL) {
  v <- as.character(v)
  if (!grepl("^[A-Za-z0-9,_-]{1,12}$", v)) {
    if (!is.null(fichier)) {
      stop(sprintf(paste0("\u00c9v\u00e9nement %s : le nom de fichier '%s' ne peut pas servir ",
                          "d'identifiantMessage ('%s') : 12 caract\u00e8res au plus (sans l'extension), ",
                          "parmi lettres, chiffres, - , _. Raccourcir evt_id ou le gabarit filename."),
                   evt_id, fichier, v), call. = FALSE)
    }
    stop(sprintf(paste0("\u00c9v\u00e9nement %s : identifiantMessage '%s' invalide (nom12 : 12 ",
                        "caract\u00e8res max, lettres, chiffres, - , _)."), evt_id, v), call. = FALSE)
  }
  v
}

# ---- blocs communs ----------------------------------------------------------

entete_tree <- function(entete, evt) {
  agent <- function(code, categorie) list(agents = list(agent = list(categorie = categorie, code = code)))
  list(
    modeTraitement = ent(evt, entete, "mode_traitement"),
    identifiantMessage = as.character(ent(evt, entete, "identifiant_message", evt$evt_id)),
    dateHeureProduction = ent(evt, entete, "date_heure_production", Sys.time()),
    emetteur = agent(ent(evt, entete, "emetteur"), ent(evt, entete, "emetteur_categorie", "application")),
    destinataire = agent(ent(evt, entete, "destinataire"),
                         ent(evt, entete, "destinataire_categorie", "application")),
    commentaireMessage = ent(evt, entete, "commentaire")
  )
}

patient_tree <- function(e) {
  rec <- g(e, "ipp_recepteur")
  list(
    identifiant = list(emetteur = list(valeur = e$ipp),
                       recepteur = if (is.null(rec)) NULL else list(valeur = rec)),
    personnePhysique = list(
      sexe = e$sexe,
      nomUsuel = g(e, "nom_usuel"),
      nomNaissance = g(e, "nom_naissance"),
      prenoms = list(prenom = g(e, "prenom")),
      dateNaissance = list(date = e$date_naissance)
    )
  )
}

venue_tree <- function(e) {
  if (is.null(g(e, "nda"))) return(NULL)
  rec <- g(e, "nda_recepteur")
  sortie <- date_heure(e, "venue_date_sortie", "venue_heure_sortie")
  list(
    identifiant = list(emetteur = list(valeur = e$nda),
                       recepteur = if (is.null(rec)) NULL else list(valeur = rec)),
    entree = list(
      dateHeureOptionnelle = date_heure(e, "venue_date_entree", "venue_heure_entree"),
      modeEntree = code_node(g(e, "venue_mode_entree")),
      uniteFonctionnelleResponsable = code_node(g(e, "venue_uf_responsable"))
    ),
    sortie = if (is.null(sortie)) NULL else list(dateHeureOptionnelle = sortie)
  )
}

intervention_tree <- function(e) {
  if (is.null(g(e, "intervention_id"))) return(NULL)
  demande <- NULL
  if (!is.null(g(e, "demande_date"))) {
    demande <- list(datePrescription = date_heure(e, "demande_date", "demande_heure"),
                    uniteFonctionnelle = code_node(g(e, "demande_uf")))
  }
  list(
    identifiant = list(emetteur = e$intervention_id),
    debut = date_heure(e, "intervention_date_debut", "intervention_heure_debut"),
    fin = date_heure(e, "intervention_date_fin", "intervention_heure_fin"),
    uniteFonctionnelle = code_node(g(e, "intervention_uf")),
    demande = demande
  )
}

root_attrs <- function(entete, evt, version) {
  list(version = version %||% ent(evt, entete, "version", "2.4"),
       acquittementAttendu = ent(evt, entete, "acquittement_attendu"))
}

# ---- atomicité ---------------------------------------------------------------

# Un message par objet atomique (acte, diagnostic) : la ligne d'événement est répétée, avec
# evt_id = {evt_id}_{rang}, evt_origine (l'evt_id de départ) et rang. Une colonne
# identifiant_message de l'objet prime sur celle de l'événement.
decouper <- function(e, objets, extra = NULL) {
  if (!length(objets)) {
    warning(sprintf(paste0("\u00c9v\u00e9nement %s : aucun objet \u00e0 \u00e9crire en mode ",
                           "atomique, ignor\u00e9"), e$evt_id), call. = FALSE)
  }
  lapply(seq_along(objets), function(rang) {
    o <- objets[[rang]]
    e_o <- e
    e_o$evt_origine <- e$evt_id
    e_o$rang <- rang
    e_o$evt_id <- paste0(e$evt_id, "_", rang)
    if (!is.null(extra)) e_o <- utils::modifyList(e_o, extra(o))
    if (!is.null(g(o, "identifiant_message"))) e_o$identifiant_message <- o$identifiant_message
    list(evt = e_o, objet = o)
  })
}

# ---- écriture ---------------------------------------------------------------

fill_template <- function(tpl, row) {
  if (is.function(tpl)) return(tpl(row))
  m <- gregexpr("\\{([A-Za-z0-9_]+)\\}", tpl)
  keys <- regmatches(tpl, m)[[1]]
  for (k in keys) {
    col <- substr(k, 2, nchar(k) - 1)
    if (is.null(row[[col]])) stop(sprintf("Gabarit filename : colonne inconnue '%s'", col), call. = FALSE)
    tpl <- sub(k, as.character(row[[col]]), tpl, fixed = TRUE)
  }
  tpl
}

chemin_ok <- function(path, extension_ok = ".ok") paste0(tools::file_path_sans_ext(path), extension_ok)

# Dépôt atomique : .ok éventuel supprimé, contenu écrit sous un nom temporaire caché puis
# renommé, .ok vide créé en dernier.
deposer <- function(path, xml, encoding, fichier_ok = FALSE) {
  ok <- chemin_ok(path)
  if (fichier_ok && file.exists(ok)) file.remove(ok)
  tmp <- file.path(dirname(path), paste0(".", basename(path), ".tmp"))
  on.exit(if (file.exists(tmp)) file.remove(tmp), add = TRUE)
  bytes <- if (toupper(encoding) %in% c("UTF-8", "UTF8")) charToRaw(enc2utf8(xml)) else
    iconv(xml, "UTF-8", encoding, toRaw = TRUE)[[1]]
  writeBin(bytes, tmp)
  if (!file.rename(tmp, path)) stop(sprintf("Impossible de d\u00e9poser %s", path), call. = FALSE)
  if (fichier_ok) file.create(ok)
  invisible(path)
}

write_trees <- function(trees, out_dir, filename, xmlns, encoding, validate_xsd, entete,
                        identifiant_egal_nom_fichier, fichier_ok) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character()
  vus <- character()
  for (item in trees) {
    e <- item$evt
    tree <- item$tree
    name <- fill_template(filename, e)
    if (name %in% names(vus)) {
      stop(sprintf(paste0("Nom de fichier '%s' produit par deux \u00e9v\u00e9nements (%s et %s) : ",
                          "le gabarit filename doit donner un nom unique par message."),
                   name, vus[[name]], e$evt_id), call. = FALSE)
    }
    vus[[name]] <- as.character(e$evt_id)
    root <- names(tree)
    head <- tree[[root]]$enteteMessage
    if (identifiant_egal_nom_fichier) {
      stem <- tools::file_path_sans_ext(basename(name))
      explicite <- ent(e, entete, "identifiant_message")
      if (!is.null(explicite) && as.character(explicite) != stem) {
        stop(sprintf(paste0("\u00c9v\u00e9nement %s : identifiant_message '%s' diff\u00e9rent du nom ",
                            "de fichier '%s'. L'identifiant du message doit \u00eatre le nom du fichier ",
                            "sans extension : retirer identifiant_message, ou s'en servir pour nommer ",
                            "le fichier (filename = \"{identifiant_message}.xml\")."),
                     e$evt_id, explicite, name), call. = FALSE)
      }
      head$identifiantMessage <- check_identifiant_message(stem, e$evt_id, name)
    } else {
      head$identifiantMessage <- check_identifiant_message(head$identifiantMessage, e$evt_id)
    }
    tree[[root]]$enteteMessage <- head
    xml <- tryCatch(
      hprim_to_xml(tree, xmlns = xmlns, encoding = encoding, validate_xsd = validate_xsd),
      error = function(err) stop(sprintf("\u00c9v\u00e9nement %s : %s", e$evt_id, conditionMessage(err)),
                                 call. = FALSE)
    )
    path <- file.path(out_dir, name)
    deposer(path, xml, encoding, fichier_ok)
    paths <- c(paths, path)
  }
  invisible(paths)
}

# ---- lecture ----------------------------------------------------------------

pick <- function(node, path) {
  cur <- node
  for (p in strsplit(path, "/", fixed = TRUE)[[1]]) {
    if (!nzchar(p)) next
    if (is_repeat(cur) && is.list(cur)) cur <- cur[[1]]
    if (!is_node(cur)) return(NULL)
    cur <- cur[[sub("^@", "", p)]]
  }
  if (is.list(cur) && is.null(names(cur)) && length(cur)) cur <- cur[[1]]
  if (is_node(cur)) cur <- cur[["#text"]]
  if (is.atomic(cur) && length(cur) == 1 && is.na(cur)) return(NULL)
  cur
}

pick_list <- function(node, path) {
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- parts[nzchar(parts)]
  cur <- node
  for (p in utils::head(parts, -1)) {
    if (is.list(cur) && is.null(names(cur)) && length(cur)) cur <- cur[[1]]
    if (!is_node(cur)) return(list())
    cur <- cur[[p]]
  }
  if (is.list(cur) && is.null(names(cur)) && length(cur)) cur <- cur[[1]]
  if (!is_node(cur)) return(list())
  v <- cur[[utils::tail(parts, 1)]]
  if (is.null(v)) list() else if (is.list(v) && is.null(names(v))) v else list(v)
}

as_list_of <- function(v) if (is.null(v)) list() else if (is.list(v) && is.null(names(v))) v else list(v)

extra_cols <- function(node, paths) {
  stats::setNames(lapply(paths, function(p) pick(node, p)), paths)
}

modificateurs_str <- function(node) {
  mods <- vapply(pick_list(node, "modificateurs/modificateur"), function(m) {
    if (is_node(m)) as.character(m[["#text"]]) else as.character(m)
  }, "")
  if (length(mods)) paste(mods, collapse = "") else NULL
}

entete_cols <- function(root) {
  e <- root$enteteMessage
  list(
    version = g(root, "version"),
    acquittement_attendu = g(root, "acquittementAttendu"),
    mode_traitement = g(e, "modeTraitement"),
    identifiant_message = g(e, "identifiantMessage"),
    date_heure_production = g(e, "dateHeureProduction"),
    emetteur = pick(e, "emetteur/agents/agent/code"),
    emetteur_categorie = pick(e, "emetteur/agents/agent/@categorie"),
    destinataire = pick(e, "destinataire/agents/agent/code"),
    destinataire_categorie = pick(e, "destinataire/agents/agent/@categorie"),
    commentaire = g(e, "commentaireMessage")
  )
}

patient_cols <- function(ev) {
  p <- ev$patient
  list(
    ipp = pick(p, "identifiant/emetteur/valeur"),
    ipp_recepteur = pick(p, "identifiant/recepteur/valeur"),
    sexe = pick(p, "personnePhysique/@sexe"),
    nom_usuel = pick(p, "personnePhysique/nomUsuel"),
    nom_naissance = pick(p, "personnePhysique/nomNaissance"),
    prenom = pick(p, "personnePhysique/prenoms/prenom"),
    date_naissance = pick(p, "personnePhysique/dateNaissance/date")
  )
}

venue_cols <- function(ev) {
  v <- ev$venue
  list(
    nda = pick(v, "identifiant/emetteur/valeur"),
    nda_recepteur = pick(v, "identifiant/recepteur/valeur"),
    venue_date_entree = pick(v, "entree/dateHeureOptionnelle/date"),
    venue_heure_entree = pick(v, "entree/dateHeureOptionnelle/heure"),
    venue_mode_entree = pick(v, "entree/modeEntree/code"),
    venue_uf_responsable = pick(v, "entree/uniteFonctionnelleResponsable/code"),
    venue_date_sortie = pick(v, "sortie/dateHeureOptionnelle/date"),
    venue_heure_sortie = pick(v, "sortie/dateHeureOptionnelle/heure")
  )
}

intervention_cols <- function(ev) {
  i <- ev$intervention
  list(
    intervention_id = pick(i, "identifiant/emetteur"),
    intervention_date_debut = pick(i, "debut/date"),
    intervention_heure_debut = pick(i, "debut/heure"),
    intervention_date_fin = pick(i, "fin/date"),
    intervention_heure_fin = pick(i, "fin/heure"),
    intervention_uf = pick(i, "uniteFonctionnelle/code"),
    demande_date = pick(i, "demande/datePrescription/date"),
    demande_heure = pick(i, "demande/datePrescription/heure"),
    demande_uf = pick(i, "demande/uniteFonctionnelle/code")
  )
}

# Sources : fichier, dossier (*.xml), motif glob, vecteur de chemins, vecteur raw
iter_sources <- function(sources, exiger_ok = FALSE) {
  if (is.raw(sources)) return(list(list(id = "message", src = sources)))
  fichiers <- character()
  for (s in sources) {
    if (dir.exists(s)) fichiers <- c(fichiers, sort(list.files(s, pattern = "\\.xml$", full.names = TRUE)))
    else if (grepl("[*?[]", s)) fichiers <- c(fichiers, sort(Sys.glob(s)))
    else fichiers <- c(fichiers, s)
  }
  if (exiger_ok) {
    sans_ok <- !file.exists(chemin_ok(fichiers))
    if (any(sans_ok)) {
      warning(sprintf("%d fichier(s) sans .ok ignor\u00e9(s) (d\u00e9p\u00f4t incomplet) : %s",
                      sum(sans_ok), paste(utils::head(basename(fichiers[sans_ok]), 5), collapse = ", ")),
              call. = FALSE)
    }
    fichiers <- fichiers[!sans_ok]
  }
  lapply(fichiers, function(f) list(id = tools::file_path_sans_ext(basename(f)), src = f))
}

# Assemble une colonne à partir de valeurs scalaires (NULL = manquant), types conservés
bind_column <- function(vals) {
  n <- length(vals)
  if (any(vapply(vals, function(v) length(v) > 1 || is.list(v), logical(1)))) {
    return(lapply(vals, function(v) if (is.null(v)) character() else unlist(v)))
  }
  present <- Filter(Negate(is.null), vals)
  if (!length(present)) return(rep(NA, n))
  ex <- present[[1]]
  num <- function() vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1))
  if (inherits(ex, "Date")) return(as.Date(num(), origin = "1970-01-01"))
  if (inherits(ex, "POSIXct")) return(as.POSIXct(num(), origin = "1970-01-01", tz = "UTC"))
  if (inherits(ex, "hms")) return(hms::hms(seconds = num()))
  if (is.integer(ex)) return(vapply(vals, function(v) if (is.null(v)) NA_integer_ else as.integer(v), integer(1)))
  if (is.logical(ex)) return(vapply(vals, function(v) if (is.null(v)) NA else as.logical(v), logical(1)))
  if (is.numeric(ex)) return(num())
  vapply(vals, function(v) if (is.null(v)) NA_character_ else as.character(v), character(1))
}

frame <- function(rows, columns) {
  cols <- unique(c(columns, unlist(lapply(rows, names))))
  out <- lapply(cols, function(c) bind_column(lapply(rows, function(r) r[[c]])))
  names(out) <- cols
  if (!length(rows)) out <- lapply(out, function(x) logical())
  tibble::as_tibble(out, .name_repair = "minimal")
}
