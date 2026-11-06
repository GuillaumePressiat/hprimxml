DEFAULT_VERSION <- "2.4"

# ---- formatage des valeurs ---------------------------------------------------

fmt_hms <- function(v) {
  secs <- as.numeric(v) %% 86400
  sprintf("%02d:%02d:%02d", as.integer(secs %/% 3600), as.integer(secs %% 3600 %/% 60),
          as.integer(floor(secs %% 60)))
}

#' Formate une valeur R au format XSD attendu
#'
#' Le type XSD cible corrige les dates : un POSIXct vers `xsd:date` devient une
#' date, une Date vers `xsd:dateTime` minuit.
#' @keywords internal
format_value <- function(v, xsd_type = NULL) {
  xsd_type <- xsd_type %||% ""
  if (inherits(v, "hms") || inherits(v, "difftime")) return(fmt_hms(v))
  if (inherits(v, "POSIXt")) {
    v <- as.POSIXct(v)
    if (xsd_type == "date") return(format(v, "%Y-%m-%d"))
    if (xsd_type == "time") return(format(v, "%H:%M:%S"))
    return(format(v, "%Y-%m-%dT%H:%M:%S"))
  }
  if (inherits(v, "Date")) {
    if (xsd_type == "dateTime") return(paste0(format(v, "%Y-%m-%d"), "T00:00:00"))
    return(format(v, "%Y-%m-%d"))
  }
  if (is.logical(v)) return(if (v) "oui" else "non")
  if (is.factor(v)) return(as.character(v))
  if (is.numeric(v)) {
    if (is.integer(v) || (is.finite(v) && v == trunc(v) && abs(v) < 1e15)) return(sprintf("%.0f", v))
    return(as.character(v))
  }
  enc2utf8(as.character(v))
}

# ---- échappement (identique à libxml2 / lxml) -------------------------------

escape_text <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  gsub("\r", "&#13;", x, fixed = TRUE)
}

escape_attr <- function(x) {
  x <- escape_text(x)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("\t", "&#9;", x, fixed = TRUE)
  gsub("\n", "&#10;", x, fixed = TRUE)
}

# Caractères absents de l'encodage cible -> références numériques (&#8364;)
to_charrefs <- function(x, encoding) {
  if (toupper(encoding) %in% c("UTF-8", "UTF8")) return(x)
  max_cp <- if (toupper(encoding) %in% c("ISO-8859-1", "LATIN1", "ISO8859-1")) 255L else 127L
  cps <- utf8ToInt(x)
  if (!length(cps) || all(cps <= max_cp)) return(x)
  out <- ifelse(cps <= max_cp, vapply(cps, intToUtf8, ""), sprintf("&#%d;", cps))
  paste(out, collapse = "")
}

# ---- classification des valeurs ---------------------------------------------

is_node <- function(v) is.list(v) && (!is.null(names(v)) || length(v) == 0)
is_repeat <- function(v) {
  (is.list(v) && is.null(names(v)) && length(v) > 0) ||
    (is.atomic(v) && length(v) > 1)
}

is_empty <- function(v) {
  if (is.null(v) || length(v) == 0) return(TRUE)
  if (is.atomic(v) && length(v) == 1) {
    if (is.na(v)) return(TRUE)
    if (is.character(v) && trimws(v) == "") return(TRUE)
  }
  FALSE
}

key_of <- function(d, name) {
  nms <- names(d)
  if (name %in% nms) return(name)
  at <- paste0("@", name)
  if (at %in% nms) return(at)
  NULL
}

# ---- construction -----------------------------------------------------------

new_writer <- function(schema, ns, drop_empty, strict, encoding) {
  list(schema = schema, ns = ns, drop_empty = drop_empty, strict = strict, encoding = encoding)
}

# Renvoie un élément sous forme d'arbre léger :
# list(name, attrs = named chr, text = chr|NULL, children = list, simple = lgl)
build_el <- function(w, name, value, type_name, path, required = FALSE) {
  s <- w$schema
  complex <- is_complex(s, type_name)
  if (is_repeat(value) && !(complex && is_node(value))) {
    items <- if (is.list(value)) value else lapply(seq_along(value), function(i) value[i])
    out <- list()
    for (i in seq_along(items)) {
      el <- build_el(w, name, items[[i]], type_name, sprintf("%s[%d]", path, i - 1L), required)
      if (!is.null(el)) out[[length(out) + 1L]] <- el
    }
    return(structure(out, class = "hprim_many"))
  }
  keep <- required && complex && is_node(value)
  if (w$drop_empty && is_empty(value) && !keep) return(NULL)

  el <- list(name = name, attrs = character(), text = NULL, children = list(), simple = !complex)
  if (!complex) {
    if (is_node(value)) stop(sprintf("%s : valeur simple attendue, liste nomm\u00e9e re\u00e7ue", path), call. = FALSE)
    el$text <- if (is_empty(value)) "" else format_value(value, type_name)
    return(el)
  }
  ct <- ctype(s, type_name)
  if (!is_node(value)) {
    if (ct$text) {
      value <- list(`#text` = value)
    } else {
      stop(sprintf("%s : \u00e9l\u00e9ment complexe attendu, valeur simple re\u00e7ue (%s)", path,
                   paste(format(value), collapse = " ")), call. = FALSE)
    }
  }
  used <- character()
  for (a in ct$attrs) {
    k <- key_of(value, a)
    if (is.null(k)) next
    used <- c(used, k)
    v <- value[[k]]
    if (w$drop_empty && is_empty(v)) next
    el$attrs[[a]] <- if (is_empty(v)) "" else format_value(v)
  }
  if (ct$text && "#text" %in% names(value)) {
    used <- c(used, "#text")
    v <- value[["#text"]]
    if (!is_empty(v) || !w$drop_empty) {
      el$text <- if (is_empty(v)) "" else format_value(v, ct$text_type)
      el$simple <- TRUE
    }
  }
  for (cname in unique(ct$child_names)) {
    k <- key_of(value, cname)
    if (is.null(k)) next
    used <- c(used, k)
    child <- build_el(w, cname, value[[k]], child_type(ct, cname), paste0(path, "/", cname),
                      cname %in% ct$required)
    if (inherits(child, "hprim_many")) {
      el$children <- c(el$children, unclass(child))
    } else if (!is.null(child)) {
      el$children[[length(el$children) + 1L]] <- child
    }
  }
  unknown <- setdiff(names(value), c(used, "@xmlns", "xmlns", ""))
  if (length(unknown)) {
    msg <- sprintf("%s : cl\u00e9s inconnues du XSD %s : %s", path, s$version,
                   paste(unknown, collapse = ", "))
    if (w$strict) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }
  if (w$drop_empty && !keep && !length(el$children) && !length(el$attrs) &&
      (is.null(el$text) || trimws(el$text) == "")) {
    return(NULL)
  }
  el
}

# ---- sérialisation (format lxml : tabulations, <x/> si vide complexe) -------

render <- function(el, depth, pretty, encoding, root_ns = NULL) {
  ind <- if (pretty) strrep("\t", depth) else ""
  attrs <- ""
  if (!is.null(root_ns)) attrs <- root_ns
  if (length(el$attrs)) {
    vals <- vapply(el$attrs, function(v) to_charrefs(escape_attr(v), encoding), "")
    attrs <- paste0(attrs, paste0(" ", names(el$attrs), "=\"", vals, "\"", collapse = ""))
  }
  open <- paste0(ind, "<", el$name, attrs)
  if (!is.null(el$text) && !length(el$children)) {
    return(paste0(open, ">", to_charrefs(escape_text(el$text), encoding), "</", el$name, ">"))
  }
  if (!length(el$children)) return(paste0(open, "/>"))
  kids <- vapply(el$children, render, "", depth = depth + 1L, pretty = pretty, encoding = encoding)
  sep <- if (pretty) "\n" else ""
  paste0(open, ">", sep, paste(kids, collapse = sep), sep, ind, "</", el$name, ">")
}

#' Écrire un message HPRIM XML à partir d'une liste
#'
#' La liste suit la même convention que le dictionnaire du package Python
#' pyhprimxml (format xmltodict) :
#' * une seule clé : l'élément racine ;
#' * attributs et éléments enfants sont des clés de la même liste nommée
#'   (préfixe `@` accepté mais facultatif : le XSD tranche) ;
#' * un élément répété est une liste non nommée (ou un vecteur atomique) ;
#' * un élément à contenu simple portant des attributs utilise la clé `#text`.
#'
#' L'ordre des clés n'a pas d'importance : les enfants sont émis dans l'ordre du
#' XSD. La sortie est identique, octet pour octet, à celle de `to_xml()` en Python.
#'
#' @param tree Liste `list(racine = list(...))`.
#' @param version Version du schéma ; par défaut l'attribut `version` du message,
#'   sinon 2.4. Si renseignée, elle remplace l'attribut.
#' @param xmlns `HPRIM_NS` (défaut) ; `NULL` : aucune déclaration ; `""` : écrit
#'   `xmlns=""` littéralement sur la racine.
#' @param encoding Encodage déclaré (`"UTF-8"` ou `"ISO-8859-1"`) ; les caractères
#'   hors encodage deviennent des références numériques.
#' @param pretty Indentation par tabulations.
#' @param drop_empty Supprime éléments et attributs vides (`NULL`, `NA`, `""`),
#'   sauf les éléments complexes obligatoires du XSD.
#' @param strict Erreur si une clé ne correspond à rien dans le XSD (sinon
#'   avertissement).
#' @param validate_xsd `FALSE`, `"warn"` ou `"error"` (`TRUE` équivaut à `"error"`).
#' @return Le message XML (chaîne UTF-8), déclaration comprise.
#' @examples
#' f <- system.file("extdata", "pmsi_00001.xml", package = "hprimxml")
#' msg <- hprim_parse_xml(f)
#' cat(substr(hprim_to_xml(msg, xmlns = NULL), 1, 200))
#' @export
hprim_to_xml <- function(tree, version = NULL, xmlns = HPRIM_NS, encoding = "UTF-8",
                         pretty = TRUE, drop_empty = TRUE, strict = TRUE,
                         validate_xsd = FALSE) {
  if (!is.list(tree) || length(tree) != 1 || is.null(names(tree))) {
    stop("La liste doit avoir une seule cl\u00e9 : l'\u00e9l\u00e9ment racine", call. = FALSE)
  }
  root_name <- names(tree)
  content <- tree[[1]]
  content[c("@xmlns", "xmlns")] <- NULL
  vkey <- key_of(content, "version")
  if (is.null(version)) version <- if (!is.null(vkey)) format_value(content[[vkey]]) else DEFAULT_VERSION
  if (!is.null(vkey)) content[[vkey]] <- NULL
  content[["version"]] <- version

  schema <- hprim_schema(version, root_name)
  w <- new_writer(schema, xmlns, drop_empty, strict, encoding)
  el <- build_el(w, root_name, content, root_type(schema), root_name)

  root_ns <- if (is.null(xmlns)) NULL else sprintf(" xmlns=\"%s\"", xmlns)
  body <- render(el, 0L, pretty, encoding, root_ns)
  out <- paste0('<?xml version="1.0" encoding="', encoding, '" standalone="yes"?>\n', body)

  if (!isFALSE(validate_xsd)) {
    errors <- hprim_validate(out)
    if (length(errors)) {
      msg <- paste0("XML non conforme au XSD :\n", paste(utils::head(errors, 20), collapse = "\n"))
      if (identical(validate_xsd, "warn")) warning(msg, call. = FALSE) else stop(msg, call. = FALSE)
    }
  }
  out
}

#' Écrire un message HPRIM XML dans un fichier
#'
#' @param tree Liste `list(racine = list(...))`.
#' @param path Fichier de sortie.
#' @param encoding Encodage du fichier.
#' @param ... Autres options de [hprim_to_xml()].
#' @return Le chemin écrit (invisible).
#' @export
hprim_write_xml <- function(tree, path, encoding = "UTF-8", ...) {
  xml <- hprim_to_xml(tree, encoding = encoding, ...)
  bytes <- if (toupper(encoding) %in% c("UTF-8", "UTF8")) {
    charToRaw(enc2utf8(xml))
  } else {
    iconv(xml, "UTF-8", encoding, toRaw = TRUE)[[1]]
  }
  writeBin(bytes, path)
  invisible(path)
}
