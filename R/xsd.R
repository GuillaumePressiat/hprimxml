#' Namespace HPRIM XML
#'
#' Valeur par défaut de l'argument `xmlns` des fonctions d'écriture.
#' @export
HPRIM_NS <- "http://www.hprim.org/hprimXML"

XSD_NS <- c(xsd = "http://www.w3.org/2001/XMLSchema")

.cache <- new.env(parent = emptyenv())

schema_dir <- function() system.file("schemas", package = "hprimxml", mustWork = TRUE)

#' Versions de schémas HPRIM XML embarquées
#'
#' @return Vecteur des versions disponibles (`"1.03a"` à `"2.4"`).
#' @examples
#' hprim_available_versions()
#' @export
hprim_available_versions <- function() {
  sort(list.files(schema_dir()))
}

local_name <- function(qname) {
  if (is.null(qname) || is.na(qname)) return(NA_character_)
  sub("^[^:]*:", "", qname)
}

xsd_tag <- function(node) xml2::xml_name(node)

xsd_attr <- function(node, attr, default = NA_character_) {
  v <- xml2::xml_attr(node, attr)
  if (is.na(v)) default else v
}

xsd_children <- function(node) xml2::xml_children(node)

# Fichier msg*.xsd qui déclare l'élément racine `root`
find_entry <- function(dir, root) {
  pat <- sprintf('<xsd:element\\s+name="%s"', root)
  for (f in sort(list.files(dir, pattern = "^msg.*\\.xsd$", full.names = TRUE))) {
    if (any(grepl(pat, readLines(f, warn = FALSE, encoding = "UTF-8")))) return(f)
  }
  stop(sprintf("Aucun message %s dans les sch\u00e9mas %s", root, basename(dirname(dir))), call. = FALSE)
}

#' Schéma d'un type de message pour une version
#'
#' Charge le XSD du message (avec ses `xsd:include`) et en donne une vue
#' utilisable pour l'écriture et la lecture : ordre des enfants, attributs,
#' éléments obligatoires et répétables, nature des types simples.
#' Le résultat est mis en cache.
#'
#' @param version Version HPRIM XML, par exemple `"2.4"`.
#' @param root Élément racine du message, par exemple `"evenementsPMSI"`.
#' @return Un environnement de classe `hprim_schema`.
#' @keywords internal
hprim_schema <- function(version, root) {
  key <- paste(version, root, sep = "|")
  if (!is.null(.cache[[key]])) return(.cache[[key]])
  dir <- file.path(schema_dir(), version, "ns")
  if (!dir.exists(dir)) {
    stop(sprintf("Version HPRIM inconnue : %s (disponibles : %s)", version,
                 paste(hprim_available_versions(), collapse = ", ")), call. = FALSE)
  }
  s <- new.env(parent = emptyenv())
  class(s) <- "hprim_schema"
  s$version <- version
  s$root <- root
  s$entry <- find_entry(dir, root)
  s$docs <- list()
  s$ctypes_xml <- list()
  s$stypes <- list()
  s$elements <- list()
  s$groups <- list()
  s$attr_groups <- list()
  s$cache <- list()
  s$anon <- 0L
  load_xsd(s, s$entry, character())
  .cache[[key]] <- s
  s
}

load_xsd <- function(s, path, seen) {
  path <- normalizePath(path)
  if (path %in% s$loaded) return(invisible())
  s$loaded <- c(s$loaded, path)
  doc <- xml2::read_xml(path)
  s$docs[[path]] <- doc
  for (inc in xml2::xml_find_all(doc, "/xsd:schema/xsd:include", XSD_NS)) {
    load_xsd(s, file.path(dirname(path), xml2::xml_attr(inc, "schemaLocation")), seen)
  }
  for (node in xsd_children(xml2::xml_root(doc))) {
    name <- xml2::xml_attr(node, "name")
    switch(xsd_tag(node),
      complexType = s$ctypes_xml[[name]] <- node,
      simpleType = s$stypes[[name]] <- node,
      group = s$groups[[name]] <- node,
      attributeGroup = s$attr_groups[[name]] <- node,
      element = s$elements[[name]] <- node
    )
  }
  invisible()
}

root_type <- function(s) local_name(xml2::xml_attr(s$elements[[s$root]], "type"))

is_complex <- function(s, type_name) {
  !is.null(type_name) && !is.na(type_name) && !is.null(s$ctypes_xml[[type_name]])
}

new_ctype <- function() {
  list(attrs = character(), child_names = character(), child_types = character(),
       required = character(), repeated = character(), text = FALSE, text_type = NA_character_)
}

#' Vue d'un type complexe : attributs, enfants ordonnés, obligatoires, répétables
#' @keywords internal
ctype <- function(s, type_name) {
  if (is.null(s$cache[[type_name]])) {
    s$cache[[type_name]] <- build_ctype(s, s$ctypes_xml[[type_name]])
  }
  s$cache[[type_name]]
}

child_type <- function(ct, name) {
  i <- match(name, ct$child_names)
  if (is.na(i)) NULL else ct$child_types[[i]]
}

build_ctype <- function(s, node) {
  ct <- new_ctype()
  for (child in xsd_children(node)) {
    tag <- xsd_tag(child)
    if (tag %in% c("complexContent", "simpleContent")) {
      for (der in xsd_children(child)) {
        dtag <- xsd_tag(der)
        if (!dtag %in% c("extension", "restriction")) next
        base <- local_name(xml2::xml_attr(der, "base"))
        if (is_complex(s, base) && dtag == "extension") {
          b <- ctype(s, base)
          ct$attrs <- c(ct$attrs, b$attrs)
          ct$child_names <- c(ct$child_names, b$child_names)
          ct$child_types <- c(ct$child_types, b$child_types)
          ct$required <- union(ct$required, b$required)
          ct$repeated <- union(ct$repeated, b$repeated)
          ct$text <- b$text
          ct$text_type <- b$text_type
        }
        if (tag == "simpleContent") {
          ct$text <- TRUE
          if (!is_complex(s, base)) ct$text_type <- base
        }
        ct <- collect(s, der, ct)
      }
    } else {
      ct <- collect_one(s, child, ct)
    }
  }
  ct
}

collect <- function(s, node, ct, optional = FALSE, repeated = FALSE) {
  for (child in xsd_children(node)) ct <- collect_one(s, child, ct, optional, repeated)
  ct
}

collect_one <- function(s, child, ct, optional = FALSE, repeated = FALSE) {
  tag <- xsd_tag(child)
  many <- !xsd_attr(child, "maxOccurs", "1") %in% c("0", "1")
  if (tag %in% c("sequence", "choice", "all")) {
    opt <- optional || tag == "choice" || identical(xml2::xml_attr(child, "minOccurs"), "0")
    ct <- collect(s, child, ct, opt, repeated || many)
  } else if (tag == "element") {
    ref <- xml2::xml_attr(child, "ref")
    ename <- if (!is.na(ref)) local_name(ref) else xml2::xml_attr(child, "name")
    if (!optional && xsd_attr(child, "minOccurs", "1") != "0") ct$required <- c(ct$required, ename)
    if (repeated || many) ct$repeated <- c(ct$repeated, ename)
    if (!is.na(ref)) {
      t <- local_name(xml2::xml_attr(s$elements[[ename]], "type"))
    } else {
      t <- local_name(xml2::xml_attr(child, "type"))
      inline <- xml2::xml_find_first(child, "xsd:complexType", XSD_NS)
      if (is.na(t) && !inherits(inline, "xml_missing")) {
        s$anon <- s$anon + 1L
        t <- sprintf("__anon%d_%s", s$anon, ename)
        s$ctypes_xml[[t]] <- inline
      }
    }
    ct$child_names <- c(ct$child_names, ename)
    ct$child_types <- c(ct$child_types, t)
  } else if (tag == "attribute") {
    nm <- xml2::xml_attr(child, "name")
    ct$attrs <- c(ct$attrs, if (is.na(nm)) local_name(xml2::xml_attr(child, "ref")) else nm)
  } else if (tag == "group") {
    ct <- collect(s, s$groups[[local_name(xml2::xml_attr(child, "ref"))]], ct, optional, repeated)
  } else if (tag == "attributeGroup") {
    ct <- collect(s, s$attr_groups[[local_name(xml2::xml_attr(child, "ref"))]], ct, optional, repeated)
  }
  ct
}

KINDS <- c(date = "date", dateTime = "datetime", time = "time",
           integer = "int", int = "int", long = "int", short = "int", positiveInteger = "int",
           nonNegativeInteger = "int", negativeInteger = "int", nonPositiveInteger = "int")

#' Nature d'un type simple : date, datetime, time, int ou str
#' @keywords internal
simple_kind <- function(s, type_name) {
  seen <- character()
  while (!is.null(type_name) && !is.na(type_name) && !is.null(s$stypes[[type_name]]) &&
         !type_name %in% seen) {
    seen <- c(seen, type_name)
    r <- xml2::xml_find_first(s$stypes[[type_name]], "xsd:restriction", XSD_NS)
    if (inherits(r, "xml_missing")) return("str")
    type_name <- local_name(xml2::xml_attr(r, "base"))
  }
  k <- if (is.null(type_name) || is.na(type_name)) NA else KINDS[type_name]
  if (is.na(k)) "str" else unname(k)
}

xml_schema_doc <- function(version, root, namespaced) {
  key <- paste("xsd", version, root, namespaced, sep = "|")
  if (is.null(.cache[[key]])) {
    entry <- hprim_schema(version, root)$entry
    path <- if (namespaced) entry else file.path(dirname(dirname(entry)), "nons", basename(entry))
    .cache[[key]] <- xml2::read_xml(path)
  }
  .cache[[key]]
}

#' Valider un message HPRIM XML contre son XSD
#'
#' La variante de schéma (avec ou sans namespace) est choisie d'après le
#' message lui-même.
#'
#' @param x Message : chaîne XML, chemin de fichier, vecteur raw ou document xml2.
#' @param version Version du schéma ; par défaut l'attribut `version` du message.
#' @return Vecteur des erreurs, vide si le message est conforme.
#' @examples
#' f <- system.file("extdata", "pmsi_00001.xml", package = "hprimxml")
#' hprim_validate(f)
#' @export
hprim_validate <- function(x, version = NULL) {
  doc <- as_xml_doc(x)
  root <- xml2::xml_root(doc)
  ns <- xml2::xml_ns(doc)
  namespaced <- HPRIM_NS %in% unname(ns)
  version <- version %||% xml2::xml_attr(root, "version")
  schema <- xml_schema_doc(version, xml2::xml_name(root), namespaced)
  ok <- xml2::xml_validate(doc, schema)
  if (isTRUE(ok)) character() else as.character(attr(ok, "errors"))
}

as_xml_doc <- function(x) {
  if (inherits(x, "xml_document")) return(x)
  if (is.raw(x)) return(xml2::read_xml(x))
  if (is.character(x) && length(x) == 1 && !grepl("^\\s*<", x)) return(xml2::read_xml(x))
  xml2::read_xml(x)
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
