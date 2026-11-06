convert_value <- function(text, kind) {
  if (is.null(text) || is.na(text) || trimws(text) == "") return(NA)
  t <- trimws(text)
  out <- switch(kind,
    date = as.Date(substr(t, 1, 10), format = "%Y-%m-%d"),
    datetime = as.POSIXct(substr(t, 1, 19), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
    time = tryCatch(hms::as_hms(substr(t, 1, 8)), error = function(e) NA),
    int = suppressWarnings(as.integer(t)),
    t
  )
  # valeur non conforme à son type : conservée telle quelle en texte
  if (length(out) != 1 || is.na(out)) t else out
}

empty_node <- function() structure(list(), names = character(0))

generic_node <- function(el) {
  d <- as.list(xml2::xml_attrs(el))
  for (c in xml2::xml_children(el)) {
    n <- xml2::xml_name(c)
    v <- generic_node(c)
    d[[n]] <- if (is.null(d[[n]])) v else c(if (is_repeat(d[[n]])) d[[n]] else list(d[[n]]), list(v))
  }
  txt <- if (length(xml2::xml_children(el))) "" else xml2::xml_text(el)
  if (!length(d)) return(if (trimws(txt) == "") NA else txt)
  if (trimws(txt) != "") d[["#text"]] <- txt
  if (!length(d)) empty_node() else d
}

parse_node <- function(el, type_name, s, typed) {
  if (!is_complex(s, type_name)) {
    return(convert_value(xml2::xml_text(el), if (typed) simple_kind(s, type_name) else "str"))
  }
  ct <- ctype(s, type_name)
  d <- as.list(xml2::xml_attrs(el))
  for (child in xml2::xml_children(el)) {
    name <- xml2::xml_name(child)
    ctp <- child_type(ct, name)
    value <- if (is.null(ctp) && !name %in% ct$child_names) generic_node(child)
             else parse_node(child, ctp, s, typed)
    if (name %in% ct$repeated) {
      d[[name]] <- c(d[[name]], list(value))
    } else if (!is.null(d[[name]])) {  # répétition non prévue par le XSD
      d[[name]] <- c(if (is_repeat(d[[name]])) d[[name]] else list(d[[name]]), list(value))
    } else {
      d[name] <- list(value)
    }
  }
  if (ct$text) {
    txt <- convert_value(xml2::xml_text(el), if (typed) simple_kind(s, ct$text_type) else "str")
    if (!length(d)) return(txt)
    if (!(length(txt) == 1 && is.atomic(txt) && is.na(txt))) d[["#text"]] <- txt
  }
  if (!length(d)) empty_node() else d
}

#' Lire un message HPRIM XML en liste
#'
#' Lecture guidée par le XSD : les éléments répétables sont toujours des listes
#' non nommées (même avec un seul élément), et les valeurs sont typées d'après le
#' XSD (`Date`, `POSIXct` en UTC, `hms`, `integer` ; le reste et les attributs en
#' texte). Le namespace est ignoré. Le résultat est accepté tel quel par
#' [hprim_to_xml()], et a la même forme que le dictionnaire de `parse_xml()` en
#' Python.
#'
#' Les `dateTime` sont lus en UTC sans décalage : l'heure écrite dans le fichier
#' est conservée, et réécrite à l'identique.
#'
#' @param source Chemin de fichier, chaîne XML ou vecteur raw.
#' @param version Version du schéma ; par défaut l'attribut `version` du message.
#' @param typed `FALSE` pour garder toutes les valeurs en texte.
#' @return Liste `list(racine = list(...))`.
#' @examples
#' f <- system.file("extdata", "pmsi_00001.xml", package = "hprimxml")
#' msg <- hprim_parse_xml(f)
#' rum <- msg$evenementsPMSI$evenementPMSI[[1]]$rss$rum
#' length(rum)
#' rum[[2]]$dateAction
#' @export
hprim_parse_xml <- function(source, version = NULL, typed = TRUE) {
  doc <- as_xml_doc(source)
  doc <- xml2::xml_ns_strip(doc)
  root <- xml2::xml_root(doc)
  name <- xml2::xml_name(root)
  version <- version %||% xml2::xml_attr(root, "version") %||% DEFAULT_VERSION
  s <- hprim_schema(version, name)
  stats::setNames(list(parse_node(root, root_type(s), s, typed)), name)
}
