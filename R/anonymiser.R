#' @include common.R xsd.R parse.R write.R faker_noms.R
NULL

# Éléments dont tout le contenu texte est masqué (avec leurs descendants)
ELEMENTS_MASQUES <- c("identifiant", "identifiantRSS", "identifiantMessage",
                      "patient", "medecin", "medecinResponsable", "personne", "acteur",
                      "nomUsuel", "nomNaissance", "prenom", "noRPPS", "numeroAdeli",
                      "commentaire", "commentaireMessage")

# Éléments remplacés par un faux nom de famille, et par un faux prénom
ELEMENTS_NOMS <- c("nomUsuel", "nomNaissance", "nomMarital")
ELEMENT_PRENOM <- "prenom"

hmac_octets <- function(message, cle) {
  as.integer(as.raw(unclass(openssl::sha256(charToRaw(enc2utf8(message)), key = cle))))
}

choix <- function(liste, cle, message) {
  h <- hmac_octets(message, cle)[1:4]
  liste[(h[1] * 2^24 + h[2] * 2^16 + h[3] * 2^8 + h[4]) %% length(liste) + 1]
}

#' Faux nom et faux prénom (faker de l'anonymisation)
#'
#' Tirés de listes de noms et prénoms français courants embarquées dans le package (les mêmes
#' que dans pyhprimxml). Une même valeur et une même clé donnent le même remplacement. Le
#' prénom est féminin si `sexe == "F"`, masculin si `"M"`, sinon tiré des deux listes.
#'
#' @param valeur Valeur d'origine.
#' @param cle Clé de transformation (propre à un fichier dans [hprim_anonymiser()]).
#' @param sexe Sexe du patient (`"F"`, `"M"` ou `NULL`).
#' @return Un nom, ou un prénom.
#' @examples
#' hprim_faux_nom("MARTIN", cle = "k")
#' hprim_faux_prenom("JEANNE", cle = "k", sexe = "F")
#' @export
hprim_faux_nom <- function(valeur, cle) choix(NOMS, cle, paste0("nom|", valeur))

#' @rdname hprim_faux_nom
#' @export
hprim_faux_prenom <- function(valeur, cle, sexe = NULL) {
  liste <- if (identical(sexe, "F")) PRENOMS_F else if (identical(sexe, "M")) PRENOMS_M else c(PRENOMS_F, PRENOMS_M)
  choix(liste, cle, paste0("prenom|", valeur))
}

#' Pseudonyme déterministe d'une valeur
#'
#' Même longueur, même nature de caractères : chiffre -> chiffre, majuscule -> majuscule,
#' minuscule -> minuscule ; ponctuation et espaces conservés ; tout autre caractère -> `X`.
#' Calculé par HMAC-SHA256 avec la clé : identique au `pseudonyme()` de pyhprimxml. C'est la
#' fonction de base de [hprim_anonymiser()], qui tire une clé aléatoire par fichier.
#'
#' @param valeur Valeur(s) à pseudonymiser.
#' @param cle Clé secrète.
#' @return Vecteur de pseudonymes.
#' @examples
#' hprim_pseudonyme("190101337", cle = "ma-cle")
#' @export
hprim_pseudonyme <- function(valeur, cle) {
  vapply(as.character(valeur), function(v) {
    ch <- strsplit(enc2utf8(v), "")[[1]]
    n <- length(ch)
    if (!n) return(v)
    flux <- integer(0)
    bloc <- 0L
    while (length(flux) < n) {
      flux <- c(flux, hmac_octets(paste0(v, "|", bloc), cle))
      bloc <- bloc + 1L
    }
    sortie <- character(n)
    for (i in seq_len(n)) {
      cp <- utf8ToInt(ch[i])
      b <- flux[i]
      sortie[i] <- if (cp >= 48 && cp <= 57) intToUtf8(48L + b %% 10L)
        else if (cp >= 65 && cp <= 90) intToUtf8(65L + b %% 26L)
        else if (cp >= 97 && cp <= 122) intToUtf8(97L + b %% 26L)
        else if (cp < 128) ch[i]
        else "X"
    }
    paste(sortie, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' Décalage des dates dérivé de la clé
#'
#' Entre -365 et +365 jours, jamais nul ; identique au `decalage_jours()` de pyhprimxml.
#' @param cle Clé secrète.
#' @return Nombre de jours.
#' @export
hprim_decalage_jours <- function(cle) {
  h <- hmac_octets("decalage|dates", cle)[1:4]
  d <- (h[1] * 2^24 + h[2] * 2^16 + h[3] * 2^8 + h[4]) %% 731 - 365
  as.integer(if (d == 0) 1 else d)
}

anonymiser_noeud <- function(v, type_name, s, masquer, champ, sexe, cle, jours) {
  if (is.list(v) && is.null(names(v)) && length(v)) {
    return(lapply(v, anonymiser_noeud, type_name, s, masquer, champ, sexe, cle, jours))
  }
  if (is.list(v)) {
    ct <- if (is_complex(s, type_name)) ctype(s, type_name) else NULL
    if (!is.null(ct) && "sexe" %in% ct$attrs && is.character(v$sexe) && length(v$sexe) == 1 && !is.na(v$sexe)) {
      sexe <- v$sexe                                            # sexe du patient, pour le prénom
    }
    for (k in names(v)) {
      if (!is.null(ct) && k %in% ct$attrs) next                 # attribut : jamais modifié
      if (k == "#text") {
        v[k] <- list(anonymiser_noeud(v[[k]], if (is.null(ct)) NULL else ct$text_type, s, masquer,
                                      champ, sexe, cle, jours))
        next
      }
      enfant <- if (is.null(ct)) NULL else child_type(ct, k)
      v[k] <- list(anonymiser_noeud(v[[k]], enfant, s, masquer || k %in% ELEMENTS_MASQUES, k, sexe,
                                    cle, jours))
    }
    return(v)
  }
  if (inherits(v, "POSIXct")) return(v + jours * 86400)
  if (inherits(v, "Date")) return(v + jours)
  if (is.character(v) && length(v) == 1 && !is.na(v)) {
    if (!is.null(champ) && champ %in% ELEMENTS_NOMS) return(hprim_faux_nom(v, cle))
    if (identical(champ, ELEMENT_PRENOM)) return(hprim_faux_prenom(v, cle, sexe))
    if (masquer) return(hprim_pseudonyme(v, cle))
  }
  v
}

#' Anonymiser un message lu avec hprim_parse_xml()
#'
#' Voir [hprim_anonymiser()] pour les règles.
#' @param tree Liste renvoyée par [hprim_parse_xml()].
#' @param cle Clé de transformation (propre à un fichier dans [hprim_anonymiser()]).
#' @param jours Décalage des dates ; par défaut [hprim_decalage_jours()].
#' @return La liste anonymisée.
#' @export
hprim_anonymiser_message <- function(tree, cle, jours = NULL) {
  jours <- jours %||% hprim_decalage_jours(cle)
  racine <- names(tree)
  s <- hprim_schema(as.character(tree[[1]]$version), racine)
  stats::setNames(list(anonymiser_noeud(tree[[1]], root_type(s), s, FALSE, NULL, NULL, cle, jours)), racine)
}

#' Anonymiser des fichiers HPRIM XML pour des tests locaux
#'
#' Chaque fichier est lu, anonymisé et réécrit dans `out_dir` sous un nom pseudonymisé, avec
#' sa propre transformation aléatoire : rien ne relie deux fichiers anonymisés. À graine égale,
#' les fichiers produits sont identiques, octet pour octet, à ceux de `anonymiser()` dans
#' pyhprimxml.
#'
#' Règles :
#' * à l'intérieur d'un fichier, tout reste cohérent : une même valeur donne le même
#'   remplacement partout dans le fichier ;
#' * noms et prénoms (patient, professionnels) sont remplacés par des noms français plausibles
#'   (faker), le prénom selon le sexe du patient ; `nomNaissance` reste égal à `nomUsuel` quand
#'   ils l'étaient ;
#' * les identifiants (patient, venue, RSS, RUM, actes, intervention, recueil, message), les
#'   adresses et les commentaires gardent leur longueur et leur nature de caractères, pour que
#'   les fichiers restent conformes au XSD ;
#' * toutes les dates d'un fichier sont décalées d'un même nombre de jours, tiré au hasard
#'   (durées et âges conservés) ; les heures ne changent pas ;
#' * les codes et les attributs (sexe, mode, action…) ne sont jamais modifiés ;
#' * le nom de fichier est pseudonymisé comme l'identifiant du message ; un `.ok` présent à
#'   côté du fichier source est reproduit.
#'
#' Ce n'est pas une anonymisation au sens du RGPD : les fichiers restent des données de santé,
#' où codes rares, durées ou unités peuvent permettre des recoupements. Usage : tests locaux.
#'
#' @param sources Fichier(s), dossier ou motif glob.
#' @param out_dir Dossier de sortie.
#' @param graine Rend le résultat reproductible (même graine, même fichier source -> même
#'   fichier anonymisé). Réservé à la doc et aux tests ; sans graine, chaque appel donne un
#'   résultat différent.
#' @param encoding Encodage des fichiers écrits.
#' @return Les chemins des fichiers anonymisés (invisible).
#' @examples
#' f <- system.file("extdata", "pmsi_00001.xml", package = "hprimxml")
#' a <- hprim_anonymiser(f, out_dir = tempfile())
#' read_evenements_pmsi(a)$evenements[, c("ipp", "nom_usuel", "prenom", "date_naissance")]
#' @export
hprim_anonymiser <- function(sources, out_dir, graine = NULL, encoding = "UTF-8") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  produits <- character()
  for (x in iter_sources(sources)) {
    cle <- if (is.null(graine)) paste(as.character(openssl::rand_bytes(16)), collapse = "")
           else paste0(graine, "|", basename(x$src))
    ns <- HPRIM_NS %in% unname(xml2::xml_ns(xml2::read_xml(x$src)))
    tree <- hprim_anonymiser_message(hprim_parse_xml(x$src), cle)
    xml <- suppressWarnings(hprim_to_xml(tree, xmlns = if (ns) HPRIM_NS else NULL, encoding = encoding,
                                         drop_empty = FALSE, strict = FALSE))
    cible <- file.path(out_dir, paste0(hprim_pseudonyme(x$id, cle), ".", tools::file_ext(x$src)))
    if (cible %in% produits) {
      stop(sprintf(paste0("%s : le nom anonymis\u00e9 %s est d\u00e9j\u00e0 pris par un autre fichier du lot ; ",
                          "relancer (ou changer de graine)."), basename(x$src), basename(cible)), call. = FALSE)
    }
    deposer(cible, xml, encoding, fichier_ok = file.exists(chemin_ok(x$src)))
    produits <- c(produits, cible)
  }
  invisible(produits)
}
