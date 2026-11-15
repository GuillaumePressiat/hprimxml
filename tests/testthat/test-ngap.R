# Actes NGAP du serveur d'actes : écriture, lecture, mélange avec la CCAM, atomicité.
ent <- list(version = "2.00", emetteur = "DIM", destinataire = "DPI")
evt <- tibble::tibble(evt_id = "E1", ipp = "1", sexe = "F", date_naissance = as.Date("1958-03-12"),
                      nda = "N1", venue_date_entree = as.Date("2024-03-08"))
ngap <- tibble::tibble(evt_id = "E1", acte_id = c("NG01", "NG02", "NG03"),
  lettre_cle = c("CS", "FTN", "B"), coefficient = c("1", "1", "26"),
  date_execution = as.Date("2024-03-08"), heure_execution = hms::as_hms(c("09:30:00", "10:15:00", "11:00:00")),
  medecin = "M1", uf = c("2101", "3301", "5201"),
  numero_forfait_technique = c(NA, "1", NA), nabm = list(NULL, NULL, c(1104L, 593L)))
ccam <- tibble::tibble(evt_id = "E1", acte_id = "CC01", code = "DEQP003", activite = 1L, phase = 0L,
                       date_execution = as.Date("2024-03-08"), medecin = "M1")
compte <- function(f, b) { x <- paste(readLines(f, warn = FALSE), collapse = ""); lengths(regmatches(x, gregexpr(b, x, fixed = TRUE))) }

test_that("NGAP : valide en 2.00 et 2.4, relecture", {
  for (v in c("2.00", "2.4")) {
    d <- tempfile()
    p <- write_evenements_serveur_actes(evt, actes_ngap = ngap, entete = ent, out_dir = d, version = v)
    expect_length(hprim_validate(p), 0)
    expect_identical(compte(p, "<acteNGAP"), 3L)
  }
  t <- read_evenements_serveur_actes(d)
  expect_identical(t$actes_ngap$acte_id, c("NG01", "NG02", "NG03"))
  expect_identical(t$actes_ngap$lettre_cle, c("CS", "FTN", "B"))
  expect_identical(t$actes_ngap$nabm[[3]], c(1104L, 593L))
  expect_identical(nrow(t$actes_ccam), 0L)
})

test_that("NABM en chaîne = NABM en liste", {
  n2 <- ngap; n2$nabm <- c(NA, NA, "1104, 593")
  a <- write_evenements_serveur_actes(evt, actes_ngap = n2, entete = ent, out_dir = tempfile())
  b <- write_evenements_serveur_actes(evt, actes_ngap = ngap, entete = ent, out_dir = tempfile())
  expect_identical(read_bytes(a), read_bytes(b))
})

test_that("CCAM et NGAP dans un message, puis par acte", {
  p <- write_evenements_serveur_actes(evt, ccam, ngap, entete = ent, out_dir = tempfile())
  expect_length(hprim_validate(p), 0)
  x <- paste(readLines(p, warn = FALSE), collapse = "")
  expect_true(regexpr("<actesCCAM>", x, fixed = TRUE) < regexpr("<actesNGAP>", x, fixed = TRUE))
  p <- write_evenements_serveur_actes(evt, ccam, ngap, entete = ent, out_dir = tempfile(), par_acte = TRUE)
  expect_identical(basename(p), paste0("E1_", 1:4, ".xml"))
  expect_identical(vapply(p, compte, 1L, b = "<acteCCAM", USE.NAMES = FALSE), c(1L, 0L, 0L, 0L))
  expect_identical(vapply(p, compte, 1L, b = "<acteNGAP", USE.NAMES = FALSE), c(0L, 1L, 1L, 1L))
})

test_that("ancien nom `actes` accepté avec un avertissement", {
  expect_warning(a <- write_evenements_serveur_actes(evt, actes = ccam, entete = ent, out_dir = tempfile()), "actes_ccam")
  b <- write_evenements_serveur_actes(evt, actes_ccam = ccam, entete = ent, out_dir = tempfile())
  expect_identical(read_bytes(a), read_bytes(b))
  expect_error(suppressWarnings(write_evenements_serveur_actes(evt, actes_ccam = ccam, actes = ccam, entete = ent,
                                                               out_dir = tempfile())), "en passer qu")
})
