# Atomicité : un message par unité atomique (RUM, acte, diagnostic).
ent <- list(version = "2.00", emetteur = "DIM", destinataire = "DPI")
ctx <- function(evt_id, ...) tibble::tibble(evt_id = evt_id, ipp = "1", sexe = "F",
  date_naissance = as.Date("1958-03-12"), nda = "N1", venue_date_entree = as.Date("2024-03-04"), ...)
actes_tbl <- tibble::tibble(evt_id = "A1", acte_id = c("X1", "X2", "X3"),
  code = c("DEQP003", "ZBQK002", "YYYY033"), activite = 1L, phase = 0L,
  date_execution = as.Date("2024-03-04"), medecin = "M1")
diag_tbl <- tibble::tibble(evt_id = "D1", type = c("dp", "da"), code = c("I21.0", "E11.9"))
ident <- function(p) xml2::xml_text(xml2::xml_find_first(xml2::xml_ns_strip(xml2::read_xml(p)), "//identifiantMessage"))

test_that("par_acte : un message par acte, contexte répété, relecture", {
  d <- tempfile()
  p <- write_evenements_serveur_actes(ctx("A1", intervention_id = "INT1"), actes_tbl, entete = ent,
                                      out_dir = d, par_acte = TRUE)
  expect_identical(basename(p), c("A1_1.xml", "A1_2.xml", "A1_3.xml"))
  for (f in p) {
    x <- paste(readLines(f, warn = FALSE), collapse = "\n")
    expect_identical(lengths(regmatches(x, gregexpr("<acteCCAM", x, fixed = TRUE))), 1L)
    expect_length(hprim_validate(f), 0)
    expect_identical(ident(f), tools::file_path_sans_ext(basename(f)))
  }
  t <- read_evenements_serveur_actes(d)
  expect_identical(t$actes$acte_id, c("X1", "X2", "X3"))
  expect_identical(t$evenements$intervention_id, rep("INT1", 3))
})

test_that("par_acte : ligne enrichie et gabarit", {
  l <- lapply(iter_evenements_serveur_actes(ctx("A1", intervention_id = "INT1"), actes_tbl, entete = ent,
                                            par_acte = TRUE), `[[`, "evt")
  expect_identical(vapply(l, `[[`, "", "evt_id"), c("A1_1", "A1_2", "A1_3"))
  expect_identical(vapply(l, `[[`, "", "evt_origine"), rep("A1", 3))
  expect_identical(vapply(l, function(e) e$rang, 1L), 1:3)
  p <- write_evenements_serveur_actes(ctx("A1", intervention_id = "INT1"), actes_tbl, entete = ent,
                                      out_dir = tempfile(), par_acte = TRUE,
                                      filename = "{evt_origine}_{acte_id}.xml")
  expect_identical(basename(p), c("A1_X1.xml", "A1_X2.xml", "A1_X3.xml"))
})

test_that("par_diagnostic : un message par diagnostic", {
  d <- tempfile()
  p <- write_evenements_etat_patient(ctx("D1", recueil_id = "R1", recueil_date = as.Date("2024-03-04"),
                                         recueil_uf = "1201"), diag_tbl, entete = ent, out_dir = d,
                                     par_diagnostic = TRUE)
  expect_identical(basename(p), c("D1_1.xml", "D1_2.xml"))
  for (f in p) expect_length(hprim_validate(f), 0)
  t <- read_evenements_etat_patient(d)
  expect_identical(t$diagnostics$code, c("I210", "E119"))
})

test_that("par_rum : evt_origine", {
  rum <- tibble::tibble(evt_id = "S1", rum_id = 1:2, um = c("1201", "3405"),
                        um_date_entree = as.Date("2024-03-04"), um_mode_entree = c("8", "6"))
  l <- lapply(iter_evenements_pmsi(ctx("S1"), rum, entete = ent, par_rum = TRUE), `[[`, "evt")
  expect_identical(vapply(l, `[[`, "", "evt_origine"), c("S1", "S1"))
})

test_that("événement sans objet en mode atomique : avertissement", {
  evt <- rbind(ctx("A1", intervention_id = "INT1"), ctx("A2", intervention_id = "INT2"))
  expect_warning(p <- write_evenements_serveur_actes(evt, actes_tbl, entete = ent, out_dir = tempfile(),
                                                     par_acte = TRUE), "A2")
  expect_length(p, 3)
})

test_that("par défaut : un message par événement", {
  p <- write_evenements_serveur_actes(ctx("A1", intervention_id = "INT1"), actes_tbl, entete = ent,
                                      out_dir = tempfile())
  expect_identical(basename(p), "A1.xml")
})
