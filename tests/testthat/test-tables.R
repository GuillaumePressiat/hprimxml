cmp_tables <- function(relu, attendu) {
  expect_equal(as.data.frame(relu[names(attendu)]), as.data.frame(attendu), ignore_attr = TRUE)
}

test_that("PMSI : tables -> XML -> tables", {
  x <- executer_exemple("pmsi")
  tables <- c("evenements", "rum", "diagnostics", "actes")
  t <- read_evenements_pmsi(file.path(x$dir, "sortie", "pmsi"), extra = chemins_extra(x$env, tables))
  for (n in tables) cmp_tables(t[[n]], x$env[[n]])
})

test_that("serveur d'actes et états patient : tables -> XML -> tables", {
  x <- executer_exemple("serveur_actes")
  t <- read_evenements_serveur_actes(file.path(x$dir, "sortie", "actes"),
                                     extra = chemins_extra(x$env, c("evenements", "actes")))
  cmp_tables(t$evenements, x$env$evenements)
  cmp_tables(t$actes, x$env$actes)
  x <- executer_exemple("etat_patient")
  t <- read_evenements_etat_patient(file.path(x$dir, "sortie", "etat_patient"),
                                    extra = chemins_extra(x$env, c("evenements", "diagnostics")))
  cmp_tables(t$evenements, x$env$evenements)
  cmp_tables(t$diagnostics, x$env$diagnostics)
})

test_that("diagnostics en colonnes : relecture et réécriture identique", {
  x <- executer_exemple("sejour")
  src <- file.path(x$dir, "sortie", "sejour")
  t <- read_evenements_pmsi(src, format_diagnostics = "colonnes",
                            extra = list(rum = "medecinResponsable/noRPPS"))
  expect_identical(t$rum$das, list(c("E119", "I10", "E780"), c("N183", "E119")))
  expect_identical(nrow(t$diagnostics), 0L)
  p <- write_evenements_pmsi(t$evenements, t$rum, t$diagnostics, t$actes,
                             out_dir = file.path(x$dir, "reecrit"), xmlns = NULL)
  expect_identical(read_bytes(p), read_bytes(file.path(src, "S240001.xml")))
})

test_that("règle identifiantMessage = nom de fichier", {
  x <- executer_exemple("sejour")
  e <- x$env
  evt <- e$evenements
  evt$identifiant_message <- "AUTRE"
  expect_error(write_evenements_pmsi(evt, e$rum, e$diagnostics, e$actes, entete = e$entete,
                                     out_dir = tempfile()), "du nom de fichier")
  p <- write_evenements_pmsi(evt, e$rum, e$diagnostics, e$actes, entete = e$entete, out_dir = tempfile(),
                             identifiant_egal_nom_fichier = FALSE)
  expect_match(paste(readLines(p, warn = FALSE), collapse = ""), "<identifiantMessage>AUTRE</identifiantMessage>")
  expect_error(write_evenements_pmsi(e$evenements, e$rum, e$diagnostics, e$actes, entete = e$entete,
                                     out_dir = tempfile(), par_rum = TRUE,
                                     filename = "RUM_{nda}_{rum_id}.xml"), "12 caract")
  expect_error(write_evenements_pmsi(e$evenements, e$rum, e$diagnostics, e$actes, entete = e$entete,
                                     out_dir = tempfile(), par_rum = TRUE, filename = "RSS.xml"),
               "produit par deux")
})

test_that("dépôts .ok : suivi et lecture des dépôts complets", {
  x <- executer_exemple("sejour")
  d <- file.path(x$dir, "sortie", "rum")
  expect_false(any(grepl("\\.tmp$", list.files(d, all.files = TRUE))))
  file.remove(file.path(d, "S240001_2.ok"))
  file.create(file.path(d, "ORPHELIN.ok"))
  s <- suivi_depots(d)
  expect_identical(stats::setNames(s$complet, s$evt_id),
                   c(S240001_1 = TRUE, S240001_2 = FALSE, ORPHELIN = FALSE))
  expect_warning(t <- read_evenements_pmsi(d, exiger_ok = TRUE), "1 fichier")
  expect_identical(t$evenements$evt_id, "S240001_1")
})

test_that("diagnostics : DP en double refusé", {
  x <- executer_exemple("sejour")
  e <- x$env
  rum <- e$rum
  rum$dp <- "I10"
  expect_error(write_evenements_pmsi(e$evenements, rum, e$diagnostics, e$actes, entete = e$entete,
                                     out_dir = tempfile()), "plusieurs DP")
})

test_that("états patient 2.4 : <divers/> et dateObservation", {
  evt <- tibble::tibble(evt_id = "EP1", ipp = "123", sexe = "F", date_naissance = as.Date("1970-01-01"),
                        date_observation = as.Date("2024-01-02"))
  dia <- tibble::tibble(evt_id = "EP1", type = "dp", code = "Z511")
  p <- write_evenements_etat_patient(evt, dia, entete = list(emetteur = "A", destinataire = "B"),
                                     out_dir = tempfile())
  xml <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(xml, "<divers/>", fixed = TRUE)
  expect_length(hprim_validate(p), 0)
  p2 <- write_evenements_etat_patient(evt, dia, entete = list(emetteur = "A", destinataire = "B"),
                                      out_dir = tempfile(), version = "2.00")
  expect_false(grepl("divers", paste(readLines(p2, warn = FALSE), collapse = "")))
})
