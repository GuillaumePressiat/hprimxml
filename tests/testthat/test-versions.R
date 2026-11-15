# Écriture dans toutes les versions : anciennes versions, accord des valeurs énumérées.
ent <- list(emetteur = "DIM", destinataire = "DPI")

test_that("accord des valeurs énumérées aux accents près", {
  expect_identical(hprimxml:::accorder_enumeration("creation", c("cr\u00e9ation", "modification")), "cr\u00e9ation")
  expect_identical(hprimxml:::accorder_enumeration("r\u00e9el", c("reel", "test")), "reel")
  expect_identical(hprimxml:::accorder_enumeration("creation", c("cr\u00e9ation", "creation")), "creation")
  expect_identical(hprimxml:::accorder_enumeration("autre", "cr\u00e9ation"), "autre")
})

test_that("serveur d'actes : toutes les versions s'écrivent et se valident", {
  x <- executer_exemple("serveur_actes")
  for (v in hprim_available_versions()) {
    p <- write_evenements_serveur_actes(x$env$evenements, x$env$actes_ccam, entete = ent, out_dir = tempfile(), version = v)
    expect_length(hprim_validate(p), 0)
  }
})

test_that("PMSI 1.0x : action accentuée, 1.06 sans validation possible", {
  x <- executer_exemple("sejour")
  evt <- x$env$evenements; evt$nom_usuel <- "DUPONT"; evt$prenom <- "ANNE"
  for (v in c("1.04", "1.05", "1.07")) {
    p <- write_evenements_pmsi(evt, x$env$rum, x$env$diagnostics, x$env$actes_ccam, entete = ent, out_dir = tempfile(), version = v)
    expect_length(hprim_validate(p), 0)
    expect_match(paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = ""), "<rum action=\"cr\u00e9ation\"", fixed = TRUE)
  }
  expect_error(write_evenements_pmsi(evt, x$env$rum, x$env$diagnostics, x$env$actes_ccam, entete = ent,
                                     out_dir = tempfile(), version = "1.06"), class = "hprim_schema_error")
  p <- write_evenements_pmsi(evt, x$env$rum, x$env$diagnostics, x$env$actes_ccam, entete = ent, out_dir = tempfile(),
                             version = "1.06", validate_xsd = FALSE)
  expect_identical(read_evenements_pmsi(p)$evenements$version, "1.06")
})
