# Les scripts inst/exemples/*.R produisent exactement les fichiers des scripts Python
# équivalents (fixtures générées par tools/fixtures_python.py).

for (nom in c("pmsi", "serveur_actes", "etat_patient", "sejour", "diagnostics", "igs2", "atomicite")) {
  test_that(sprintf("exemple %s : fichiers identiques à Python, octet pour octet", nom), {
    x <- executer_exemple(nom)
    ref <- fixture(file.path("exemples", paste0("exemple_", nom)))
    attendus <- sort(list.files(ref, recursive = TRUE))
    expect_identical(fichiers_produits(x$dir), attendus)
    for (f in attendus) {
      expect_identical(read_bytes(file.path(x$dir, "sortie", f)), read_bytes(file.path(ref, f)), label = f)
    }
  })
}

test_that("IGS2 : scores et validité", {
  x <- executer_exemple("igs2")
  expect_identical(x$env$igs2$valeurGlobale, c(54L, 56L))
  t <- read_evenements_etat_patient(file.path(x$dir, "sortie", "igs2"))
  expect_identical(t$evenements$recueil_uf, c("4501", "4501"))
  expect_identical(t$evenements$recueil_date, as.Date(c("2024-03-06", "2024-03-06")))
  for (f in list.files(file.path(x$dir, "sortie", "igs2"), full.names = TRUE)) {
    expect_length(hprim_validate(f), 0)
  }
})
