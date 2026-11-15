# Anonymisation : transformation aléatoire par fichier, faker ; à graine égale, mêmes fichiers
# que pyhprimxml, octet pour octet (fixtures/python/anonymes).
sources <- vapply(c("pmsi_00001", "actes_00001", "etat_patient_00001", "special"), function(n) ext(paste0(n, ".xml")), "")

test_that("pseudonyme, décalage et faker identiques à Python (valeurs de référence)", {
  # valeurs calculées par pyhprimxml
  expect_identical(hprim_pseudonyme("190101337", "cle-de-test"), "500232054")
  expect_identical(hprim_decalage_jours("cle-de-test"), 330L)
  expect_identical(hprim_faux_nom("MARTIN", "k"), "MARCHAND")
  expect_identical(hprim_faux_prenom("JEANNE", "k", "F"), "CHRISTINE")
  expect_identical(hprim_faux_prenom("JEAN", "k", "M"), "ALAIN")
  expect_identical(hprim_faux_prenom("X", "k"), "ELODIE")
  p <- hprim_pseudonyme("AB-12 cd \u00e9", "k")
  expect_true(grepl("^[A-Z]{2}-[0-9]{2} [a-z]{2} X$", p))
})

test_that("à graine égale, fichiers identiques à Python, octet pour octet", {
  d <- tempfile()
  a <- hprim_anonymiser(sources, d, graine = "graine-de-test")
  ref <- fixture("anonymes")
  expect_identical(sort(basename(a)), sort(list.files(ref)))
  for (f in basename(a)) expect_identical(read_bytes(file.path(d, f)), read_bytes(file.path(ref, f)), label = f)
})

test_that("aucune valeur identifiante, faker cohérent, codes et attributs intacts", {
  a <- hprim_anonymiser(sources[1:3], tempfile())
  x <- paste(unlist(lapply(a, readLines, warn = FALSE, encoding = "UTF-8")), collapse = "\n")
  for (v in c("123456789", "987654321", "SANDWICK", "JOHN", "HOBBES", "1970-01-01")) expect_false(grepl(v, x, fixed = TRUE), label = v)
  t0 <- read_evenements_pmsi(sources[1])
  t1 <- read_evenements_pmsi(hprim_anonymiser(sources[1], tempfile(), graine = "g"))
  e <- t1$evenements
  expect_true(e$nom_usuel %in% hprimxml:::NOMS && e$nom_usuel == e$nom_naissance)
  expect_true(e$prenom %in% hprimxml:::PRENOMS_M)
  expect_identical(e$ipp, e$ipp_recepteur)
  expect_identical(t1$diagnostics$code, t0$diagnostics$code)
  expect_identical(e$sexe, t0$evenements$sexe)
  expect_identical(e$date_naissance, t0$evenements$date_naissance + hprim_decalage_jours("g|pmsi_00001.xml"))
})

test_that("sans graine, résultats différents ; fichiers indépendants", {
  a <- hprim_anonymiser(sources[1], tempfile()); b <- hprim_anonymiser(sources[1], tempfile())
  expect_false(identical(read_bytes(a), read_bytes(b)))
  d <- tempfile(); dir.create(d)
  file.copy(sources[1], file.path(d, c("x.xml", "y.xml")))
  p <- hprim_anonymiser(d, tempfile(), graine = "g")
  ipp <- vapply(p, function(f) read_evenements_pmsi(f)$evenements$ipp, "")
  expect_false(ipp[1] == ipp[2])
})
