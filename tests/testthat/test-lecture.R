test_that("les valeurs sont typées d'après le XSD", {
  msg <- hprim_parse_xml(ext("pmsi_00001.xml"))
  evt <- msg$evenementsPMSI$evenementPMSI[[1]]
  expect_s3_class(evt$patient$personnePhysique$dateNaissance$date, "Date")
  rum <- evt$rss$rum
  expect_s3_class(rum[[1]]$dateAction, "POSIXct")
  expect_identical(format(rum[[1]]$dateAction, "%Y-%m-%dT%H:%M:%S"), "2024-01-01T08:10:00")
  expect_s3_class(rum[[1]]$uniteMedicale$entree$heure, "hms")
  expect_identical(rum[[1]]$actes$acte[[1]]$CCAM$codePhase, 0L)
  # attributs en texte
  expect_identical(rum[[1]]$uniteMedicale$entree$mode, "8")
})

test_that("un élément répétable est toujours une liste, même unique", {
  msg <- hprim_parse_xml(ext("pmsi_00001.xml"))
  rum <- msg$evenementsPMSI$evenementPMSI[[1]]$rss$rum
  expect_length(rum, 2)
  expect_length(rum[[2]]$actes$acte, 1)          # un seul acte : liste de longueur 1
  expect_null(names(rum[[2]]$actes$acte))
})

test_that("le namespace est indifférent à la lecture", {
  tree <- hprim_parse_xml(ext("pmsi_00001.xml"))
  avec <- hprim_to_xml(tree, drop_empty = FALSE)
  sans <- hprim_to_xml(tree, drop_empty = FALSE, xmlns = NULL)
  expect_identical(hprim_parse_xml(sans), hprim_parse_xml(avec))
})

test_that("typed = FALSE garde tout en texte", {
  msg <- hprim_parse_xml(ext("pmsi_00001.xml"), typed = FALSE)
  expect_identical(msg$evenementsPMSI$evenementPMSI[[1]]$patient$personnePhysique$dateNaissance$date,
                   "1970-01-01")
})
