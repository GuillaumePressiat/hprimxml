test_that("les éléments vides sont supprimés et la validation repère le défaut restant", {
  out <- hprim_to_xml(hprim_parse_xml(ext("pmsi_00001.xml")))
  expect_false(grepl("<seance", out, fixed = TRUE))
  expect_false(grepl("radiotherapie", out, fixed = TRUE))
  err <- hprim_validate(out)
  expect_length(err, 1)
  expect_match(err, "agent")
})

test_that("validation avec et sans namespace", {
  tree <- hprim_parse_xml(ext("pmsi_00001.xml"))
  for (ns in list(HPRIM_NS, NULL, "")) {
    expect_length(hprim_validate(hprim_to_xml(tree, xmlns = ns)), 1)
  }
})

test_that("une clé inconnue du XSD lève une erreur (strict)", {
  tree <- hprim_parse_xml(ext("pmsi_00001.xml"))
  tree$evenementsPMSI$enteteMessage$champInvente <- "x"
  expect_error(hprim_to_xml(tree), "champInvente")
  expect_warning(hprim_to_xml(tree, strict = FALSE), "champInvente")
})

test_that("ordre des clés indifférent, préfixe @ facultatif, vecteurs = répétition", {
  tree <- list(evenementsPMSI = list(
    evenementPMSI = list(list(
      rss = list(rum = list(list(
        diagnostics = list(diagnosticPrincipal = list(codeCim10 = "Z511")),
        uniteMedicale = list(entree = list(date = as.Date("2024-01-01"), `@mode` = "8"), code = "4321"),
        identifiant = list(emetteur = "R1"),
        dateAction = as.POSIXct("2024-01-01 08:10:00", tz = "UTC"),
        actes = list(acte = list(list(CCAM = list(
          quantite = 1, codeActivite = 1L, codePhase = 0L, codeActe = "EBLA003",
          modificateurs = list(modificateur = c("7", "J")))))),
        action = "creation"
      )), identifiantRSS = list(emetteur = "0")),
      venue = list(entree = list(dateHeureOptionnelle = list(date = as.Date("2024-01-01"))),
                   identifiant = list(emetteur = list(valeur = "987654321"))),
      patient = list(personnePhysique = list(dateNaissance = list(date = as.Date("1970-01-01")),
                                             sexe = "M"),
                     identifiant = list(emetteur = list(valeur = "123")))
    )),
    enteteMessage = list(
      destinataire = list(agents = list(agent = list(code = "DPI", categorie = "application"))),
      emetteur = list(agents = list(agent = list(code = "DIM", categorie = "application"))),
      dateHeureProduction = as.POSIXct("2024-01-01 01:01:01", tz = "UTC"),
      identifiantMessage = "M1"
    )
  ))
  out <- hprim_to_xml(tree, validate_xsd = "error")
  expect_match(out, "<modificateur>7</modificateur>\n +<modificateur>J</modificateur>")
  expect_match(out, '<entree mode="8">\n +<date>2024-01-01</date>')
  expect_match(out, "<quantite>1</quantite>")
})

test_that("dates : conversion selon le type XSD cible", {
  tree <- hprim_parse_xml(ext("pmsi_00001.xml"))
  pp <- tree$evenementsPMSI$evenementPMSI[[1]]$patient$personnePhysique
  # POSIXct vers xsd:date -> date seule
  tree$evenementsPMSI$evenementPMSI[[1]]$patient$personnePhysique$dateNaissance$date <-
    as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  expect_match(hprim_to_xml(tree), "<date>1970-01-01</date>", fixed = TRUE)
  # Date vers xsd:dateTime -> minuit
  tree$evenementsPMSI$enteteMessage$dateHeureProduction <- as.Date("2024-01-01")
  expect_match(hprim_to_xml(tree), "<dateHeureProduction>2024-01-01T00:00:00</dateHeureProduction>",
               fixed = TRUE)
})

test_that("versions disponibles", {
  expect_identical(hprim_available_versions(),
                   c("1.03a", "1.04", "1.05", "1.06", "1.07", "2.00", "2.1", "2.2", "2.3", "2.4"))
})
