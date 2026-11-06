# Le barème IGS2 de inst/exemples/exemple_igs2.R est conforme à la grille de la SFAR.
# Source : https://sfar.org/scores/igs2.php (grille et définitions), d'après Le Gall JR et
# coll., JAMA 1993; 270: 2957-2963. Points recopiés de cette page (comme le test Python).

SFAR <- list(
  age = list(c(40, 60, 70, 75, 80), c(0, 7, 12, 15, 16, 18)),
  frequenceCardiaque = list(c(40, 70, 120, 160), c(11, 2, 0, 4, 7)),
  tensionArterielleSystolique = list(c(70, 100, 200), c(13, 5, 0, 2)),
  temperature = list(39, c(0, 3)),
  paO2_FIO2 = list(c(100, 200), c(11, 9, 6)),
  diurese = list(c(0.5, 1), c(11, 4, 0)),
  ureeSanguine = list(c(10, 30), c(0, 6, 10)),
  leucocytes = list(c(1, 20), c(12, 0, 3)),
  kaliemie = list(c(3, 5), c(3, 0, 3)),
  natremie = list(c(125, 145), c(5, 0, 1)),
  HCO3 = list(c(15, 20), c(6, 3, 0)),
  bilirubine = list(c(68.4, 102.6), c(0, 4, 9)),
  scoreGlasgow = list(c(6, 9, 11, 14), c(26, 13, 7, 5, 0))
)
SFAR_QUALITATIFS <- list(
  modeAdmission = c(chirProgrammee = 0, medecine = 6, chirUrgente = 8),
  maladieChronique = c(aucune = 0, cancerMetastatique = 9, maladieHematologiqueMaligne = 10, sida = 17)
)

x <- executer_exemple("igs2")
B <- x$env$BAREME

for (item in names(SFAR)) {
  test_that(sprintf("IGS2 SFAR : %s", item), {
    expect_equal(B[[item]][[2]], SFAR[[item]][[1]])
    expect_equal(unname(B[[item]][[3]]), SFAR[[item]][[2]])
  })
}

test_that("IGS2 SFAR : items qualitatifs", {
  for (item in names(SFAR_QUALITATIFS)) {
    q <- x$env$POINTS_QUALITATIFS[[item]][[2]]
    expect_equal(q[order(names(q))], SFAR_QUALITATIFS[[item]][order(names(SFAR_QUALITATIFS[[item]]))])
  }
})

test_that("IGS2 : valeurs aux seuils (bornes fermées à gauche)", {
  b <- B$age
  expect_identical(x$env$classer(c(39, 40, 59, 60, 79, 80, NA), b[[2]], names(b[[3]])),
                   c("inferieurA40", "40A59", "40A59", "60A69", "75A79", "superieurEgalA80", NA))
})

test_that("IGS2 : règles conditionnelles (ventilation, ictère)", {
  i <- x$env$igs2
  expect_identical(i$paO2_FIO2, c("100A199", "rien"))
  expect_identical(i$points_paO2_FIO2, c(9L, 0L))
  expect_identical(i$bilirubine, c("inferieurA68,4", "superieurA102,6"))
  expect_identical(i$points_bilirubine, c(0L, 9L))
})
