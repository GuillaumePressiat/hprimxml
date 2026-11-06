# Test croisé : à partir du même fichier, R doit produire exactement les octets
# produits par pyhprimxml (fixtures générées par tools/fixtures_python.py).

sources <- c("pmsi_00001", "actes_00001", "etat_patient_00001", "special")
variantes <- list(
  brut = list(drop_empty = FALSE),
  defaut = list(),
  nons = list(xmlns = NULL),
  vide = list(xmlns = ""),
  latin1 = list(encoding = "ISO-8859-1"),
  compact = list(pretty = FALSE)
)

for (src in sources) {
  for (v in names(variantes)) {
    test_that(sprintf("%s / %s : identique à Python, octet pour octet", src, v), {
      tree <- hprim_parse_xml(ext(paste0(src, ".xml")))
      out <- tempfile(fileext = ".xml")
      do.call(hprim_write_xml, c(list(tree = tree, path = out), variantes[[v]]))
      expect_identical(read_bytes(out), read_bytes(fixture(sprintf("%s_%s.xml", src, v))))
    })
  }
}
