ext <- function(f) system.file("extdata", f, package = "hprimxml", mustWork = TRUE)
fixture <- function(f) test_path("fixtures", "python", f)
read_bytes <- function(f) readBin(f, "raw", file.size(f))

exemple <- function(nom) system.file("exemples", paste0("exemple_", nom, ".R"), package = "hprimxml",
                                     mustWork = TRUE)

# Exécute un script d'exemple dans un dossier temporaire ; renvoie son environnement et le dossier
executer_exemple <- function(nom) {
  dir <- tempfile("exemple_")
  dir.create(dir)
  old <- setwd(dir)
  on.exit(setwd(old))
  env <- new.env()
  suppressWarnings(sys.source(exemple(nom), envir = env))
  list(env = env, dir = dir)
}

fichiers_produits <- function(dir) sort(list.files(file.path(dir, "sortie"), recursive = TRUE))

chemins_extra <- function(env, tables) {
  stats::setNames(lapply(tables, function(t) { n <- names(env[[t]]); n[grepl("/", n) | startsWith(n, "@")] }),
                  tables)
}
