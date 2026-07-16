example_maps <- function() {
  path <- system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")
  stopifnot(nzchar(path), file.exists(path))

  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  mapas <- env[["mapas"]]

  # The bundled fixture predates the collection field.  Keep the fixture
  # compatible with the current analysis functions without calling
  # load_rasters() in the tests.
  if (is.null(mapas[["collection"]])) {
    mapas[["collection"]] <- 10
  }

  mapas
}
