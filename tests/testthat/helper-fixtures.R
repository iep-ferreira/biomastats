example_maps <- function() {
  path <- system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")
  stopifnot(nzchar(path), file.exists(path))

  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  mapas <- env[["mapas"]]

  # The bundled example (Lagoa do Sino, MapBiomas Collection 10, 1985-2024)
  # was produced by load_rasters() and already carries its collection; the
  # fallback only protects older saved objects.
  if (is.null(mapas[["collection"]])) {
    mapas[["collection"]] <- 10
  }

  mapas
}
