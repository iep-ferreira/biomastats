options(warn = 2)

stopifnot(requireNamespace("biomastats", quietly = TRUE))
library(biomastats)

example_path <- system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")
stopifnot(nzchar(example_path), file.exists(example_path))

fixture <- new.env(parent = emptyenv())
load(example_path, envir = fixture)
mapas <- fixture[["mapas"]]
if (is.null(mapas[["collection"]])) {
  mapas[["collection"]] <- 10
}

stopifnot(is.list(mapas), inherits(mapas[["shape"]], "sf"))
stopifnot(length(mapas[["raster"]]) == diff(mapas[["time_range"]]) + 1)

areas <- get_area(mapas, plot_type = "areaplot")
stopifnot(inherits(areas[["time_series"]], "ggplot"))
stopifnot(nrow(areas[["aggregate_data"]]) > 0)

stopifnot(inherits(land_vis(mapas, year = 1990), "ggplot"))
grDevices::pdf(NULL)
on.exit(grDevices::dev.off(), add = TRUE)
stopifnot(inherits(land_dist(areas, year = 2021, type = "barplot"), "ggplot"))

metrics <- biomastats_metrics(
  mapas,
  start = 1985,
  end = 1986,
  zone = "22",
  hemisphere = "south",
  classes = c(3, 4),
  metrics = "aggregation_index"
)
stopifnot(nrow(metrics[["metrics_table"]]) == 2)
stopifnot(inherits(reclass_map(metrics, year = 1985), "ggplot"))

cat("Bioma Stats smoke test: OK\n")
