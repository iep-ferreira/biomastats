# Local-only test: a local MapBiomas 10 m collection (Collection 4 beta,
# Sentinel-2, 2017-2025) for Lagoa do Sino - SP, stored outside the package.
# Excluded from the built package (.Rbuildignore: tests/testthat/test-local-*),
# so GitHub Actions never runs it; skip_on_ci() is an extra safeguard.
#
# Folder (option biomastats.mapbiomas10m_dir or environment variable
# BIOMASTATS_MAPBIOMAS10M_DIR; default ~/Documents/mapbiomas_10m_col4) with
# mapbiomas10m_<year>.tif and the study area (lagoa_do_sino.gpkg or .shp,
# detected). The rasters are windows of the public files at
# storage.googleapis.com/mapbiomas-public/initiatives/brasil/lulc_10m/
# collection4/coverage/brazil_coverage/, read with GDAL /vsicurl for the study
# area plus 5 km (about 0.17 MB per year); load_rasters(method = "http",
# product = "mapbiomas_brasil_10m_c4", export_folder_path = ) rebuilds them.

mapbiomas10m_dir <- function() {
  dir <- getOption("biomastats.mapbiomas10m_dir",
    Sys.getenv("BIOMASTATS_MAPBIOMAS10M_DIR", path.expand("~/Documents/mapbiomas_10m_col4")))
  if (!dir.exists(dir) || !length(list.files(dir, "[.](gpkg|shp)$"))) {
    testthat::skip(paste("Colecao MapBiomas 10 m ausente em", dir))
  }
  dir
}

test_that("load_local_rasters reads the local MapBiomas 10 m collection", {
  skip_on_ci()
  skip_on_cran()
  dir <- mapbiomas10m_dir()
  maps <- load_local_rasters(folder = dir,
                             years = 2023:2025, collection = 10)
  expect_equal(maps$time_range, c(2023, 2025))
  expect_equal(maps$metadata$harmonization$action, rep("none", 3))
  # Pixel de 10 m (0,0000898 grau).
  expect_equal(raster::res(maps$raster[[1]]), rep(8.983153e-05, 2), tolerance = 1e-6)
  a <- get_area(maps)
  dict <- attr(a, "class_dictionary")
  expect_equal(dict$classe[dict$code == 3], "Forma\u00e7\u00e3o Florestal")
  total <- tapply(a$aggregate_data$area, a$aggregate_data$year, sum)
  study <- as.numeric(sf::st_area(.read_study_area(dir)))
  expect_equal(as.numeric(total), rep(study / 1e6, 3), tolerance = 0.02)
})

test_that("connectivity_context reloads the 10 m context from the same folder", {
  skip_on_ci()
  skip_on_cran()
  dir <- mapbiomas10m_dir()
  maps <- load_local_rasters(folder = dir,
                             years = 2025, collection = 10)
  ctx <- connectivity_context(maps, classes = 3, max_dist = 100, context = c(0, 250, 1000),
                              metric_patch = c("IIC", "PC"), d_ref = 50, build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "load_local_rasters")
  s <- ctx$sensitivity
  expect_true(all(diff(s$context_patches[s$metric == "PC"]) > 0))
  for (metric in c("IIC", "PC")) {
    expect_true(all(diff(s$ECA_ha[s$metric == metric]) >= -1e-9))
  }
  expect_equal(unique(attr(ctx$polygons[["2025"]], "class_dictionary")$classe),
               "Forma\u00e7\u00e3o Florestal")
})
