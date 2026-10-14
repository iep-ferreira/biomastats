polygon_fixture <- function(values = c(1, 1, 0, 0, 0, 0, 0, 1, NA)) {
  r <- raster::raster(nrows = 3, ncols = 3, xmn = 500000, xmx = 500090,
                      ymn = 7400000, ymx = 7400090,
                      crs = "+proj=utm +zone=22 +south +datum=WGS84 +units=m")
  list(rec_map = list(raster::setValues(r, values)), start = 1985,
       end = 1985, classes = 3)
}

test_that("adjacent cells dissolve and preserve area, background and CRS", {
  obj <- polygon_fixture()
  p <- reclass_to_polygons(obj, save_shp = FALSE, crs = 32722)
  expect_s3_class(p, "sf")
  expect_equal(nrow(p), 2L)
  expect_equal(sort(p$area_ha), c(0.09, 0.18))
  expect_equal(p$fragment_id, seq_len(nrow(p)))
  expect_true(all(p$value == 1))
  expect_true(all(sf::st_is_valid(p)))
  expect_equal(sf::st_crs(p)$epsg, 32722)
  cells <- reclass_to_polygons(obj, dissolve = FALSE, only_class = FALSE,
                               save_shp = FALSE, crs = 32722)
  expect_equal(nrow(cells), 8L)
  expect_equal(sum(cells$area_ha), 0.72)
  expect_setequal(cells$value, c(0, 1))
  expect_equal(sf::st_crs(reclass_to_polygons(obj, save_shp = FALSE))$epsg, 4326)
})

test_that("years, empty selections and shapefile round trips work", {
  obj <- polygon_fixture()
  obj$rec_map[[2]] <- raster::setValues(obj$rec_map[[1]], rep(0, 9))
  obj$end <- 1986
  outdir <- tempfile("polygons-")
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  p <- reclass_to_polygons(obj, years = c(1986, 1985, 1985), output_dir = outdir)
  expect_named(p, c("1985", "1986"))
  expect_equal(nrow(p[[2]]), 0L)
  expect_named(p[[2]], c("value", "area_ha", "year", "fragment_id", "geometry"))
  expect_true(all(file.exists(attr(p, "shp_paths"))))
  saved <- sf::read_sf(attr(p, "shp_paths")[[1]])
  expect_equal(saved$frag_id, p[[1]]$fragment_id)
  expect_equal(saved$area_ha, p[[1]]$area_ha, tolerance = 1e-6)
  expect_equal(nrow(sf::read_sf(attr(p, "shp_paths")[[2]])), 0L)
  expect_s3_class(reclass_to_polygons(obj, years = 1986, save_shp = FALSE), "sf")
  obj$rec_map[[1]] <- raster::setValues(obj$rec_map[[1]], rep(NA_real_, 9))
  expect_equal(nrow(reclass_to_polygons(obj, years = 1985, save_shp = FALSE)), 0L)
})

test_that("invalid inputs fail before writing", {
  obj <- polygon_fixture()
  expect_error(reclass_to_polygons(NULL), "obj")
  for (yr in list(NA, 1985.5, numeric(), "1985")) {
    expect_error(reclass_to_polygons(obj, years = yr), "years")
  }
  expect_error(reclass_to_polygons(obj, years = 1984), "intervalo")
  expect_error(reclass_to_polygons(obj, dissolve = NA), "logicos")
  obj["rec_map"] <- list(NULL)
  expect_error(reclass_to_polygons(obj), "rec_map")
})

test_that("holes and diagonal fragments retain their topology", {
  obj <- polygon_fixture(c(1, 1, 1, 1, 0, 1, 1, 1, 1))
  p <- reclass_to_polygons(obj, save_shp = FALSE, crs = 32722)
  expect_equal(nrow(p), 1L)
  expect_equal(length(sf::st_geometry(p)[[1]]), 2L)
  expect_equal(p$area_ha, 0.72)
  obj <- polygon_fixture(c(1, 0, 0, 0, 1, 0, 0, 0, 1))
  obj$classes <- c(3, 4)
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  expect_equal(nrow(p), 3L)
  expect_true(all(sf::st_is_valid(p)))
})

test_that("forest polygons follow the tutorial workflow", {
  example_env <- new.env(parent = emptyenv())
  load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"),
       envir = example_env)
  mapas <- example_env$mapas
  mapas$collection <- 10

  metricas <- biomastats::biomastats_metrics(
    mapas, start = 1985, end = 1985, zone = "22", hemisphere = "south",
    classes = 3, metrics = "class_area", export.raster = TRUE
  )
  expect_s3_class(biomastats::reclass_map(metricas, year = 1985), "ggplot")

  poligonos <- biomastats::reclass_to_polygons(
    metricas, years = 1985, save_shp = FALSE
  )
  expect_s3_class(poligonos, "sf")
  expect_gt(nrow(poligonos), 0L)
  expect_equal(unique(poligonos$year), 1985)
  expect_true(all(poligonos$value == 1))
  expect_true(all(poligonos$area_ha > 0))
  expect_true(all(sf::st_is_valid(poligonos)))
})
