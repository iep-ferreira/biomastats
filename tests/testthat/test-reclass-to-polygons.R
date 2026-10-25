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
  expect_equal(sf::st_crs(reclass_to_polygons(obj, save_shp = FALSE)), sf::st_crs(obj$rec_map[[1]]))
  expect_equal(sf::st_crs(reclass_to_polygons(obj, save_shp = FALSE, crs = 4326))$epsg, 4326)
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
  expect_named(p[[2]], c("value", "area_ha", "year", "fragment_id", "code", "classe",
                         "class", "color", "collection", "classes", "geometry"))
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
  expect_equal(length(sf::st_geometry(p)[[1]][[1]]), 2L)
  expect_equal(p$area_ha, 0.72)
  obj <- polygon_fixture(c(1, 0, 0, 0, 1, 0, 0, 0, 1))
  obj$classes <- c(3, 4)
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  expect_equal(nrow(p), 1L)
  expect_equal(length(sf::st_geometry(p)[[1]]), 3L)
  expect_equal(attr(p, "metadata")$connectivity, 8L)
  expect_true(all(sf::st_is_valid(p)))
  rook <- reclass_to_polygons(obj, directions = 4, save_shp = FALSE)
  expect_equal(nrow(rook), 3L)
  expect_true(all(sf::st_is_valid(rook)))
  expect_error(reclass_to_polygons(obj, directions = 6, save_shp = FALSE), "directions")
})

test_that("8-neighbour fragments never touch and feed land_connectivity", {
  obj <- polygon_fixture(c(1, 1, 0, 0, 0, 1, 1, 0, 0))
  rook <- reclass_to_polygons(obj, directions = 4, save_shp = FALSE)
  d <- sf::st_distance(rook)
  expect_equal(nrow(rook), 3L)
  expect_true(any(as.numeric(d[upper.tri(d)]) == 0))
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  expect_equal(nrow(p), 2L)
  expect_true(all(as.character(sf::st_geometry_type(p)) == "MULTIPOLYGON"))
  d <- sf::st_distance(p)
  expect_true(all(as.numeric(d[upper.tri(d)]) > 0))
  out <- land_connectivity(p, max_dist = 100, build_plot = FALSE)
  expect_equal(nrow(out$analysis_polygons), nrow(p))
})

test_that("forest polygons follow the tutorial workflow", {
  example_env <- new.env(parent = emptyenv())
  load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"),
       envir = example_env)
  mapas <- example_env$mapas
  mapas$collection <- 10

  metricas <- biomastats_metrics(
    mapas, start = 1985, end = 1985, zone = "22", hemisphere = "south",
    classes = 3, metrics = "class_area", export.raster = TRUE
  )
  expect_s3_class(reclass_map(metricas, year = 1985), "ggplot")

  poligonos <- reclass_to_polygons(
    metricas, years = 1985, save_shp = FALSE
  )
  expect_s3_class(poligonos, "sf")
  expect_gt(nrow(poligonos), 0L)
  expect_equal(unique(poligonos$year), 1985)
  expect_true(all(poligonos$value == 1))
  expect_true(all(poligonos$area_ha > 0))
  expect_true(all(sf::st_is_valid(poligonos)))
  expect_equal(metricas$collection, 10)
  expect_equal(attr(poligonos, "class_dictionary"), metricas$class_dictionary)
  expect_equal(unique(poligonos$code), 3)
  expect_equal(unique(poligonos$color), metricas$class_dictionary$color)
  expect_equal(sf::st_crs(poligonos), sf::st_crs(metricas$rec_map[[1]]))
  expect_equal(attr(poligonos, "metadata")$upstream, metricas$metadata)
})

test_that("polygons preserve dictionary identities through serialization and SHP", {
  obj <- polygon_fixture()
  obj$collection <- "10"
  outdir <- tempfile("polygons-contract-")
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  p <- reclass_to_polygons(obj, only_class = FALSE, output_dir = outdir)
  expect_equal(sum(p$area_ha[p$value == 1]), 0.27)
  expect_equal(sum(p$area_ha), 0.72)
  dic <- dict_build(10)
  expect_equal(unique(p$color[p$value == 1]), dic$color[dic$code == 3])
  expect_true(all(is.na(p$code[p$value == 0])))
  expect_equal(attr(p, "metadata")$source[[1]]$resolution, raster::res(obj$rec_map[[1]]))
  saved <- sf::read_sf(attr(p, "shp_paths")[[1]])
  expect_equal(saved$code, p$code)
  expect_equal(saved$color, p$color)
  dir.create(outdir, showWarnings = FALSE)
  saveRDS(p, file.path(outdir, "polygons.rds"))
  recovered <- readRDS(file.path(outdir, "polygons.rds"))
  expect_equal(attr(recovered, "metadata"), attr(p, "metadata"))
  expect_equal(attr(recovered, "class_dictionary"), attr(p, "class_dictionary"))
  expect_equal(recovered, p)
  obj$classes <- c(3, 4)
  merged <- reclass_to_polygons(obj, save_shp = FALSE)
  expect_true(all(is.na(merged$code)))
  expect_true(all(is.na(merged$color)))
  expect_equal(unique(merged$classes), "3,4")
  expect_equal(attr(merged, "class_dictionary")$code, c(3, 4))
  legacy <- reclass_to_polygons(polygon_fixture(), save_shp = FALSE)
  expect_true(all(is.na(legacy$color)))
  expect_null(attr(legacy, "class_dictionary"))
})

test_that("metrics inherit years and select the matching source raster", {
  r <- polygon_fixture()$rec_map[[1]]
  maps <- list(raster = list(raster::setValues(r, rep(4, 9)),
                             raster::setValues(r, rep(3, 9))),
               time_range = c(1985, 1986), collection = "10")
  result <- biomastats_metrics(maps, start = 1986, zone = "22", hemisphere = "south",
                              classes = 3, metrics = "class_area")
  expect_equal(result$end, 1986)
  expect_equal(result$metadata$time_range, c(1986, 1986))
  expect_equal(result$collection, 10)
  expect_equal(unique(na.omit(raster::getValues(result$rec_map[[1]]))), 3)
  p <- reclass_to_polygons(result, save_shp = FALSE)
  expect_equal(sum(p$area_ha), 0.81)
  expect_equal(unique(p$year), 1986)
  expect_equal(unique(p$code), 3)
})
