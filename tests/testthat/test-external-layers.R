# Light, offline tests for the external-layer functions. Network access is
# never required: remote roots point to a closed local port (fail-fast rule)
# or to synthetic local files.

lagoa_grid_features <- function() {
  maps <- example_maps()
  r <- terra::rast(maps$raster[[length(maps$raster)]])
  e <- terra::ext(r); cx <- mean(c(e$xmin, e$xmax)); cy <- mean(c(e$ymin, e$ymax))
  list(maps = maps, cx = cx, cy = cy,
       points = sf::st_sf(id = 1:3, geometry = sf::st_sfc(sf::st_point(c(cx, cy)), sf::st_point(c(cx + 0.004, cy)),
                                                          sf::st_point(c(cx, cy + 0.004)), crs = 4326)),
       line = sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_linestring(rbind(c(cx - 0.008, cy + 1e-4), c(cx + 0.008, cy + 1e-4))), crs = 4326)),
       polygon = sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(cx - 0.004, cy - 0.003), c(cx + 0.004, cy - 0.003),
                                                         c(cx + 0.004, cy + 0.003), c(cx - 0.004, cy + 0.003), c(cx - 0.004, cy - 0.003)))), crs = 4326)))
}

test_that("catalogue lists worldwide sources with catalogue years", {
  cat <- layer_catalog()
  expect_true(all(c("id", "product", "version", "catalog_year", "coverage", "license", "url") %in% names(cat)))
  expect_true(all(cat$coverage == "world"))
  expect_true(all(nzchar(cat$catalog_year)))
  expect_true(all(c("esa_worldcover_2021", "copernicus_dem", "soilgrids", "osm_geofabrik", "overture") %in% cat$id))
})

test_that("ESA WorldCover dictionary follows the native contract", {
  d <- dict_build("esa_worldcover")
  expect_equal(sort(d$code), c(10, 20, 30, 40, 50, 60, 70, 80, 90, 95, 100))
  expect_true(all(c("grupo", "group", "classe", "class", "code", "color") %in% names(d)))
  expect_equal(d$color[d$code == 10], "#006400")
  expect_equal(.biomastats_collection("esa_worldcover"), "esa_worldcover")
  expect_equal(.biomastats_dictionary("esa_worldcover", c(80, 10))$class, c("Tree cover", "Permanent water bodies"))
})

test_that("tile names cover both hemispheres", {
  expect_equal(.worldcover_tiles(c(xmin = -48.55, ymin = -23.62, xmax = -48.50, ymax = -23.59)), "S24W051")
  expect_equal(.worldcover_tiles(c(xmin = 9.5, ymin = 47.1, xmax = 9.6, ymax = 47.2)), "N45E009")
  expect_length(.worldcover_tiles(c(xmin = -48.2, ymin = -24.1, xmax = -47.8, ymax = -23.9)), 4)
  expect_equal(.copernicus_tiles(c(xmin = -48.55, ymin = -23.62, xmax = -48.50, ymax = -23.59)),
               "Copernicus_DSM_COG_10_S24_00_W049_00_DEM")
  expect_equal(.copernicus_tiles(c(xmin = 9.5, ymin = 47.1, xmax = 9.6, ymax = 47.2)),
               "Copernicus_DSM_COG_10_N47_00_E009_00_DEM")
})

test_that("unreachable sources are skipped quickly without errors or side effects", {
  closed <- "http://127.0.0.1:9"
  maps <- example_maps()
  old_gdal <- terra::getGDALconfig("GDAL_HTTP_TIMEOUT")
  withr::local_options(biomastats.worldcover_root = closed, biomastats.overture_stac = closed,
                       biomastats.copernicus_root = closed, biomastats.soilgrids_wcs = closed)
  t0 <- Sys.time()
  expect_warning(wc <- suppressMessages(load_lulc(system.file("shp", "UFSCar.shp", package = "biomastats"), years = 2021)),
                 "ignorado")
  expect_null(wc)
  f <- suppressMessages(load_features(maps, c("roads", "places"), source = "overture"))
  expect_s3_class(f, "biomastats_features")
  expect_true(all(f$status$status == "skipped"))
  expect_true(all(vapply(f$features, is.null, TRUE)))
  p <- suppressMessages(load_physical(maps, layers = c("elevation", "slope", "clay")))
  expect_equal(p$status$status, rep("skipped", 3))
  expect_length(p$rasters, 0)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 20)
  expect_equal(terra::getGDALconfig("GDAL_HTTP_TIMEOUT"), old_gdal)
  dest <- tempfile(fileext = ".bin")
  expect_error(.fetch_file(paste0(closed, "/x"), dest, 5))
  expect_false(file.exists(dest) || file.exists(paste0(dest, ".part")))
  blk <- suppressMessages(.layer_block("teste", stop("falhou")))
  expect_equal(blk$status, "skipped"); expect_null(blk$value)
})

test_that("load_lulc returns a maps object usable by the land-cover analyses", {
  root <- file.path(tempdir(), "wc-root"); dir.create(file.path(root, "v200", "2021", "map"), recursive = TRUE, showWarnings = FALSE)
  r <- terra::rast(terra::ext(-48.6, -48.45, -23.65, -23.55), resolution = 0.0005, crs = "EPSG:4326")
  set.seed(1); terra::values(r) <- sample(c(10, 30, 40, 80), terra::ncell(r), replace = TRUE)
  terra::writeRaster(r, file.path(root, "v200", "2021", "map", "ESA_WorldCover_10m_2021_v200_S24W051_Map.tif"), overwrite = TRUE)
  withr::local_options(biomastats.worldcover_root = root)
  wc <- load_lulc(system.file("shp", "UFSCar.shp", package = "biomastats"), years = 2021)
  expect_equal(names(wc)[1:4], c("shape", "time_range", "raster", "collection"))
  expect_equal(wc$time_range, c(2021, 2021))
  expect_equal(wc$collection, "esa_worldcover")
  expect_s4_class(wc$raster[[1]], "RasterLayer")
  expect_true(raster::inMemory(wc$raster[[1]]))
  expect_equal(wc$metadata$status$catalog_year, 2021)
  v <- raster::getValues(wc$raster[[1]])
  expect_true(any(is.na(v)) && all(v[!is.na(v)] %in% c(10, 30, 40, 80)))
  a <- get_area(wc)
  expect_equal(sort(unique(as.numeric(a$aggregate_data$land_class))), c(10, 30, 40, 80))
  expect_equal(attr(a, "class_dictionary")$class[attr(a, "class_dictionary")$code == 80], "Permanent water bodies")
  expect_s3_class(land_vis(wc, 2021), "ggplot")
  expect_s3_class(class_diversity(a, build_plot = FALSE), "biomastats_class_diversity")
})

test_that("feature rasters follow the geometry type and the reference grid", {
  x <- lagoa_grid_features(); maps <- x$maps
  ref <- maps$raster[[length(maps$raster)]]
  p <- feature_raster(x$points, maps, window_m = 300)
  expect_equal(names(p$rasters), c("distance", "count", "density"))
  for (r in p$rasters) expect_true(raster::compareRaster(r, ref, stopiffalse = FALSE))
  expect_equal(sum(raster::getValues(p$rasters$count), na.rm = TRUE), 3)
  cell <- raster::cellFromXY(ref, cbind(x$cx, x$cy))
  expect_equal(raster::getValues(p$rasters$distance)[cell], 0)
  expect_gt(max(raster::getValues(p$rasters$distance), na.rm = TRUE), 300)
  expect_identical(is.na(raster::getValues(p$rasters$distance)), is.na(raster::getValues(ref)))
  l <- feature_raster(x$line, maps, metrics = c("length", "density"))
  inside <- suppressWarnings(sf::st_intersection(x$line, sf::st_geometry(maps$shape)))
  expect_equal(sum(raster::getValues(l$rasters$length), na.rm = TRUE), as.numeric(sum(sf::st_length(inside))), tolerance = .05)
  expect_gt(max(raster::getValues(l$rasters$density), na.rm = TRUE), 0)
  g <- feature_raster(x$polygon, maps)
  expect_equal(names(g$rasters), c("distance", "coverage", "area", "density"))
  cov <- raster::getValues(g$rasters$coverage)
  expect_true(all(cov[!is.na(cov)] >= 0 & cov[!is.na(cov)] <= 1))
  expect_equal(sum(raster::getValues(g$rasters$area), na.rm = TRUE), as.numeric(sf::st_area(x$polygon)), tolerance = .05)
  expect_equal(raster::getValues(g$rasters$distance)[cell], 0)
  expect_error(feature_raster(x$line, maps, metrics = "coverage"), "length")
  expect_error(feature_raster(rbind(x$points[, "id"], x$line[, "id"]), maps), "pontos")
  for (pl in g$plots) expect_s3_class(ggplot2::ggplot_build(pl), "ggplot_built")
  en <- feature_raster(x$points, maps, metrics = "distance", language = "en")
  expect_match(en$metrics$label, "Distance")
})

test_that("features just outside the study area count for distances", {
  x <- lagoa_grid_features(); maps <- x$maps
  e <- terra::ext(terra::rast(maps$raster[[1]]))
  out <- sf::st_sf(id = 1, geometry = sf::st_sfc(sf::st_linestring(rbind(c(e$xmin - 0.002, e$ymin), c(e$xmin - 0.002, e$ymax))), crs = 4326))
  d <- feature_raster(out, maps, metrics = "distance")$rasters$distance
  v <- raster::getValues(d)
  expect_true(any(is.finite(v)))
  expect_gt(min(v, na.rm = TRUE), 0)
})

test_that("distance_to_nearest measures from coordinates to features and classes", {
  x <- lagoa_grid_features(); maps <- x$maps
  pts <- data.frame(lon = c(x$cx, x$cx), lat = c(x$cy + 0.002, x$cy - 0.010))
  d <- distance_to_nearest(pts, x$line)
  ref <- as.numeric(sf::st_distance(sf::st_as_sf(pts, coords = c("lon", "lat"), crs = 4326), x$line))
  expect_equal(d$distance_m, ref, tolerance = 1e-6)
  expect_equal(distance_to_nearest(data.frame(x = x$cx, y = x$cy), x$polygon)$inside, TRUE)
  f <- structure(list(features = list(roads = x$line), status = data.frame(feature = "roads", source = "teste",
    product = "sint\u00e9tico", version = "1", catalog_year = 2026)), class = "biomastats_features")
  expect_equal(distance_to_nearest(pts, f, feature = "roads")$distance_m, ref, tolerance = 1e-6)
  r <- maps$raster[[length(maps$raster)]]
  xy <- raster::xyFromCell(r, which(raster::getValues(r) == 3)[1])
  dc <- distance_to_nearest(data.frame(lon = c(xy[1], -48.70), lat = c(xy[2], -23.70)), maps, classes = 3)
  expect_equal(dc$distance_m[1], 0)
  expect_gt(dc$distance_m[2], 10000)
  expect_equal(attr(dc, "metadata")$classes, 3)
  expect_error(distance_to_nearest(pts, maps), "classes")
})
