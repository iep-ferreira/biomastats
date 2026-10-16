utm_grid <- function(values, nrow, crs = "EPSG:32722", res = 10, x0 = 500000, y0 = 7400000) {
  r <- terra::rast(nrows = nrow, ncols = length(values) / nrow, xmin = x0,
                   xmax = x0 + res * length(values) / nrow, ymin = y0,
                   ymax = y0 + res * nrow, crs = crs)
  terra::values(r) <- values
  r
}

test_that("projected grids give exact fragment geometry", {
  v <- matrix(0, 8, 10)
  v[2:3, 2:4] <- 3      # 2 x 3 block
  v[2:5, 7:10] <- 3     # 4 x 4 block touching the east border
  r <- utm_grid(as.vector(t(v)), 8)
  f <- fragment_metrics(r, classes = 3, lsm = NULL, build_plot = FALSE)
  p <- f$patches[order(f$patches$n_cells), ]
  k2 <- p$area_ha * 1e4 / (p$n_cells * 100)   # UTM scale factor squared
  expect_equal(nrow(p), 2L)
  expect_equal(p$n_cells, c(6, 16))
  expect_lt(max(abs(k2 - 1)), 2e-3)
  expect_equal(p$perimeter_m, c(100, 160) * sqrt(k2))
  expect_equal(p$core_area_ha, c(0, 4 * 100 * k2[2] / 1e4))
  expect_equal(p$shape_index, c(0.25 * 100 / sqrt(600), 1), tolerance = 1e-8)
  expect_equal(p$orientation_deg[1], 90)
  expect_gt(p$elongation[1], 0)
  expect_equal(p$elongation[2], 0, tolerance = 1e-6)
  expect_equal(p$centroid_x, c(500025, 500080))
  expect_equal(p$centroid_y, c(7400060, 7400050))
  expect_equal(p$enn_m, c(20, 20))
  expect_equal(f$metadata$source[[1]]$utm$zone, "22")
  expect_equal(f$metadata$source[[1]]$utm$hemisphere, "south")
})

test_that("connectivity follows directions and matches landscapemetrics on projected grids", {
  v <- matrix(0, 6, 6); v[2, 2] <- 3; v[3, 3] <- 3; v[5, 5:6] <- 3; v[4:6, 1] <- 3
  r <- utm_grid(as.vector(t(v)), 6)
  expect_equal(nrow(fragment_metrics(r, classes = 3, lsm = NULL, vectorize = FALSE, build_plot = FALSE)$patches), 3L)
  f4 <- fragment_metrics(r, classes = 3, directions = 4, lsm = NULL, build_plot = FALSE)
  expect_equal(nrow(f4$patches), 4L)
  f <- fragment_metrics(r, classes = 3, lsm = NULL, vectorize = FALSE, build_plot = FALSE)
  ref <- function(fun) sort(as.data.frame(fun(r, directions = 8))$value[as.data.frame(fun(r, directions = 8))$class == 3])
  expect_equal(sort(f$patches$area_ha), ref(landscapemetrics::lsm_p_area), tolerance = 2e-3)
  expect_equal(sort(f$patches$perimeter_m), ref(landscapemetrics::lsm_p_perim), tolerance = 2e-3)
  expect_equal(sort(f$patches$shape_index), ref(landscapemetrics::lsm_p_shape), tolerance = 2e-3)
})

test_that("geographic grids agree with geodesic vector geometry without reprojection", {
  maps <- example_maps()
  f <- fragment_metrics(maps, classes = 3, years = 1985, lsm = NULL, build_plot = FALSE)
  poly <- f$polygons[order(f$polygons$patch_id), ]
  geod_area <- as.numeric(sf::st_area(poly)) / 1e4
  geod_perim <- as.numeric(sf::st_perimeter(poly))
  p <- f$patches[order(f$patches$patch_id), ]
  expect_equal(p$area_ha, geod_area, tolerance = 5e-3)
  expect_equal(p$perimeter_m, geod_perim, tolerance = 5e-3)
  expect_true(all(abs(p$lon - p$centroid_x) < 1e-12))
  expect_equal(f$metadata$source[["1985"]]$utm$zone, "22")
  expect_true(f$metadata$source[["1985"]]$lonlat)
  np <- as.data.frame(landscapemetrics::lsm_c_np(maps$raster[[1]]))
  expect_equal(nrow(p), np$value[np$class == 3])
})

test_that("summaries, foot CRS, merging, lsm, plots and serialization", {
  v <- matrix(0, 6, 8); v[1:2, 1:2] <- 3; v[5:6, 5:8] <- 3; v[1, 8] <- 4
  us <- 1200 / 3937; ft <- utm_grid(as.vector(t(v)), 6, crs = "+proj=utm +zone=22 +south +datum=WGS84 +units=us-ft", res = 10 / us, x0 = 500000 / us, y0 = 7400000 / us)
  m <- utm_grid(as.vector(t(v)), 6)
  a <- fragment_metrics(ft, classes = 3, lsm = NULL, build_plot = FALSE)
  b <- fragment_metrics(m, classes = 3, lsm = NULL, build_plot = FALSE)
  expect_equal(a$summary$class_area_ha, b$summary$class_area_ha, tolerance = 1e-6)
  expect_equal(a$patches$perimeter_m, b$patches$perimeter_m, tolerance = 1e-6)
  s <- b$summary; area <- sort(b$patches$area_ha)
  expect_equal(s$n_patches, 2L)
  expect_equal(s$lpi, 100 * max(area) / s$landscape_area_ha)
  expect_equal(s$mesh_ha, sum(area^2) / s$landscape_area_ha)
  expect_equal(s$area_gini, (area[2] - area[1]) / (2 * sum(area)))
  expect_equal(s$pland, 100 * sum(area) / s$landscape_area_ha)
  merged <- fragment_metrics(m, classes = c(3, 4), merge_classes = TRUE, lsm = "lsm_c_np", build_plot = FALSE)
  expect_equal(nrow(merged$patches), 3L)
  expect_equal(merged$lsm$value[merged$lsm$metric == "np"], 3)
  maps <- example_maps()
  f <- fragment_metrics(maps, classes = c(3, 15), years = c(1985, 1986))
  expect_s3_class(f, "biomastats_fragment_metrics")
  expect_equal(sort(unique(f$lsm$metric)), c("ai", "clumpy", "cohesion"))
  expect_equal(f$class_dictionary$code, c(3, 15))
  for (p in f$plots) expect_s3_class(ggplot2::ggplot_build(p), "ggplot_built")
  en <- fragment_metrics(maps, classes = 3, years = 1985, lsm = NULL, language = "en")
  expect_equal(en$plots$shape$labels$x, "Fragment area (ha, log scale)")
  path <- tempfile(fileext = ".rds"); on.exit(unlink(path))
  saveRDS(f, path); back <- readRDS(path)
  expect_equal(back$summary, f$summary)
  expect_error(fragment_metrics(maps, classes = 999, lsm = NULL, build_plot = FALSE), "fragmento")
})

test_that("lulc_metrics runs on the native grid without zone or hemisphere", {
  maps <- example_maps()
  x <- lulc_metrics(maps$raster[[1]], classe = 3)
  f <- fragment_metrics(maps, classes = 3, years = 1985, lsm = NULL, vectorize = FALSE, build_plot = FALSE)
  expect_equal(x$total_area_for_the_class$value[2], f$summary$class_area_ha)
  expect_equal(nrow(x$area_of_the_patches), f$summary$n_patches)
  expect_equal(x$utm$zone, "22")
  expect_equal(x$total_area$value, f$summary$landscape_area_ha)
  m <- biomastats_metrics(maps, start = 1985, end = 1985, classes = 3)
  expect_equal(m$metrics_table[["Number of Fragments"]], f$summary$n_patches)
  expect_equal(m$metadata$source[["1985"]]$utm$hemisphere, "south")
})
