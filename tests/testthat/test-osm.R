test_that("load_osm_data queries a key without a value", {
  reference <- raster::raster(
    nrows = 2, ncols = 2,
    xmn = 0, xmx = 2, ymn = 0, ymx = 2,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, c(1, NA, 2, 3))
  calls <- new.env(parent = emptyenv())

  mock_fetch <- function(bbox, key_feature, value_feature, timeout, endpoint) {
    calls$bbox <- bbox
    calls$key <- key_feature
    calls$value <- value_feature
    calls$timeout <- timeout
    calls$endpoint <- endpoint
    list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL)
  }
  mock_rasterize_osm <- function(osm_data, reference_raster) {
    calls$reference_values <- raster::getValues(reference_raster)
    raster::setValues(reference_raster, c(4, NA, 5, NA))
  }

  testthat::local_mocked_bindings(
    fetch_osm_feature = mock_fetch,
    rasterize_osm = mock_rasterize_osm,
    .package = "biomastats"
  )

  result <- load_osm_data(reference, key_feature = "highway")

  expect_s4_class(result, "Raster")
  expect_s4_class(result, "RasterLayer")
  expect_equal(calls$key, "highway")
  expect_null(calls$value)
  expect_true(calls$bbox[["xmin"]] < 0)
  expect_true(calls$bbox[["ymin"]] < 0)
  expect_true(calls$bbox[["xmax"]] > 2)
  expect_true(calls$bbox[["ymax"]] > 2)
  expect_equal(mean(calls$bbox[c("xmin", "xmax")]), 1, tolerance = 0.01)
  expect_equal(calls$timeout, 60)
  expect_true(grepl("^https://", calls$endpoint))
  expect_equal(calls$reference_values, c(0, NA, 0, 0))
  expect_equal(raster::getValues(result), c(0, NA, 0, NA))
})

test_that("load_osm_data passes a feature value to OSM", {
  reference <- raster::raster(
    nrows = 1, ncols = 1,
    xmn = 0, xmx = 1, ymn = 0, ymx = 1
  )
  reference <- raster::setValues(reference, 1)
  calls <- new.env(parent = emptyenv())

  mock_fetch <- function(bbox, key_feature, value_feature, timeout, endpoint) {
    calls$key <- key_feature
    calls$value <- value_feature
    list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL)
  }
  mock_rasterize_osm <- function(osm_data, reference_raster) reference_raster

  testthat::local_mocked_bindings(
    fetch_osm_feature = mock_fetch,
    rasterize_osm = mock_rasterize_osm,
    .package = "biomastats"
  )

  result <- load_osm_data(
    reference,
    key_feature = "natural",
    value_feature = "water"
  )

  expect_s4_class(result, "Raster")
  expect_equal(calls$key, "natural")
  expect_equal(calls$value, "water")
})

test_that("load_osm_data uses an adjustable metric buffer in metres", {
  reference <- raster::raster(
    nrows = 1, ncols = 1,
    xmn = -0.005, xmx = 0.005, ymn = -0.005, ymx = 0.005,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, 1)
  calls <- new.env(parent = emptyenv())
  calls$boxes <- list()

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(bbox, ...) {
      calls$boxes[[length(calls$boxes) + 1L]] <- bbox
      list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) reference_raster,
    .package = "biomastats"
  )

  load_osm_data(reference, "highway", buffer_distance = 1000)
  load_osm_data(reference, "highway", buffer_distance = 2000)

  width_1km <- diff(calls$boxes[[1]][c("xmin", "xmax")])
  width_2km <- diff(calls$boxes[[2]][c("xmin", "xmax")])
  expect_gt(width_2km, width_1km)
  expect_equal(unname(width_2km - width_1km), 2000 / 111320,
               tolerance = 0.005)
})

test_that("load_osm_data reuses a fresh persistent cache entry", {
  reference <- raster::raster(
    nrows = 1, ncols = 1,
    xmn = -48.55, xmx = -48.54, ymn = -23.61, ymx = -23.60,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, 1)
  cache_dir <- tempfile("biomastats-osm-cache-")
  old_options <- options(biomastats.osm_cache_dir = cache_dir)
  on.exit(options(old_options), add = TRUE)
  calls <- 0L

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(...) {
      calls <<- calls + 1L
      list(osm_points = NULL, osm_lines = "cached", osm_polygons = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) {
      expect_identical(osm_data$osm_lines, "cached")
      raster::setValues(reference_raster, 1)
    },
    .package = "biomastats"
  )

  load_osm_data(reference, "highway", use_cache = TRUE)
  load_osm_data(reference, "highway", use_cache = TRUE)

  expect_equal(calls, 1L)
  expect_length(list.files(cache_dir, pattern = "[.]rds$"), 1L)
})

test_that("load_osm_data fails over between Overpass endpoints", {
  reference <- raster::raster(
    nrows = 1, ncols = 1,
    xmn = -48.55, xmx = -48.54, ymn = -23.61, ymx = -23.60,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, 1)
  attempted <- character()

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(..., endpoint) {
      attempted <<- c(attempted, endpoint)
      if (length(attempted) == 1L) stop("endpoint unavailable")
      list(osm_points = "available", osm_lines = NULL, osm_polygons = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) {
      raster::setValues(reference_raster, 1)
    },
    .package = "biomastats"
  )

  result <- load_osm_data(
    reference,
    "highway",
    overpass_urls = c(
      "https://first.example/api/interpreter",
      "https://second.example/api/interpreter"
    ),
    use_cache = FALSE
  )

  expect_s4_class(result, "RasterLayer")
  expect_equal(
    attempted,
    c(
      "https://first.example/api/interpreter",
      "https://second.example/api/interpreter"
    )
  )
})

test_that("load_osm_data uses expired cache when every endpoint fails", {
  reference <- raster::raster(
    nrows = 1, ncols = 1,
    xmn = -48.55, xmx = -48.54, ymn = -23.61, ymx = -23.60,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, 1)
  cache_dir <- tempfile("biomastats-stale-osm-cache-")
  old_options <- options(biomastats.osm_cache_dir = cache_dir)
  on.exit(options(old_options), add = TRUE)

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(...) {
      list(osm_points = "stale", osm_lines = NULL, osm_polygons = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) {
      expect_identical(osm_data$osm_points, "stale")
      raster::setValues(reference_raster, 1)
    },
    .package = "biomastats"
  )
  load_osm_data(reference, "amenity", use_cache = TRUE)

  cache_file <- list.files(cache_dir, full.names = TRUE, pattern = "[.]rds$")
  record <- readRDS(cache_file)
  record$fetched_at <- Sys.time() - 30 * 24 * 60 * 60
  saveRDS(record, cache_file)

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(...) stop("network unavailable"),
    .package = "biomastats"
  )
  expect_warning(
    result <- load_osm_data(
      reference,
      "amenity",
      use_cache = TRUE,
      cache_ttl_days = 7,
      overpass_urls = "https://offline.example/api/interpreter"
    ),
    "expired OSM cache"
  )
  expect_s4_class(result, "RasterLayer")
})

test_that("load_osm_data accepts data recovered by the XML fallback", {
  reference <- raster::raster(
    nrows = 2, ncols = 2,
    xmn = 0, xmx = 2, ymn = 0, ymx = 2,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, rep(1, 4))
  calls <- new.env(parent = emptyenv())

  fallback_data <- list(
    osm_points = NULL,
    osm_lines = structure(list(), class = "mock_osm_lines"),
    osm_polygons = NULL
  )

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(...) {
      calls$fallback_used <- TRUE
      fallback_data
    },
    rasterize_osm = function(osm_data, reference_raster) {
      calls$osm_data <- osm_data
      raster::setValues(reference_raster, c(1, NA, NA, NA))
    },
    .package = "biomastats"
  )

  result <- load_osm_data(reference, key_feature = "highway")

  expect_s4_class(result, "RasterLayer")
  expect_true(calls$fallback_used)
  expect_identical(calls$osm_data, fallback_data)
  expect_equal(raster::getValues(result), c(0, NA, NA, NA))
})

test_that("load_osm_data trims nodata margins before querying OSM", {
  reference <- raster::raster(
    nrows = 4, ncols = 4,
    xmn = 0, xmx = 4, ymn = 0, ymx = 4,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(
    reference,
    c(NA, NA, NA, NA, NA, 1, 1, NA, NA, 1, 1, NA, NA, NA, NA, NA)
  )
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(bbox, ...) {
      calls$bbox <- bbox
      list(osm_points = NULL, osm_lines = NULL, osm_polygons = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) reference_raster,
    .package = "biomastats"
  )

  load_osm_data(reference, key_feature = "highway")

  expect_true(calls$bbox[["xmin"]] < 1)
  expect_true(calls$bbox[["ymin"]] < 1)
  expect_true(calls$bbox[["xmax"]] > 3)
  expect_true(calls$bbox[["ymax"]] > 3)
  expect_equal(mean(calls$bbox[c("xmin", "xmax")]), 2, tolerance = 0.01)
})

test_that("load_osm_data converts a projected extent to WGS84", {
  reference <- raster::raster(
    nrows = 2, ncols = 2,
    xmn = 500000, xmx = 501000,
    ymn = 7400000, ymx = 7401000,
    crs = "+proj=utm +zone=23 +south +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 4))
  calls <- new.env(parent = emptyenv())

  testthat::local_mocked_bindings(
    fetch_osm_feature = function(bbox, ...) {
      calls$bbox <- bbox
      list(osm_points = NULL)
    },
    rasterize_osm = function(osm_data, reference_raster) {
      raster::setValues(reference_raster, c(0, NA, NA, NA))
    },
    .package = "biomastats"
  )

  load_osm_data(reference, "highway", "primary")

  expect_true(all(calls$bbox[c("xmin", "xmax")] > -180))
  expect_true(all(calls$bbox[c("xmin", "xmax")] < 180))
  expect_true(all(calls$bbox[c("ymin", "ymax")] > -90))
  expect_true(all(calls$bbox[c("ymin", "ymax")] < 90))
  expect_equal(mean(calls$bbox[c("xmin", "xmax")]), -45, tolerance = 0.02)
})

test_that("rasterize_osm combines available OSM geometries", {
  reference <- raster::raster(
    nrows = 4, ncols = 4,
    xmn = 0, xmx = 4, ymn = 0, ymx = 4,
    crs = "+proj=longlat +datum=WGS84"
  )
  line <- sf::st_sfc(
    sf::st_linestring(matrix(c(0, 0, 4, 4), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  polygon <- sf::st_sfc(
    sf::st_polygon(list(matrix(c(1, 1, 3, 1, 3, 3, 1, 3, 1, 1),
                               ncol = 2, byrow = TRUE))),
    crs = 4326
  )
  points <- sf::st_sfc(sf::st_point(c(0.5, 3.5)), crs = 4326)
  osm_data <- list(
    osm_points = sf::st_sf(id = 1, geometry = points),
    osm_lines = sf::st_sf(id = 1, geometry = line),
    osm_polygons = sf::st_sf(id = 1, geometry = polygon)
  )

  result <- rasterize_osm(osm_data, reference)

  expect_s4_class(result, "Raster")
  expect_equal(raster::extent(result), raster::extent(reference))
  expect_true(any(!is.na(raster::getValues(result))))
})

test_that("rasterize_osm supports OSM multiline and multipolygon objects", {
  reference <- raster::raster(
    nrows = 4, ncols = 4,
    xmn = 0, xmx = 4, ymn = 0, ymx = 4,
    crs = "+proj=longlat +datum=WGS84"
  )
  multiline <- sf::st_sfc(
    sf::st_multilinestring(list(
      matrix(c(0, 0, 4, 4), ncol = 2, byrow = TRUE)
    )),
    crs = 4326
  )
  multipolygon <- sf::st_sfc(
    sf::st_multipolygon(list(list(
      matrix(c(1, 1, 3, 1, 3, 3, 1, 3, 1, 1),
             ncol = 2, byrow = TRUE)
    ))),
    crs = 4326
  )
  osm_data <- list(
    osm_points = NULL,
    osm_lines = NULL,
    osm_multilines = sf::st_sf(id = 1, geometry = multiline),
    osm_polygons = NULL,
    osm_multipolygons = sf::st_sf(id = 1, geometry = multipolygon)
  )

  result <- rasterize_osm(osm_data, reference)

  expect_s4_class(result, "RasterLayer")
  expect_true(any(!is.na(raster::getValues(result))))
})

test_that("rasterize_osm transforms and clips OSM geometries to projected rasters", {
  reference <- raster::raster(
    nrows = 10, ncols = 10,
    xmn = 500000, xmx = 501000,
    ymn = 7400000, ymx = 7401000,
    crs = "+proj=utm +zone=23 +south +datum=WGS84 +units=m"
  )
  centre <- sf::st_transform(
    sf::st_sfc(sf::st_point(c(500500, 7400500)), crs = 31983),
    4326
  )
  centre_coordinates <- sf::st_coordinates(centre)[1, ]
  line <- sf::st_sfc(
    sf::st_linestring(rbind(
      centre_coordinates + c(-0.004, 0),
      centre_coordinates + c(0.004, 0)
    )),
    crs = 4326
  )
  outside_line <- sf::st_sfc(
    sf::st_linestring(matrix(c(-40, -10, -40, -9), ncol = 2, byrow = TRUE)),
    crs = 4326
  )
  osm_data <- list(
    osm_points = NULL,
    osm_lines = sf::st_sf(
      id = c(1, 2),
      geometry = c(line, outside_line)
    ),
    osm_polygons = NULL
  )

  result <- rasterize_osm(osm_data, reference)

  expect_s4_class(result, "RasterLayer")
  expect_true(any(!is.na(raster::getValues(result))))
})

test_that("load_osm_data validates its public arguments", {
  reference <- raster::raster(nrows = 1, ncols = 1)

  expect_error(load_osm_data(NULL, "highway"), "reference_raster")
  expect_error(load_osm_data(reference, ""), "key_feature")
  expect_error(load_osm_data(reference, "highway", value_feature = ""),
               "value_feature")
  expect_error(load_osm_data(reference, "highway", provider = ""),
               "provider")
  expect_error(load_osm_data(reference, "highway", provider = "other"),
               "Only provider")
  expect_error(load_osm_data(reference, "highway", buffer_distance = -1),
               "buffer_distance")
  expect_error(load_osm_data(reference, "highway", timeout = 0), "timeout")
  expect_error(load_osm_data(reference, "highway", overpass_urls = "ftp://x"),
               "overpass_urls")
  expect_error(load_osm_data(reference, "highway", use_cache = NA),
               "use_cache")
  expect_error(load_osm_data(reference, "highway", cache_ttl_days = -1),
               "cache_ttl_days")
})

test_that("distance_to_feature calculates distances and preserves the mask", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = 0, xmx = 3, ymn = 0, ymx = 3,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, c(1, 1, NA, 1, 1, 1, 1, 1, 1))
  feature <- raster::setValues(reference, c(NA, NA, NA, NA, 0, NA, NA, NA, NA))
  calls <- new.env(parent = emptyenv())

  mock_load_osm_data <- function(reference_raster, key_feature,
                                 value_feature = NULL, provider = "osm",
                                 buffer_distance = 2000) {
    calls$key <- key_feature
    calls$value <- value_feature
    calls$provider <- provider
    feature
  }

  testthat::local_mocked_bindings(
    load_osm_data = mock_load_osm_data,
    .package = "biomastats"
  )

  result <- distance_to_feature(
    reference,
    key_feature = "highway",
    value_feature = "primary"
  )

  expect_named(result, c("raster", "plot"))
  expect_s4_class(result$raster, "RasterLayer")
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$plot$labels$x, "Easting")
  expect_equal(result$plot$labels$y, "Northing")
  expect_match(result$plot$labels$title, "highway = primary", fixed = TRUE)
  expect_equal(calls$key, "highway")
  expect_equal(calls$value, "primary")
  expect_equal(calls$provider, "osm")
  expect_equal(
    raster::getValues(result$raster),
    c(sqrt(2), 1, NA, 1, 0, 1, sqrt(2), 1, sqrt(2)),
    tolerance = 1e-6
  )
})

test_that("distance_to_feature accepts an annual biomastats raster", {
  mapas <- example_maps()
  ano <- mapas[["time_range"]][1]
  indice_ano <- ano - mapas[["time_range"]][1] + 1
  raster_anual <- mapas[["raster"]][[indice_ano]]
  reference_values <- raster::getValues(raster_anual)
  valid_cells <- which(!is.na(reference_values))

  expect_true(length(valid_cells) > 0)
  expect_s4_class(raster_anual, "RasterLayer")

  feature_values <- rep(NA_real_, length(reference_values))
  feature_values[valid_cells[1]] <- 0
  feature_raster <- raster::setValues(raster_anual, feature_values)
  calls <- new.env(parent = emptyenv())

  mock_load_osm_data <- function(reference_raster, key_feature,
                                 value_feature = NULL, provider = "osm",
                                 buffer_distance = 2000) {
    calls$reference_raster <- reference_raster
    calls$key <- key_feature
    calls$value <- value_feature
    feature_raster
  }

  testthat::local_mocked_bindings(
    load_osm_data = mock_load_osm_data,
    .package = "biomastats"
  )

  result <- distance_to_feature(
    reference_raster = raster_anual,
    key_feature = "highway",
    value_feature = NULL
  )

  expect_named(result, c("raster", "plot"))
  expect_s4_class(result$raster, "RasterLayer")
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$plot$labels$x, "Longitude")
  expect_equal(result$plot$labels$y, "Latitude")
  expect_equal(calls$key, "highway")
  expect_null(calls$value)
  expect_equal(
    raster::extent(calls$reference_raster),
    raster::extent(raster_anual)
  )
  expect_equal(raster::res(result$raster), raster::res(raster_anual))
  expect_equal(
    raster::projection(result$raster),
    raster::projection(raster_anual)
  )
  expect_true(all(is.na(raster::getValues(result$raster)[
    is.na(reference_values)
  ])))
  expect_equal(raster::getValues(result$raster)[valid_cells[1]], 0)
})

test_that("distance_to_feature returns a customizable and exportable map", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = -48.6, xmx = -48.5,
    ymn = -23.7, ymx = -23.6,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, rep(1, 9))
  feature <- raster::setValues(
    reference,
    c(NA, NA, NA, NA, 0, NA, NA, NA, NA)
  )

  testthat::local_mocked_bindings(
    load_osm_data = function(...) feature,
    .package = "biomastats"
  )

  result <- distance_to_feature(reference, "highway")
  customized <- result$plot + ggplot2::labs(
    title = "Distance to roads in 2024"
  )
  output_file <- tempfile(fileext = ".png")

  ggplot2::ggsave(
    filename = output_file,
    plot = customized,
    width = 6,
    height = 4,
    dpi = 96
  )

  expect_s3_class(customized, "ggplot")
  expect_equal(customized$labels$title, "Distance to roads in 2024")
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)
})

test_that("distance_to_feature validates CRS and occupied cells", {
  without_crs <- raster::raster(nrows = 2, ncols = 2)
  raster::projection(without_crs) <- NA_character_
  expect_error(
    distance_to_feature(without_crs, "highway", "primary"),
    "must have a CRS"
  )

  empty_reference <- raster::raster(
    nrows = 2, ncols = 2,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  empty_reference <- raster::setValues(empty_reference, rep(NA_real_, 4))
  expect_error(
    distance_to_feature(empty_reference, "highway", "primary"),
    "at least one valid cell"
  )

  reference <- raster::raster(
    nrows = 2, ncols = 2,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 4))
  empty_feature <- raster::setValues(reference, rep(NA_real_, 4))

  testthat::local_mocked_bindings(
    load_osm_data = function(...) empty_feature,
    .package = "biomastats"
  )

  expect_error(
    distance_to_feature(reference, "highway", "primary"),
    "contains no occupied cells"
  )
  expect_error(
    distance_to_feature(
      reference,
      "highway",
      osm_options = list(unsupported = TRUE)
    ),
    "Unsupported OSM option"
  )
})

test_that("density_of_feature calculates local and global coverage", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = 0, xmx = 3, ymn = 0, ymx = 3,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 9))
  feature <- raster::setValues(
    reference,
    c(NA, NA, NA, NA, 0, NA, NA, NA, NA)
  )

  calls <- new.env(parent = emptyenv())
  mock_load_osm_data <- function(reference_raster, key_feature,
                                 value_feature = NULL, provider = "osm",
                                 buffer_distance = 2000) {
    calls$key <- key_feature
    calls$value <- value_feature
    calls$provider <- provider
    feature
  }
  testthat::local_mocked_bindings(
    load_osm_data = mock_load_osm_data,
    .package = "biomastats"
  )

  result <- density_of_feature(
    reference,
    key_feature = "highway",
    value_feature = "primary",
    window_size = 3,
    window_shape = "square"
  )

  expect_named(result, c("raster", "plot", "global"))
  expect_s4_class(result$raster, "RasterLayer")
  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$global, 1 / 9)
  expect_equal(
    raster::getValues(result$raster),
    c(1 / 4, 1 / 6, 1 / 4, 1 / 6, 1 / 9, 1 / 6, 1 / 4, 1 / 6, 1 / 4),
    tolerance = 1e-6
  )
  expect_equal(result$plot$labels$y, "Northing")
  expect_match(result$plot$labels$title, "highway = primary", fixed = TRUE)
  expect_match(result$plot$labels$subtitle, "3 x 3 square", fixed = TRUE)
  expect_equal(calls$key, "highway")
  expect_equal(calls$value, "primary")
  expect_equal(calls$provider, "osm")
})

test_that("density_of_feature supports circular windows and export", {
  reference <- raster::raster(
    nrows = 5, ncols = 5,
    xmn = -48.6, xmx = -48.5, ymn = -23.7, ymx = -23.6,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, rep(1, 25))
  feature <- raster::setValues(reference, rep(NA_real_, 25))
  feature[13] <- 0

  testthat::local_mocked_bindings(
    load_osm_data = function(...) feature,
    .package = "biomastats"
  )

  result <- density_of_feature(reference, "waterway", window_size = 3,
                               window_shape = "circle")
  output_file <- tempfile(fileext = ".png")
  ggplot2::ggsave(output_file, result$plot, width = 6, height = 4, dpi = 96)

  expect_s3_class(result$plot, "ggplot")
  expect_equal(result$plot$labels$x, "Longitude")
  expect_equal(result$global, 1 / 25)
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)
})

test_that("density_of_feature returns zero when the feature is outside the mask", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = 0, xmx = 3, ymn = 0, ymx = 3,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(
    reference,
    c(NA, NA, NA, NA, 1, NA, NA, NA, NA)
  )
  feature <- raster::setValues(
    reference,
    c(0, NA, NA, NA, NA, NA, NA, NA, NA)
  )

  result <- density_of_feature(
    reference,
    key_feature = "waterway",
    feature_raster = feature,
    window_size = 3,
    plot = FALSE
  )

  expect_equal(result$global, 0)
  expect_equal(raster::getValues(result$raster)[5], 0)
  expect_null(result$plot)
})

test_that("density_of_feature validates the moving window", {
  reference <- raster::raster(
    nrows = 2, ncols = 2,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 4))

  expect_error(
    density_of_feature(reference, "highway", window_size = 2),
    "odd positive integer"
  )
  expect_error(
    density_of_feature(reference, "highway", window_size = 0),
    "odd positive integer"
  )
})

test_that("integrate_feature loads each feature once and reuses it", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = 0, xmx = 3, ymn = 0, ymx = 3,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 9))
  calls <- new.env(parent = emptyenv())
  calls$total <- 0L
  calls$keys <- character()

  mock_load_osm_data <- function(reference_raster, key_feature,
                                 value_feature = NULL, provider = "osm",
                                 buffer_distance = 2000) {
    calls$total <- calls$total + 1L
    calls$keys <- c(calls$keys, key_feature)
    values <- rep(NA_real_, raster::ncell(reference_raster))
    values[if (identical(key_feature, "highway")) 5 else 1] <- 0
    raster::setValues(reference_raster, values)
  }
  testthat::local_mocked_bindings(
    load_osm_data = mock_load_osm_data,
    .package = "biomastats"
  )

  result <- integrate_feature(
    reference,
    features = list(
      roads = list(key = "highway", value = "primary"),
      rivers = list(key = "waterway", value = "river")
    ),
    metrics = list(
      distance = list(),
      density = list(window_size = 3, window_shape = "circle")
    ),
    plot = FALSE
  )

  expect_s3_class(result, "biomastats_feature_integration")
  expect_named(result, c("features", "results", "plots"))
  expect_equal(calls$total, 2L)
  expect_equal(calls$keys, c("highway", "waterway"))
  expect_named(result$features, c("roads", "rivers"))
  expect_named(result$results$roads, c("distance", "density"))
  expect_s4_class(result$results$roads$distance$raster, "RasterLayer")
  expect_s4_class(result$results$roads$density$raster, "RasterLayer")
  expect_equal(result$results$roads$density$global, 1 / 9)
  expect_null(result$results$roads$distance$plot)
  expect_null(result$results$roads$density$plot)
  expect_null(result$plots)
})

test_that("integrate_feature returns named plots without reloading", {
  reference <- raster::raster(
    nrows = 3, ncols = 3,
    xmn = -48.6, xmx = -48.5, ymn = -23.7, ymx = -23.6,
    crs = "+proj=longlat +datum=WGS84"
  )
  reference <- raster::setValues(reference, rep(1, 9))
  feature <- raster::setValues(
    reference,
    c(NA, NA, NA, NA, 0, NA, NA, NA, NA)
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    load_osm_data = function(...) {
      calls <<- calls + 1L
      feature
    },
    .package = "biomastats"
  )

  result <- integrate_feature(
    reference,
    features = list(roads = list(key = "highway")),
    metrics = c("distance_to_feature", "density_of_feature"),
    plot = TRUE
  )

  expect_equal(calls, 1L)
  expect_named(result$plots, c("roads_distance", "roads_density"))
  expect_s3_class(result$plots$roads_distance, "ggplot")
  expect_s3_class(result$plots$roads_density, "ggplot")
})

test_that("integrate_feature validates feature and metric specifications", {
  reference <- raster::raster(
    nrows = 2, ncols = 2,
    crs = "+proj=utm +zone=23 +datum=WGS84 +units=m"
  )
  reference <- raster::setValues(reference, rep(1, 4))

  expect_error(integrate_feature(reference, list(), "distance"),
               "non-empty list")
  expect_error(
    integrate_feature(reference, list(roads = list(value = "primary"))),
    "must define"
  )
  expect_error(
    integrate_feature(reference, list(roads = "highway"), "unknown"),
    "Unsupported metric"
  )
  expect_error(
    integrate_feature(
      reference,
      list(roads = "highway"),
      list(density = list(window_size = 3, unsupported = TRUE))
    ),
    "Unsupported density option"
  )
  expect_error(
    integrate_feature(
      reference,
      list(roads = "highway"),
      osm_options = list(unsupported = TRUE)
    ),
    "Unsupported OSM option"
  )
})
