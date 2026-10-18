# Local-only test: downloads two MapBiomas tiles for Itapetininga - SP.
# Excluded from the built package (.Rbuildignore: tests/testthat/test-local-*),
# so GitHub Actions never runs it; skip_on_ci() is an extra safeguard.

itapetininga_shape <- function(dir) {
  url <- paste0("https://servicodados.ibge.gov.br/api/v3/malhas/municipios/3522307",
                "?formato=application/vnd.geo+json&qualidade=intermediaria")
  s <- tryCatch(sf::st_read(url, quiet = TRUE), error = function(e) NULL)
  if (is.null(s)) testthat::skip("Malha municipal do IBGE indispon\u00edvel.")
  path <- file.path(dir, "itapetininga.shp")
  sf::st_write(sf::st_transform(s, 4326), path, quiet = TRUE)
  path
}

test_that("load_rasters downloads and clips the two tiles of Itapetininga", {
  skip_on_ci()
  skip_on_cran()
  skip_if_offline("servicodados.ibge.gov.br")
  dir <- tempfile("itapetininga-"); dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  shape <- itapetininga_shape(dir)
  expect_equal(sort(unique(compare_shapefiles(shape))), c(93L, 94L))

  cache <- file.path(dir, "maps")
  mapas <- load_rasters(shape, start = 2023, end = 2024, method = "download",
                        export_folder_path = cache, collection = "10")
  expect_equal(mapas$time_range, c(2023, 2024))
  expect_length(mapas$raster, 2L)
  expect_length(list.files(cache, pattern = "frag-9[34]-year-202[34][.]tif$"), 4L)
  r <- terra::rast(mapas$raster[[1]])
  expect_true(terra::is.lonlat(r))
  bb <- sf::st_bbox(mapas$shape); e <- terra::ext(r)
  expect_true(e$xmin >= bb$xmin - 1e-3 && e$xmax <= bb$xmax + 1e-3)
  # The clip spans both tiles: the classified area covers the municipality.
  areas <- get_area(mapas)
  covered <- sum(areas$aggregate_data$area_raw[areas$aggregate_data$year == 2024], na.rm = TRUE)
  municipality <- as.numeric(sf::st_area(mapas$shape)) / 1e6
  expect_equal(covered, municipality, tolerance = .01)
  expect_equal(attr(areas, "metadata")$collection, 10)

  # New functions accept the downloaded object directly.
  expect_s3_class(class_diversity(areas, build_plot = FALSE), "biomastats_class_diversity")
  f <- fragment_metrics(mapas, classes = 3, years = 2024, vectorize = FALSE, lsm = NULL, build_plot = FALSE)
  expect_gt(f$summary$n_patches, 0)
  expect_equal(f$metadata$source[["2024"]]$utm$zone, "22")
})
