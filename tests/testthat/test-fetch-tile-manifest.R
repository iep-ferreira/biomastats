test_that("fetch_tile_manifest validates its public arguments", {
  expect_error(
    biomastats::fetch_tile_manifest(fragment = 1, api_urls = "http://example.org"),
    "No valid API URL"
  )
  expect_error(
    biomastats::fetch_tile_manifest(fragment = 1, api_urls = "https://example.org", timeout = 0),
    "positive number"
  )
  expect_error(
    biomastats::fetch_tile_manifest(api_urls = "https://example.org"),
    "fragment"
  )
})

test_that("fetch_tile_manifest reuses an exact valid JSON cache entry", {
  fragment <- "910001"
  cache_dir <- file.path(
    tools::R_user_dir("biomastats", which = "cache"),
    "manifests"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(
    cache_dir,
    paste0(
      "tile-manifest-v3-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"collection":7,"fragment":', fragment,
    ',"version":"test-v1","years":{}}}'
  )
  writeBin(charToRaw(manifest_json), cache_file)

  cached <- biomastats::fetch_tile_manifest(
    type = "cover",
    collection = "7",
    fragment = fragment,
    use_cache = TRUE,
    api_urls = "not-a-url"
  )

  expect_identical(cached, manifest_json)
})

test_that("cache keys separate collection and cache reads can be bypassed", {
  expect_error(
    biomastats::fetch_tile_manifest(
      type = "cover",
      collection = "8",
      fragment = "910001",
      use_cache = TRUE,
      api_urls = "not-a-url"
    ),
    "No valid API URL"
  )
  expect_error(
    biomastats::fetch_tile_manifest(
      type = "cover",
      collection = "7",
      fragment = "910001",
      use_cache = FALSE,
      api_urls = "not-a-url"
    ),
    "No valid API URL"
  )
})

test_that("manifest cache entries expire after seven days", {
  fragment <- "910002"
  cache_dir <- file.path(
    tools::R_user_dir("biomastats", which = "cache"),
    "manifests"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(
    cache_dir,
    paste0(
      "tile-manifest-v3-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"collection":7,"fragment":', fragment,
    ',"version":"test-v1","years":{}}}'
  )
  writeBin(charToRaw(manifest_json), cache_file)
  Sys.setFileTime(cache_file, Sys.time() - (8 * 24 * 60 * 60))

  expect_error(
    biomastats::fetch_tile_manifest(
      type = "cover",
      collection = "7",
      fragment = fragment,
      use_cache = TRUE,
      api_urls = "not-a-url"
    ),
    "No valid API URL"
  )
  expect_false(file.exists(cache_file))
})

test_that("download_maps selects one cached record for the requested year", {
  fragment <- "910003"
  cache_dir <- file.path(
    tools::R_user_dir("biomastats", which = "cache"),
    "manifests"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  cache_file <- file.path(
    cache_dir,
    paste0(
      "tile-manifest-v3-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"collection":7,"fragment":', fragment,
    ',"years":{"1985":[',
    '{"shareable_link":"https://example.org/a","file_name":"a.tif"},',
    '{"shareable_link":"https://example.org/b","file_name":"b.tif"}',
    ']}}}'
  )
  writeBin(charToRaw(manifest_json), cache_file)

  export_dir <- file.path(tempdir(), "biomastats-download-maps-test")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  candidate_paths <- file.path(export_dir, c("a.tif", "b.tif"))
  writeBin(as.raw(1L), candidate_paths[[1L]])
  writeBin(as.raw(2L), candidate_paths[[2L]])

  downloaded <- biomastats::download_maps(
    fragment = fragment,
    type = "cover",
    collection = "7",
    year = 1985,
    export_folder_path = export_dir
  )

  expect_true(downloaded %in% candidate_paths)
})

test_that("download_maps skips manifests when the local raster exists", {
  fragment <- "910004"
  year <- 1985
  export_dir <- file.path(tempdir(), "biomastats-local-raster-test")
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  local_path <- file.path(
    export_dir,
    paste0("coverage-collection-7-frag-", fragment, "-year-", year, ".tif")
  )
  writeBin(as.raw(1L), local_path)

  testthat::local_mocked_bindings(
    fetch_tile_manifest = function(...) stop("The manifest was requested."),
    .package = "biomastats"
  )

  downloaded <- biomastats::download_maps(
    fragment = fragment,
    type = "cover",
    collection = "7",
    year = year,
    export_folder_path = export_dir
  )

  expect_identical(downloaded, local_path)
})

test_that("download_maps fetches one manifest for multiple missing years", {
  fragment <- "910005"
  years <- 1985:1987
  export_dir <- tempfile("biomastats-multiple-years-")
  dir.create(export_dir, recursive = TRUE)
  existing_file <- file.path(
    export_dir,
    paste0("coverage-collection-7-frag-", fragment, "-year-1985.tif")
  )
  writeBin(as.raw(1L), existing_file)
  calls <- new.env(parent = emptyenv())
  calls$manifest <- 0L
  calls$download <- 0L
  manifest_json <- paste0(
    '{"error":"","result":{"fragment":', fragment, ',"years":{',
    '"1985":[{"shareable_link":"https://example.org/1985",',
    '"file_name":"coverage-collection-7-frag-', fragment, '-year-1985.tif"}],',
    '"1986":[{"shareable_link":"https://example.org/1986",',
    '"file_name":"coverage-collection-7-frag-', fragment, '-year-1986.tif"}],',
    '"1987":[{"shareable_link":"https://example.org/1987",',
    '"file_name":"coverage-collection-7-frag-', fragment, '-year-1987.tif"}]}}}'
  )

  testthat::local_mocked_bindings(
    fetch_tile_manifest = function(...) {
      calls$manifest <- calls$manifest + 1L
      manifest_json
    },
    download_public_gdrive_file = function(file_link, local_path) {
      calls$download <- calls$download + 1L
      writeBin(charToRaw(file_link), local_path)
      invisible(local_path)
    },
    .package = "biomastats"
  )

  downloaded <- biomastats::download_maps(
    fragment = fragment,
    type = "cover",
    collection = "7",
    year = years,
    export_folder_path = export_dir
  )

  expect_identical(calls$manifest, 1L)
  expect_identical(calls$download, 2L)
  expect_identical(
    basename(downloaded),
    paste0("coverage-collection-7-frag-", fragment, "-year-", years, ".tif")
  )
  expect_true(all(file.exists(downloaded)))
})

test_that("check_maps resolves downloads before loading rasters", {
  ids <- c(920001, 920002)
  years <- 1985:1986
  export_dir <- tempfile("biomastats-check-maps-")
  dir.create(export_dir, recursive = TRUE)

  write_test_raster <- function(fragment, year) {
    path <- file.path(
      export_dir,
      paste0("coverage-collection-7-frag-", fragment, "-year-", year, ".tif")
    )
    map <- raster::raster(
      nrows = 2, ncols = 2,
      xmn = fragment, xmx = fragment + 1,
      ymn = 0, ymx = 1
    )
    raster::values(map) <- year
    raster::writeRaster(map, path, overwrite = TRUE)
    path
  }

  invisible(vapply(years, function(year) {
    write_test_raster(ids[[1L]], year)
  }, character(1L)))

  calls <- new.env(parent = emptyenv())
  calls$fragments <- numeric()
  calls$years <- list()

  testthat::local_mocked_bindings(
    download_maps = function(
        fragment, type, collection, year, export_folder_path) {
      calls$fragments <- c(calls$fragments, fragment)
      calls$years[[length(calls$years) + 1L]] <- year
      vapply(year, function(current_year) {
        write_test_raster(fragment, current_year)
      }, character(1L))
    },
    .package = "biomastats"
  )

  maps <- biomastats::check_maps(
    ids = ids,
    start = min(years),
    end = max(years),
    export_folder_path = export_dir,
    type = "cover",
    collection = "7"
  )

  expect_identical(calls$fragments, ids[[2L]])
  expect_identical(calls$years, list(years))
  expect_length(maps, length(years))
  expect_true(all(vapply(maps, methods::is, logical(1L), class2 = "RasterLayer")))
})
