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
      "tile-manifest-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"fragment":', fragment,
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
      "tile-manifest-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"fragment":', fragment,
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
      "tile-manifest-type-cover-collection-7-fragment-",
      fragment,
      ".json"
    )
  )
  manifest_json <- paste0(
    '{"error":"","result":{"fragment":', fragment,
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
