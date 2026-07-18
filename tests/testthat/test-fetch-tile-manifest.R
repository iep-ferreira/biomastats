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
