test_that("biomastats is installed and loadable", {
  expect_true(nzchar(system.file(package = "biomastats")))
  expect_true(requireNamespace("biomastats", quietly = TRUE))
  expect_equal(as.character(utils::packageVersion("biomastats")), "2.1.0.9000")
})

# Public interface: every exported function, by module. A function added with
# @export (or removed) must be listed here, so changes to the API are explicit.
test_that("NAMESPACE exports exactly the public functions", {
  public <- c(
    # acquisition and preparation
    "load_rasters", "load_rasters_from_folder", "check_maps", "download_maps",
    "download_mapbiomas", "fetch_tile_manifest", "compare_shapefiles", "get_api_values",
    "post_to_biomastats", "download_public_gdrive_file", "get_gdrive_filename",
    "make_polygon", "circle_coords", "square_coords", "hexagon_coords",
    # areas, maps and dictionaries
    "get_area", "land_vis", "land_dist", "dict_build", "raster_index",
    # landscape and fragment metrics
    "lulc_metrics", "biomastats_metrics", "reclass_map", "fragment_metrics", "class_diversity",
    # change, adjacency, vectorization and connectivity
    "land_change", "land_to_graph", "reclass_to_polygons", "land_connectivity", "patch_imp_fast",
    # resistance, least-cost distance, temporal connectivity and Makurhini
    "resistance_surface", "cost_distance", "connectivity_change", "as_makurhini",
    "connectivity_context",
    # local collections of any origin and their dictionaries
    "load_local_rasters", "class_dictionary", "raster_catalog",
    # time series
    "land_time_series", "series_correlation", "series_fit_regime", "series_diagnostics",
    "series_forecast",
    # external layers (OpenStreetMap through regional extracts, not Overpass)
    "layer_catalog", "load_lulc", "load_features", "load_physical", "feature_raster",
    "distance_to_nearest")
  exported <- getNamespaceExports("biomastats")
  expect_setequal(exported, public)
  expect_true(all(vapply(public, function(f) is.function(getExportedValue("biomastats", f)), logical(1))))
})

test_that("package examples are installed", {
  expect_true(file.exists(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")))
  expect_true(file.exists(system.file("shp", "UFSCar.shp", package = "biomastats")))
  expect_true(file.exists(system.file("shp", "polygon.shp", package = "biomastats")))
  expect_true(file.exists(system.file("api-servers.json", package = "biomastats")))
})
