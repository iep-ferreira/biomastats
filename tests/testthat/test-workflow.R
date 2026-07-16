test_that("study-area shapefiles are readable and valid", {
  ufs <- sf::read_sf(system.file("shp", "UFSCar.shp", package = "biomastats"))
  polygon <- sf::read_sf(system.file("shp", "polygon.shp", package = "biomastats"))

  expect_s3_class(ufs, "sf")
  expect_s3_class(polygon, "sf")
  expect_false(any(sf::st_is_empty(ufs)))
  expect_false(any(sf::st_is_empty(polygon)))
  expect_true(all(sf::st_is_valid(ufs)))
  expect_true(all(sf::st_is_valid(polygon)))
})

test_that("saved maps reproduce the analysis workflow", {
  mapas <- example_maps()

  expect_named(mapas, c("shape", "time_range", "raster", "collection"))
  expect_equal(mapas[["time_range"]], c(1985, 2021))
  expect_length(mapas[["raster"]], diff(mapas[["time_range"]]) + 1)
  expect_s3_class(mapas[["shape"]], "sf")

  areas <- biomastats::get_area(mapas, plot_type = "areaplot")
  expect_named(areas, c("Years", "Occupied area", "aggregate_data", "time_series", "collection"))
  expect_equal(range(areas[["Years"]]), mapas[["time_range"]])
  expect_true(nrow(areas[["aggregate_data"]]) > 0)
  expect_s3_class(areas[["time_series"]], "ggplot")

  expect_s3_class(biomastats::land_vis(mapas, year = 1990), "ggplot")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(biomastats::land_dist(areas, year = 2021, type = "barplot"), "ggplot")
  expect_s3_class(biomastats::land_dist(areas, year = 2021, type = "pie"), "ggplot")
})

test_that("landscape metrics and reclassified maps use saved maps", {
  mapas <- example_maps()

  metrics <- biomastats::biomastats_metrics(
    mapas,
    start = 1985,
    end = 1986,
    zone = "22",
    hemisphere = "south",
    classes = c(3, 4),
    metrics = "aggregation_index"
  )

  expect_named(metrics, c(
    "classes", "ai_plot", "fragments_number", "area_plot",
    "edge_density_plot", "egde_lenght_plot", "frag_avg_area",
    "metrics", "metrics_table", "rec_map", "start", "end"
  ))
  expect_equal(metrics[["start"]], 1985)
  expect_equal(metrics[["end"]], 1986)
  expect_equal(nrow(metrics[["metrics_table"]]), 2)
  expect_s3_class(biomastats::reclass_map(metrics, year = 1985), "ggplot")
})
