test_that("biomastats is installed and loadable", {
  expect_true(nzchar(system.file(package = "biomastats")))
  expect_true(requireNamespace("biomastats", quietly = TRUE))
  expect_equal(as.character(utils::packageVersion("biomastats")), "2.0.0")
  expect_true(utils::packageVersion("osmdata") >= package_version("0.4.0"))
  expect_true(is.function(biomastats::get_area))
  expect_true(is.function(biomastats::land_vis))
  expect_true(is.function(biomastats::density_of_feature))
  expect_true(is.function(biomastats::integrate_feature))
})

test_that("package examples are installed", {
  expect_true(file.exists(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")))
  expect_true(file.exists(system.file("shp", "UFSCar.shp", package = "biomastats")))
  expect_true(file.exists(system.file("shp", "polygon.shp", package = "biomastats")))
  expect_true(file.exists(system.file("api-servers.json", package = "biomastats")))
})
