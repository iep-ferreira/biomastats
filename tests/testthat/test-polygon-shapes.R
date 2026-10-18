# make_polygon() writes polygon.shp under system.file("shp"), which under
# load_all() is inst/shp/polygon.shp in the repository. It passes size to these
# shape builders unchanged, so they are tested directly.
ground_area_km2 <- function(shape) {
  as.numeric(sf::st_area(sf::st_set_crs(shape, 4326))) / 1e6
}

extent_km <- function(shape, lat, lon) {
  box <- sf::st_bbox(shape)
  c(geosphere::distGeo(c(lon, box[["ymin"]]), c(lon, box[["ymax"]])),
    geosphere::distGeo(c(box[["xmin"]], lat), c(box[["xmax"]], lat))) / 1000
}

test_that("size is the total width of the shape on the ground at any latitude", {
  # Regression: size went through a cos(lat) factor and became the same radius
  # in degrees of latitude and longitude. A 2 km circle in Liechtenstein came
  # out ~2.7 km north-south by ~1.8 km east-west.
  for (point in list(c(lat = -23.605, lon = -48.529), c(lat = 47.16, lon = 9.555))) {
    lat <- point[["lat"]]
    lon <- point[["lon"]]
    circle <- circle_coords(lat, lon, 2)
    square <- square_coords(lat, lon, 2)
    hexagon <- hexagon_coords(lat, lon, 2.5)
    expect_equal(extent_km(circle, lat, lon), c(2, 2), tolerance = 0.01)
    expect_equal(extent_km(square, lat, lon), c(2, 2), tolerance = 0.01)
    expect_equal(extent_km(hexagon, lat, lon), c(2.5 * sqrt(3) / 2, 2.5), tolerance = 0.01)
    expect_equal(ground_area_km2(circle), pi, tolerance = 0.01)
    expect_equal(ground_area_km2(square), 4, tolerance = 0.01)
    expect_equal(ground_area_km2(hexagon), 3 * sqrt(3) / 8 * 2.5^2, tolerance = 0.01)
  }
})

test_that("shapes are centred on the point, with vertices at a geodesic distance", {
  lat <- 47.16
  lon <- 9.555
  for (builder in list(circle_coords, square_coords, hexagon_coords)) {
    shape <- builder(lat, lon, 2)
    expect_equal(as.numeric(sf::st_coordinates(sf::st_centroid(shape))), c(lon, lat),
      tolerance = 1e-6)
  }
  radius <- function(shape) {
    vertices <- sf::st_coordinates(shape)[, c("X", "Y")]
    geosphere::distGeo(c(lon, lat), vertices[-nrow(vertices), ])
  }
  expect_equal(radius(circle_coords(lat, lon, 2)), rep(1000, 100), tolerance = 1e-6)
  expect_equal(radius(square_coords(lat, lon, 2)), rep(1000 * sqrt(2), 4), tolerance = 1e-6)
  expect_equal(radius(hexagon_coords(lat, lon, 2.5)), rep(1250, 6), tolerance = 1e-6)
})
