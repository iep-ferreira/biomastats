change_maps <- function() {
  r <- raster::raster(nrows = 2, ncols = 3, xmn = 500000, xmx = 500030,
    ymn = 7400000, ymx = 7400020, crs = "EPSG:32722")
  values <- list(c(3, 3, 4, 4, 3, 4), c(3, 4, 4, 3, 3, 3), c(3, 3, 4, 3, 4, 15))
  list(raster = lapply(values, function(v) raster::setValues(r, v)),
       time_range = c(1985, 1987), collection = "10")
}

# Areas come from terra::cellSize(), as in get_area(). A 10 m cell on the UTM
# central meridian covers 0.01 ha divided by the squared scale factor 0.9996,
# with ~1e-8 noise between cells, hence the tolerance.
change_cell_ha <- 0.01 / 0.9996^2
expect_ha <- function(object, cells) {
  expect_equal(object, cells * change_cell_ha, tolerance = 1e-6)
}

test_that("bipartite transitions conserve pixels and quantify gains and losses", {
  maps <- change_maps()
  result <- land_change(maps, years = c(1985, 1986), build_plot = FALSE)
  expect_s3_class(result, "biomastats_land_change")
  expect_true(igraph::is_bipartite(result$graph))
  expect_equal(result$metadata$collection, 10)
  expect_equal(result$years, c(1985, 1986))
  m <- result$matrices[[1]]
  expect_equal(unname(m$pixels), matrix(c(2, 2, 1, 1), 2))
  expect_ha(m$area_ha, m$pixels)
  expect_equal(unname(m$probability), matrix(c(2/3, 2/3, 1/3, 1/3), 2), tolerance = 1e-6)
  expect_ha(result$class_metrics$gain_ha, c(2, 1))
  expect_ha(result$class_metrics$loss_ha, c(1, 2))
  expect_ha(result$class_metrics$net_ha, c(1, -1))
  expect_ha(result$interval_metrics$total_ha, 6)
  expect_ha(result$interval_metrics$persistence_ha, 3)
  expect_ha(result$interval_metrics$change_ha, 3)
  expect_ha(result$interval_metrics$quantity_ha, 1)
  expect_ha(result$interval_metrics$allocation_ha, 2)
  expect_equal(sum(result$transitions$share), 1)
  expect_equal(sum(result$trajectory_metrics$share), 1)
  expect_null(result$plot)
  expect_null(result$change_plot)
  expect_identical(maps$collection, "10")
})

test_that("tripartite trajectories preserve observed paths and all comparisons", {
  result <- land_change(change_maps(), build_plot = FALSE)
  expect_named(result$matrices, c("1985_1986", "1986_1987", "1985_1987"))
  expect_equal(result$trajectory_metrics$pixels, c(2, 2, 1, 1))
  expect_ha(result$trajectory_metrics$area_ha, c(2, 2, 1, 1))
  expect_equal(result$trajectories$trajectory_id, seq_len(6))
  expect_equal(sum(result$trajectories$pixels), 6)
  expect_ha(sum(result$trajectories$area_ha), 6)
  expect_true(all(is.na(result$matrices[[1]]$probability["15", ])))
  expect_equal(rowSums(result$matrices[[1]]$probability)[c("3", "4")], c("3" = 1, "4" = 1))
  expect_true(igraph::is_directed(result$graph))
  expect_equal(sort(unique(igraph::vertex_attr(result$graph, "partition"))), 1:3)
  nodes <- result$node_metrics
  edges <- igraph::as_data_frame(result$graph, what = "edges")
  for (name in nodes$name[nodes$year == 1986 & nodes$area_ha > 0]) {
    expect_equal(sum(edges$area_ha[edges$to == name]), sum(edges$area_ha[edges$from == name]))
  }
  expect_true(all(edges$end_year - edges$start_year == 1))
  expect_ha(sum(edges$area_ha), 12)
  expect_equal(sum(result$class_metrics$gross_change_ha), 2 * sum(result$interval_metrics$change_ha))
  subset <- land_change(change_maps(), years = c(1987, 1986), build_plot = FALSE)
  expect_equal(subset$years, c(1986, 1987))
  expect_equal(subset$matrices[[1]]$pixels, result$matrices[[2]]$pixels)
})

test_that("one complete-case mask is shared across all three moments", {
  maps <- change_maps()
  maps$raster[[2]][1] <- NA
  maps$raster[[3]][2] <- NA
  result <- land_change(maps, build_plot = FALSE)
  expect_equal(result$coverage$valid_pixels, c(6, 5, 5))
  expect_equal(result$coverage$common_pixels, rep(4, 3))
  expect_equal(result$coverage$excluded_valid_pixels, c(2, 1, 1))
  expect_ha(result$coverage$valid_area_ha, c(6, 5, 5))
  expect_ha(result$coverage$common_area_ha, rep(4, 3))
  expect_equal(result$interval_metrics$common_pixels, rep(4, 3))
  expect_ha(result$interval_metrics$total_ha, rep(4, 3))
  maps$raster[[1]][] <- NA
  expect_error(land_change(maps), "Nenhum pixel")
})

test_that("grid compatibility, selection and input validation are explicit", {
  maps <- change_maps()
  expect_error(land_change(NULL), "data")
  for (years in list(1985, c(1985, 1985), c(1985, 1988), c(1985, NA),
                    c(1985.5, 1986), c(1985, 1986, 1987, 1988))) {
    expect_error(land_change(maps, years), "years")
  }
  expect_error(land_change(maps, build_plot = NA), "build_plot")
  maps$time_range <- c(1985, 1988)
  expect_error(land_change(maps), "time_range")
  maps$raster[[4]] <- maps$raster[[3]]
  expect_error(land_change(maps), "years")
  maps <- change_maps()
  maps$raster[[2]] <- raster::shift(maps$raster[[2]], dx = 10)
  expect_error(land_change(maps), "mesma grade")
  maps <- change_maps()
  # Floating-point noise in the extent (years written to disk by raster) is the same grid.
  maps$raster[[2]] <- raster::shift(maps$raster[[2]], dx = 1e-9)
  expect_s3_class(land_change(maps, build_plot = FALSE), "biomastats_land_change")
  maps <- change_maps()
  raster::crs(maps$raster[[2]]) <- "EPSG:32723"
  expect_error(land_change(maps), "mesma grade")
  maps <- change_maps()
  maps$raster[[1]][1] <- 3.5
  expect_error(land_change(maps), "inteiros")
})

test_that("cell areas follow projected units or geographic latitude, as in get_area()", {
  # 10 x 20 US survey feet on the central meridian of UTM 22S (x = 500 km).
  r <- raster::raster(nrows = 1, ncols = 1, xmn = 1640416.67, xmx = 1640426.67,
    ymn = 24278000, ymx = 24278020, crs = "+proj=utm +zone=22 +south +datum=WGS84 +units=us-ft")
  maps <- list(raster = list(raster::setValues(r, 3), raster::setValues(r, 4)),
               time_range = c(2000, 2001), collection = 10)
  result <- land_change(maps, build_plot = FALSE)
  expect_equal(result$interval_metrics$total_ha, 200 * (1200 / 3937)^2 / 10000 / 0.9996^2,
    tolerance = 1e-4)
  expect_equal(result$interval_metrics$total_ha, 100 * get_area(maps)$aggregate_data$area_raw[1])
  expect_equal(result$interval_metrics$change_fraction, 1)
  r <- raster::raster(nrows = 2, ncols = 1, xmn = -49, xmx = -48.99,
    ymn = -24.01, ymx = -23.99, crs = "EPSG:4326")
  maps$raster <- list(raster::setValues(r, c(3, 4)), raster::setValues(r, c(4, 3)))
  result <- land_change(maps, build_plot = FALSE)
  cell <- sf::st_sfc(sf::st_polygon(list(rbind(c(-49, -23.99), c(-48.99, -23.99),
    c(-48.99, -24), c(-49, -24), c(-49, -23.99)))), crs = 4326)
  # s2 measures on a sphere; terra::cellSize on the WGS84 ellipsoid.
  expected <- as.numeric(units::set_units(sf::st_area(cell), "ha"))
  expect_equal(result$matrices[[1]]$area_ha["3", "4"], expected, tolerance = 0.005)
  expect_gt(result$matrices[[1]]$area_ha["3", "4"], result$matrices[[1]]$area_ha["4", "3"])
})

test_that("dictionary colours, reusable graphs and plots survive serialization", {
  maps <- change_maps()
  result <- land_change(maps, language = "en")
  numeric_only <- land_change(maps, language = "en", build_plot = FALSE)
  fields <- setdiff(names(result), c("plot", "change_plot", "graph"))
  expect_equal(result[fields], numeric_only[fields])
  dic <- dict_build(10)
  expect_equal(result$class_dictionary$color, dic$color[match(c(3, 4, 15), dic$code)])
  for (name in c("plot", "change_plot")) {
    expect_s3_class(result[[name]], "ggplot")
    built <- ggplot2::ggplot_build(result[[name]])
    scale <- built$plot$scales$get_scales("fill")
    expect_equal(unname(scale$map(c("3", "4", "15"))), result$class_dictionary$color)
    expect_false(any(grepl("#[0-9A-Fa-f]{6}", scale$get_labels())))
    expect_equal(as.character(scale$get_labels())[1], "3 Forest Formation")
  }
  edited <- result$plot + ggplot2::labs(title = "Edited after analysis")
  expect_equal(edited$labels$title, "Edited after analysis")
  file <- tempfile(fileext = ".rds")
  on.exit(unlink(file), add = TRUE)
  result$plot <- edited
  saveRDS(result, file)
  recovered <- readRDS(file)
  expect_equal(recovered$metadata, result$metadata)
  expect_equal(recovered$trajectories, result$trajectories)
  expect_equal(recovered$class_dictionary, result$class_dictionary)
  expect_equal(recovered$plot$labels$title, "Edited after analysis")
  expect_equal(igraph::as_data_frame(recovered$graph), igraph::as_data_frame(result$graph))
  maps$raster <- lapply(maps$raster, function(r) raster::setValues(r, rep(0, 6)))
  stable <- land_change(maps)
  expect_equal(stable$trajectory_metrics$pixels, c(6, 0, 0, 0))
  expect_equal(stable$class_dictionary$code, 0)
  expect_s3_class(ggplot2::ggplot_build(stable$plot), "ggplot_built")
  maps$raster[[3]][1] <- 999
  unknown <- land_change(maps, build_plot = FALSE)
  expect_false(unknown$class_dictionary$known[2])
  expect_equal(unknown$class_dictionary$color[2], "#999999")
})

test_that("bundled maps support the public two- and three-moment workflow", {
  maps <- example_maps()
  result <- land_change(maps, years = c(1985, 2000, 2021), build_plot = FALSE)
  expect_equal(result$years, c(1985, 2000, 2021))
  expect_equal(result$metadata$resolution, raster::res(maps$raster[[1]]))
  expect_equal(result$metadata$crs_wkt, sf::st_crs(maps$raster[[1]])$wkt)
  expect_equal(sum(result$trajectory_metrics$area_ha), result$interval_metrics$total_ha[1])
  expect_equal(result$interval_metrics$total_ha, rep(result$interval_metrics$total_ha[1], 3))
  expect_equal(igraph::vertex_attr(result$graph, "color"), result$node_metrics$color)
})

test_that("valid area is reported in hectares, not in pixels", {
  # Regression: colSums(!is.na(values) * area) negated the product and
  # returned the pixel count instead of the valid area.
  result <- land_change(example_maps(), years = c(1985, 2024), build_plot = FALSE)
  coverage <- result$coverage
  expect_equal(coverage$excluded_valid_pixels, c(0, 0))
  expect_equal(coverage$valid_area_ha, coverage$common_area_ha)
  expect_equal(coverage$valid_area_ha, rep(result$interval_metrics$total_ha, 2))
  expect_false(isTRUE(all.equal(coverage$valid_area_ha, coverage$valid_pixels)))
})

test_that("class areas match get_area() on the bundled maps", {
  # Regression: sf::st_area cell polygons gave 200.21 ha of forest formation in
  # 1985 against 199.74 ha in get_area().
  maps <- example_maps()
  result <- land_change(maps, years = c(1985, 2024), build_plot = FALSE)
  areas <- get_area(maps)$aggregate_data
  km2 <- function(year, codes) {
    areas$area_raw[match(paste(codes, year), paste(areas$land_class, areas$year))]
  }
  metrics <- result$class_metrics
  expect_false(anyNA(km2(1985, metrics$code)))
  expect_equal(metrics$initial_ha, 100 * km2(1985, metrics$code), tolerance = 1e-6)
  expect_equal(metrics$final_ha, 100 * km2(2024, metrics$code), tolerance = 1e-6)
  expect_equal(result$metadata$area_method, "terra::cellSize(unit = \"ha\", mask = FALSE), as in get_area()")
})
