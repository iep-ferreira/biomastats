adjacency_maps <- function(values = c(3, 4, 3, 4), nr = 2, nc = 2,
                           crs = "+proj=utm +zone=22 +south +datum=WGS84 +units=m") {
  r <- raster::raster(nrows = nr, ncols = nc, xmn = 0, xmx = nc * 10,
                      ymn = 0, ymx = nr * 20, crs = crs)
  list(raster = list(raster::setValues(r, values)), time_range = c(1985, 1985), collection = 10)
}

test_that("counts and boundaries count each shared side once", {
  mapas <- adjacency_maps()
  rede <- land_to_graph(mapas, 1985, build_plot = FALSE)
  expect_equal(unname(rede$adjacency), matrix(c(0, 2, 2, 0), 2))
  expect_equal(unname(rede$boundary_m), matrix(c(0, 40, 40, 0), 2))
  expect_equal(rede$node_metrics$code, c(3, 4))
  expect_equal(rede$node_metrics$pixels, c(2, 2))
  expect_equal(rede$node_metrics$degree, c(1, 1))
  expect_equal(rede$node_metrics$strength, c(2, 2))
  expect_equal(rede$node_metrics$homogeneity, c(0.5, 0.5))
  expect_equal(rede$node_metrics$neighbor_entropy, c(0, 0))
  expect_equal(rede$graph_metrics$total_adjacencies, 2)
  expect_equal(rede$graph_metrics$total_boundary_m, 40)
  expect_equal(rede$graph_metrics$interclass_fraction, 0.5)
  expect_equal(rede$graph_metrics$density, 1)
  expect_equal(rede$edge_metrics$share, 1)
  expect_null(rede$plot)
  expect_null(rede$graph_plot)
  expect_null(rede$heatmap)
  lengths <- land_to_graph(mapas, 1985, weight = "boundary", build_plot = FALSE)
  expect_equal(igraph::edge_attr(lengths$graph, "weight"), 40)
})

test_that("diagonals add contacts but no length and loops do not inflate metrics", {
  mapas <- adjacency_maps()
  rede <- land_to_graph(mapas, 1985, connectivity = 8, include_self = TRUE, build_plot = FALSE)
  expect_equal(unname(rede$adjacency), matrix(c(1, 4, 4, 1), 2))
  expect_equal(unname(rede$boundary_m), matrix(c(0, 40, 40, 0), 2))
  expect_equal(igraph::ecount(rede$graph), 3)
  expect_equal(rede$graph_metrics$edges_count, 1)
  expect_equal(rede$node_metrics$strength, c(4, 4))
  expect_equal(rede$graph_metrics$total_adjacencies, 6)
  diagonal <- adjacency_maps(c(3, NA, NA, 4))
  counts <- land_to_graph(diagonal, 1985, connectivity = 8, build_plot = FALSE)
  lengths <- land_to_graph(diagonal, 1985, connectivity = 8, weight = "boundary", build_plot = FALSE)
  expect_equal(igraph::ecount(counts$graph), 1)
  expect_equal(igraph::ecount(lengths$graph), 0)
  expect_equal(lengths$edge_metrics$boundary_m, 0)
  expect_equal(lengths$graph_metrics$connected_components, 2)
})

test_that("isolates, single rows and columns, and missing cells are retained", {
  single <- land_to_graph(adjacency_maps(3, 1, 1), 1985, build_plot = FALSE)
  expect_equal(dim(single$adjacency), c(1, 1))
  expect_equal(single$node_metrics$harmonic, 0)
  expect_equal(single$graph_metrics$density, 0)
  expect_true(is.na(single$node_metrics$homogeneity))
  separated <- land_to_graph(adjacency_maps(c(3, NA, 4), 1, 3), 1985, build_plot = FALSE)
  expect_equal(separated$node_metrics$code, c(3, 4))
  expect_equal(separated$node_metrics$degree, c(0, 0))
  expect_equal(separated$graph_metrics$connected_components, 2)
  column <- land_to_graph(adjacency_maps(c(3, 4), 2, 1), 1985, build_plot = FALSE)
  expect_equal(column$edge_metrics$boundary_m, 10)
  row <- land_to_graph(adjacency_maps(c(3, 4), 1, 2), 1985, build_plot = FALSE)
  expect_equal(row$edge_metrics$boundary_m, 20)
  expect_error(land_to_graph(adjacency_maps(rep(NA_real_, 4)), 1985), "pixels validos")
})

test_that("centralities use inverse contact weights and handle disconnected networks", {
  rede <- land_to_graph(adjacency_maps(c(3, 4, 15), 1, 3), 1985, build_plot = FALSE)
  expect_equal(rede$node_metrics$betweenness, c(0, 1, 0))
  expect_equal(rede$node_metrics$harmonic, c(0.75, 1, 0.75))
  expect_equal(rede$node_metrics$neighbor_entropy, c(0, log(2), 0))
  disconnected <- land_to_graph(adjacency_maps(c(3, 4, NA, 15, 24), 1, 5),
                                 1985, build_plot = FALSE)
  expect_equal(disconnected$graph_metrics$connected_components, 2)
  expect_equal(disconnected$node_metrics$harmonic, rep(1 / 3, 4))
})

test_that("CRS units and latitude control physical boundary lengths", {
  feet <- land_to_graph(adjacency_maps(c(3, 4), 1, 2, crs = "EPSG:2227"),
                        1985, build_plot = FALSE)
  expect_equal(feet$edge_metrics$boundary_m, 20 * 1200 / 3937, tolerance = 1e-6)
  r <- raster::raster(nrows = 2, ncols = 1, xmn = -49, xmx = -48.99,
                      ymn = -24.01, ymx = -23.99, crs = "EPSG:4326")
  mapas <- list(raster = list(raster::setValues(r, c(3, 4))), time_range = c(1985, 1985), collection = 10)
  geo <- land_to_graph(mapas, 1985, build_plot = FALSE)
  # A 0.01-degree east-west side at latitude -24 is about 1.016 km.
  expect_equal(geo$edge_metrics$boundary_m / 1016, 1, tolerance = 0.005)
})

test_that("validation and year indexing follow the maps object", {
  mapas <- adjacency_maps()
  mapas$collection <- "10"
  inherited <- land_to_graph(mapas, build_plot = FALSE)
  expect_equal(inherited$metadata$year, 1985)
  expect_equal(inherited$metadata$collection, 10)
  expect_equal(mapas$collection, "10")
  expect_error(land_to_graph(NULL, 1985), "data")
  for (yr in list(1984, 1985.5, NA_real_, c(1985, 1985))) {
    expect_error(land_to_graph(mapas, yr), "year")
  }
  expect_error(land_to_graph(mapas, 1985, connectivity = 6), "connectivity")
  expect_error(land_to_graph(mapas, 1985, include_self = NA), "logicos")
  mapas$time_range <- c(1985, 1986)
  mapas$raster[[2]] <- raster::setValues(mapas$raster[[1]], rep(15, 4))
  expect_error(land_to_graph(mapas, build_plot = FALSE), "year")
  rede <- land_to_graph(mapas, 1986, build_plot = FALSE)
  expect_equal(rede$node_metrics$code, 15)
})

test_that("static and interactive plots use the same native analysis", {
  skip_if_not_installed("ggiraph")
  rede <- land_to_graph(adjacency_maps(), 1985, include_self = TRUE)
  expect_s3_class(rede$plot, "ggplot")
  expect_s3_class(rede$heatmap, "ggplot")
  expect_s3_class(rede$graph_plot, "girafe")
  expect_s3_class(ggplot2::ggplot_build(rede$plot), "ggplot_built")
  expect_equal(rede$adjacency,
    land_to_graph(adjacency_maps(), 1985, include_self = TRUE, build_plot = FALSE)$adjacency)
})

test_that("saved maps support the simple tutorial workflow", {
  rede <- biomastats::land_to_graph(example_maps(), year = 1985, build_plot = FALSE)
  expect_s3_class(rede$graph, "igraph")
  expect_equal(rede$adjacency, t(rede$adjacency))
  expect_equal(rede$boundary_m, t(rede$boundary_m))
  expect_gt(rede$graph_metrics$total_boundary_m, 0)
  expect_equal(sum(rede$node_metrics$pixels),
               sum(!is.na(raster::getValues(example_maps()$raster[[1]]))))
})

test_that("dictionary keys, both languages and original colours survive presentation choices", {
  for (collection in c(7, 10)) {
    mapas <- adjacency_maps()
    mapas$collection <- collection
    pt <- land_to_graph(mapas, 1985, language = "pt", build_plot = FALSE)
    en <- land_to_graph(mapas, 1985, language = "en", build_plot = FALSE)
    dic <- dict_build(collection)
    dic <- dic[match(pt$node_metrics$code, dic$code), ]
    expect_s3_class(pt, "biomastats_land_graph")
    expect_equal(pt$node_metrics$classe, dic$classe)
    expect_equal(pt$node_metrics$class, dic$class)
    expect_equal(pt$node_metrics$color, dic$color)
    expect_equal(pt$node_metrics$label, dic$classe)
    expect_equal(en$node_metrics$label, dic$class)
    expect_equal(pt$adjacency, en$adjacency)
    expect_equal(pt$boundary_m, en$boundary_m)
    expect_equal(pt$class_dictionary, en$class_dictionary)
    expect_equal(igraph::vertex_attr(pt$graph, "classe"), dic$classe)
    expect_equal(igraph::vertex_attr(pt$graph, "strength"), pt$node_metrics$strength)
    expect_equal(igraph::edge_attr(pt$graph, "boundary_m"), pt$edge_metrics$boundary_m)
  }
  unknown <- land_to_graph(adjacency_maps(c(999, 3), 1, 2), 1985, build_plot = FALSE)
  expect_false(unknown$class_dictionary$known[2])
  expect_equal(unknown$class_dictionary$class[2], "Class 999")
})

test_that("returned analysis round trips without original rasters", {
  rede <- land_to_graph(adjacency_maps(), 1985, build_plot = FALSE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(rede, path)
  saved <- readRDS(path)
  fields <- setdiff(names(rede), "graph")
  expect_equal(saved[fields], rede[fields])
  expect_equal(igraph::as_adjacency_matrix(saved$graph, attr = "weight", sparse = FALSE),
               igraph::as_adjacency_matrix(rede$graph, attr = "weight", sparse = FALSE))
  expect_equal(igraph::vertex_attr(saved$graph, "color"), saved$class_dictionary$color)
})
test_that("customised native graphics and widgets can be exported", {
  skip_if_not_installed("ggiraph")
  skip_if_not_installed("htmlwidgets")
  rede <- land_to_graph(adjacency_maps(), 1985, language = "en")
  expect_null(rede$plot$labels$title)
  edited_title <- rede$plot + ggplot2::labs(title = "Custom network")
  expect_equal(edited_title$labels$title, "Custom network")
  legend_labels <- paste(rede$node_metrics$code, rede$node_metrics$label)
  for (plot_name in c("plot", "heatmap", "edge_plot")) {
    built <- ggplot2::ggplot_build(rede[[plot_name]])
    aesthetic <- if (plot_name == "edge_plot") "fill" else "colour"
    scale <- built$plot$scales$get_scales(aesthetic)
    expect_equal(as.character(scale$get_labels()), legend_labels)
    expect_equal(unname(scale$map(as.character(rede$node_metrics$code))),
                 rede$class_dictionary$color)
  }
  expect_null(rede$heatmap$labels$title)
  expect_null(rede$edge_plot$labels$title)
  expect_equal(rede$plot$layers[[2]]$data$color, rede$class_dictionary$color)
  expect_equal(rede$class_dictionary$color, dict_build(10)$color[match(c(3, 4), dict_build(10)$code)])
  expect_equal(rede$node_metrics$label, rede$node_metrics$class)
  expect_s3_class(rede$edge_plot, "ggplot")
  rede$plot <- rede$plot + ggplot2::labs(title = "Edited after analysis")
  outdir <- tempfile("network-graphics-")
  on.exit(unlink(outdir, recursive = TRUE), add = TRUE)
  dir.create(outdir)
  ggplot2::ggsave(file.path(outdir, "plot.png"), plot = rede$plot, width = 5, height = 4, dpi = 72)
  htmlwidgets::saveWidget(rede$graph_plot, file.path(outdir, "graph.html"), selfcontained = FALSE)
  saveRDS(rede, file.path(outdir, "analysis.rds"))
  expect_true(file.info(file.path(outdir, "plot.png"))$size > 0)
  expect_true(file.info(file.path(outdir, "graph.html"))$size > 0)
  expect_equal(readRDS(file.path(outdir, "analysis.rds"))$plot$labels$title,
               "Edited after analysis")
})

test_that("a class without contacts still has exportable graphics and tables", {
  skip_if_not_installed("ggiraph")
  rede <- land_to_graph(adjacency_maps(3, 1, 1), 1985)
  expect_equal(nrow(rede$edge_metrics), 0L)
  expect_s3_class(rede$graph_plot, "girafe")
  for (name in c("plot", "heatmap", "edge_plot")) {
    expect_s3_class(ggplot2::ggplot_build(rede[[name]]), "ggplot_built")
  }
})
