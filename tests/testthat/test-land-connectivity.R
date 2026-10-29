connectivity_patches <- function(x = c(0, 20, 45, 120)) {
  geoms <- lapply(x + 500000, function(a) sf::st_polygon(list(rbind(
    c(a, 7400000), c(a + 10, 7400000), c(a + 10, 7400010),
    c(a, 7400010), c(a, 7400000)))))
  p <- sf::st_sf(value = rep(1, length(x)), fragment_id = seq_along(x) * 10L,
    year = 1985, code = 3, collection = 10, geometry = sf::st_sfc(geoms, crs = 32722))
  attr(p, "class_dictionary") <- .biomastats_dictionary(10, 3)
  attr(p, "metadata") <- list(collection = 10, classes = 3, years = 1985,
    source = list("1985" = list(resolution = c(10, 10))))
  p
}

test_that("connectivity inherits polygons and retains isolates and IDs", {
  p <- connectivity_patches()
  out <- land_connectivity(p, max_dist = 30, build_plot = FALSE)
  expect_s3_class(out, "biomastats_connectivity")
  expect_equal(out$source_polygons, p)
  expect_equal(out$distance["10", "20"], 10)
  expect_equal(out$distance["20", "30"], 15)
  expect_equal(out$metrics$NC, 2)
  expect_equal(out$metrics$LNK, 2)
  expect_equal(out$metrics$graph_components, 2)
  expect_equal(out$metrics$isolates, 1)
  expect_equal(out$patch_metrics$component, c(1L, 1L, 1L, 2L))
  # Definicao original: o isolado (40) entra com a^2; o fragmento 20 liga 10 e 30.
  expect_equal(out$patch_metrics$iic_included, rep(TRUE, 4))
  expect_equal(out$patch_metrics$dIIC, c(40, 55, 40, 15))
  expect_equal(out$patch_metrics$dIIC_intra, rep(15, 4))
  expect_equal(out$patch_metrics$dIIC_connector, c(0, 10, 0, 0))
  expect_equal(out$metadata$upstream, attr(p, "metadata"))
  expect_equal(out$class_dictionary, attr(p, "class_dictionary"))
  g <- out$scenarios[[1]]$graph
  expect_equal(igraph::vcount(g), 4)
  expect_equal(igraph::vertex_attr(g, "name"), c("10", "20", "30", "40"))
  expect_equal(igraph::ecount(g), 2)
  expect_equal(sort(igraph::edge_attr(g, "distance_m")), c(10, 15))
  expect_s3_class(out$scenarios[[1]]$land, "lconnect")
  expect_named(out$scenarios[[1]]$land$landscape, c("clusters", "geometry"))
  expect_equal(out$scenarios[[1]]$land$fragment_id, p$fragment_id)
  expect_s3_class(out$scenarios[[1]]$patch_importance$IIC, "pimp")
  expect_null(out$plot)
  expect_equal(out$metrics$IIC, (4 * 100^2 + 4 * 100^2 / 2 + 2 * 100^2 / 3) / 1300^2)
  expect_equal(out$metrics$IIC_lconnect, (3 * 100^2 + 4 * 100^2 / 2 + 2 * 100^2 / 3) / 1300^2)
  expect_true(is.na(out$metrics$PC))
  lc <- land_connectivity(p, max_dist = 30, build_plot = FALSE, iic_rule = "lconnect")
  expect_equal(lc$patch_metrics$iic_included, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(lc$patch_metrics$dIIC[c(2, 4)], c(100, 0))
  expect_true(all(is.na(lc$patch_metrics$dIIC_connector)))
})

test_that("fast metrics and patch importance agree with installed lconnect", {
  skip_if_not_installed("lconnect")
  out <- land_connectivity(connectivity_patches(), max_dist = 30, build_plot = FALSE)
  land <- out$scenarios[[1]]$land
  names <- c("NC", "LNK", "SLC", "MSC", "CCP", "LCP", "CPL", "ECS", "AWF", "IIC")
  ours <- sub("^IIC$", "IIC_lconnect", names)
  original <- suppressWarnings(lconnect::con_metric(land, metric = names))
  expect_equal(as.numeric(out$metrics[1, ours]), as.numeric(original), tolerance = 1e-10)
  for (metric in c("IIC", "AWF")) {
    capture.output(native <- suppressWarnings(lconnect::patch_imp(land, metric = metric)))
    fast <- patch_imp_fast(land, metric = metric, iic_rule = "lconnect")
    expect_equal(fast$prioritization, native$prioritization, tolerance = 1e-9)
    expect_equal(fast$landscape$attributes, fast$prioritization)
    expect_equal(attr(fast, "metric"), metric)
    expect_equal(fast$landscape$fragment_id, land$fragment_id)
  }
  if (requireNamespace("future.apply", quietly = TRUE)) {
    expect_equal(patch_imp_fast(land, "IIC", parallel = TRUE)$prioritization,
                 patch_imp_fast(land, "IIC")$prioritization)
  } else {
    expect_error(patch_imp_fast(land, "IIC", parallel = TRUE), "future.apply")
  }
  dir <- tempfile("connectivity-native-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  sf::st_write(out$analysis_polygons[, "value", drop = FALSE], file.path(dir, "habitat.shp"), quiet = TRUE)
  native <- lconnect::upload_land(land_path = file.path(dir, "habitat.shp"), habitat = 1, max_dist = 30)
  original <- suppressWarnings(lconnect::con_metric(native, metric = names))
  expect_equal(as.numeric(out$metrics[1, ours]), as.numeric(original), tolerance = 1e-10)
  expect_equal(patch_imp_fast(native, "AWF")$prioritization,
    out$scenarios[[1]]$patch_importance$AWF$prioritization, tolerance = 1e-9)
})

test_that("AWF removes both diagonal contributions and includes distant pairs", {
  out <- land_connectivity(connectivity_patches(), max_dist = 30, build_plot = FALSE)
  a <- out$analysis_polygons$area_m2
  D <- out$distance
  kernel <- exp(-(-log(0.5) / 15) * D)
  weight <- outer(a, a) * kernel
  diag(weight) <- 0
  expect_equal(out$metrics$AWF, sum(weight))
  manual <- vapply(seq_along(a), function(i) {
    100 * (sum(weight) - sum(weight[-i, -i])) / sum(weight)
  }, numeric(1))
  expect_equal(out$patch_metrics$dAWF, manual, tolerance = 1e-10)
  expect_gt(out$patch_metrics$dAWF[4], 0)
  two <- land_connectivity(connectivity_patches(c(0, 20)), max_dist = 30, build_plot = FALSE)
  expect_equal(two$patch_metrics$dAWF, c(100, 100))
})

test_that("threshold boundaries and zero-distance contacts have explicit semantics", {
  out <- land_connectivity(connectivity_patches(), max_dist = c(30, 10, 10, 1000), build_plot = FALSE)
  expect_named(out$scenarios, c("10", "30", "1000"))
  expect_equal(out$metrics$NC[1], 3)
  expect_equal(out$metrics$graph_components[1], 4)
  expect_equal(out$metrics$LNK[1], 0)
  expect_equal(out$metrics$IIC_lconnect[1], 0)
  expect_equal(out$metrics$IIC[1], 4 * 100^2 / out$metadata$landscape_area_m2^2)
  expect_true(is.na(out$metrics$CPL[1]))
  expect_equal(out$metrics$isolates[3], 0)
  expect_true(all(diff(out$metrics$LNK) >= 0))
  touching <- land_connectivity(connectivity_patches(c(0, 10)), max_dist = 5, build_plot = FALSE)
  expect_equal(touching$metrics$LNK, 1)
  expect_equal(touching$metrics$NC, 1)
  # Touching fragments are linked: (a^2 + a^2 + 2 * a^2 / 2) / area_l^2.
  a <- 100
  expect_equal(touching$metrics$IIC, 3 * a^2 / touching$metadata$landscape_area_m2^2)
  expect_equal(touching$metrics$CPL, 1)
  expect_equal(igraph::ecount(touching$scenarios[[1]]$graph), 1)
  expect_true(all(touching$patch_metrics$iic_included))
  # Sem um deles, resta a^2 do outro: dIIC = 100 * 2 / 3.
  expect_equal(touching$patch_metrics$dIIC, rep(200 / 3, 2))
  expect_equal(touching$metrics$IIC_lconnect, 0)
})

test_that("single patches and zero baselines mark relative priority undefined", {
  for (positions in list(0, c(0, 20, 45))) {
    out <- land_connectivity(connectivity_patches(positions), max_dist = 1, build_plot = FALSE)
    expect_equal(out$metrics$IIC_lconnect, 0)
    expect_equal(out$metrics$IIC, length(positions) * 100^2 / out$metadata$landscape_area_m2^2)
    expect_true(is.na(out$metrics$CPL))
    expect_equal(igraph::vcount(out$scenarios[[1]]$graph), length(positions))
    if (length(positions) == 1L) {
      expect_true(all(is.nan(out$patch_metrics$dIIC)))
      expect_equal(attr(out$scenarios[[1]]$patch_importance$IIC, "status"), "undefined_baseline")
      expect_true(all(is.nan(out$patch_metrics$dAWF)))
      expect_equal(attr(out$scenarios[[1]]$patch_importance$AWF, "status"), "undefined_baseline")
      expect_true(all(is.nan(patch_imp_fast(out$scenarios[[1]]$land, "AWF")$prioritization)))
    } else {
      # Isolados de mesma area: cada um responde por 1/3 do IIC, todo intra.
      expect_equal(out$patch_metrics$dIIC, rep(100 / 3, 3))
      expect_equal(out$patch_metrics$dIIC_intra, rep(100 / 3, 3))
      expect_true(all(is.finite(out$patch_metrics$dAWF)))
      expect_equal(attr(out$scenarios[[1]]$patch_importance$AWF, "status"), "ok")
    }
  }
})

test_that("CRS, years, habitat and denominators are inherited without disk adapters", {
  p <- connectivity_patches()
  geographic <- sf::st_transform(p, 4326)
  out <- land_connectivity(geographic, max_dist = 30, build_plot = FALSE)
  expect_equal(sf::st_crs(out$analysis_polygons)$epsg, 32722)
  expect_equal(sf::st_crs(out$source_polygons)$epsg, 4326)
  expect_equal(out$distance, land_connectivity(p, build_plot = FALSE)$distance, tolerance = 1e-5)
  bound <- sf::st_buffer(sf::st_convex_hull(sf::st_union(sf::st_geometry(p))), 10)
  attr(p, "boundary") <- bound
  inherited <- land_connectivity(p, build_plot = FALSE)
  expect_equal(inherited$metadata$boundary_method, "inherited")
  expect_equal(inherited$metadata$landscape_area_m2, as.numeric(sf::st_area(bound)))
  series <- list("1985" = p, "1986" = p)
  series[[2]]$year <- 1986
  expect_error(land_connectivity(series), "year")
  selected <- land_connectivity(series, year = 1986, build_plot = FALSE)
  expect_equal(selected$metadata$year, 1986)
  p$value[4] <- 0
  expect_equal(nrow(land_connectivity(p, build_plot = FALSE)$analysis_polygons), 3)
  p <- connectivity_patches()
  attr(p, "class_dictionary") <- attr(p, "metadata") <- NULL
  p$classes <- "3,4"
  p$code <- NA_real_
  merged <- land_connectivity(p, build_plot = FALSE)
  expect_equal(merged$metadata$classes, c(3, 4))
  expect_equal(merged$class_dictionary$code, c(3, 4))
  expect_true(all(is.na(merged$patch_metrics$code)))
})

test_that("invalid geometries, selections, and fast inputs fail clearly", {
  p <- connectivity_patches()
  expect_error(land_connectivity(NULL), "polygons")
  expect_error(land_connectivity(p, max_dist = 0), "max_dist")
  expect_error(land_connectivity(p, max_dist = NA_real_), "max_dist")
  expect_error(land_connectivity(p, metric_patch = "bad"), "metric_patch")
  expect_error(land_connectivity(p, crs = 4326), "projetado")
  expect_error(land_connectivity(p, year = 2000), "year")
  expect_error(land_connectivity(p, build_plot = NA), "build_plot")
  p$fragment_id[2] <- p$fragment_id[1]
  expect_error(land_connectivity(p), "fragment_id")
  p <- connectivity_patches(c(0, 5))
  expect_error(land_connectivity(p), "sobrepostos")
  p <- connectivity_patches()
  p$value[] <- 0
  expect_error(land_connectivity(p), "Nenhum fragmento")
  expect_error(patch_imp_fast(list()), "lconnect")
  land <- land_connectivity(connectivity_patches(), build_plot = FALSE)$scenarios[[1]]$land
  expect_error(patch_imp_fast(land, parallel = NA), "parallel")
  land$distance <- stats::as.dist(matrix(NA_real_, 4, 4))
  expect_error(patch_imp_fast(land), "Distancias")
})

test_that("native vectorization, plots, dictionary and RDS remain interoperable", {
  maps <- example_maps()
  metrics <- biomastats_metrics(maps, start = 1985, end = 1985, zone = "22", hemisphere = "south",
    classes = 3, metrics = "class_area")
  polygons <- reclass_to_polygons(metrics, save_shp = FALSE)
  expect_equal(attr(polygons, "boundary"), maps$shape)
  out <- land_connectivity(polygons, max_dist = c(30, 100, 300))
  expect_equal(out$class_dictionary, metrics$class_dictionary)
  expect_equal(out$metadata$upstream, attr(polygons, "metadata"))
  expect_equal(out$metadata$boundary_method, "inherited")
  if (requireNamespace("lconnect", quietly = TRUE)) {
    native <- suppressWarnings(lconnect::con_metric(out$scenarios[["100"]]$land, c("AWF", "IIC")))
    expect_equal(as.numeric(out$metrics[out$metrics$max_dist_m == 100, c("AWF", "IIC_lconnect")]),
                 as.numeric(native), tolerance = 1e-9)
  }
  expect_equal(nrow(out$patch_metrics), 3 * nrow(polygons))
  expect_equal(sort(unique(out$patch_metrics$fragment_id)), polygons$fragment_id)
  for (name in c("plot", "cluster_plot", "importance_plot", "connector_plot", "fractions_plot",
                 "range_plot")) {
    expect_s3_class(out[[name]], "ggplot")
    expect_s3_class(ggplot2::ggplot_build(out[[name]]), "ggplot_built")
  }
  built <- ggplot2::ggplot_build(out$plot)
  expect_equal(unname(built$plot$scales$get_scales("fill")$map("3")), metrics$class_dictionary$color)
  out$plot <- out$plot + ggplot2::labs(title = "Custom connectivity")
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(out, path)
  restored <- readRDS(path)
  expect_equal(restored$metrics, out$metrics)
  expect_equal(restored$class_dictionary, out$class_dictionary)
  expect_equal(restored$metadata, out$metadata)
  expect_equal(restored$plot$labels$title, "Custom connectivity")
  expect_equal(restored$fraction_metrics, out$fraction_metrics)
  expect_equal(restored$patch_metrics, out$patch_metrics)
  expect_equal(attributes(restored$scenarios[[1]]$patch_importance$IIC),
               attributes(out$scenarios[[1]]$patch_importance$IIC))
  for (name in c("fractions_plot", "connector_plot")) {
    expect_s3_class(ggplot2::ggplot_build(restored[[name]]), "ggplot_built")
  }
  expect_equal(restored$metadata$schema_version, "1.4")
  expect_equal(igraph::as_data_frame(restored$scenarios[[1]]$graph),
               igraph::as_data_frame(out$scenarios[[1]]$graph))
})
