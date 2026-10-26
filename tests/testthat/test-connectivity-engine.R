# Paisagem lconnect em memoria a partir de quadrados (x0, y0, lado em m).
engine_land <- function(x0, y0, side, max_dist, area_l = NULL) {
  geoms <- lapply(seq_along(x0), function(i) sf::st_polygon(list(rbind(
    c(x0[i], y0[i]), c(x0[i] + side[i], y0[i]), c(x0[i] + side[i], y0[i] + side[i]),
    c(x0[i], y0[i] + side[i]), c(x0[i], y0[i])))))
  geometry <- sf::st_sfc(geoms, crs = 32722)
  if (is.null(area_l)) area_l <- sf::st_area(sf::st_convex_hull(sf::st_union(geometry)))
  structure(list(landscape = sf::st_sf(clusters = 1, geometry = geometry),
                 max_dist = max_dist, distance = stats::as.dist(sf::st_distance(geometry)),
                 area_l = area_l), class = "lconnect")
}

random_engine_land <- function(seed, n = 25, max_dist = 250, touch = TRUE) {
  set.seed(seed)
  side <- stats::runif(n, 40, 200)
  x0 <- (seq_len(n) - 1) %% 5 * 260 + stats::runif(n, 0, 60)
  y0 <- (seq_len(n) - 1) %/% 5 * 260 + stats::runif(n, 0, 60)
  if (touch) { x0[2] <- x0[1] + side[1]; y0[2] <- y0[1] }  # contato por aresta
  x0[20:25] <- x0[20:25] + 3000                            # bloco distante
  engine_land(x0, y0, side, max_dist)
}

# Referencia independente por Floyd-Warshall, sem igraph.
floyd <- function(W) {
  for (m in seq_len(nrow(W))) W <- pmin(W, outer(W[, m], W[m, ], "+"))
  W
}
floyd_importance <- function(land, metric, k = NULL) {
  D <- as.matrix(land$distance)
  a <- as.numeric(sf::st_area(land$landscape))
  index <- function(D, a) {
    M <- if (metric == "IIC") {
      W <- ifelse(D < land$max_dist, 1, Inf); diag(W) <- 0
      1 / (1 + floyd(W))
    } else exp(-k * floyd(D))
    sum(outer(a, a) * M)
  }
  full <- index(D, a)
  vapply(seq_along(a), function(r) 100 * (full - index(D[-r, -r, drop = FALSE], a[-r])) / full, 1)
}

test_that("IIC inclui fragmentos isolados (exemplo de tres fragmentos)", {
  s10 <- sqrt(10e4)
  land <- engine_land(c(500000, 505000, 505000 + s10 + 50), rep(7400000, 3),
                      c(1000, s10, s10), max_dist = 200)
  out <- patch_imp_fast(land, "IIC")
  expect_equal(out$prioritization, 100 * c(10000, 200, 200) / 10300, tolerance = 1e-10)
  expect_equal(out$fractions$intra[1], 100 * 10000 / 10300, tolerance = 1e-10)
  expect_equal(out$fractions$connector, c(0, 0, 0))
  expect_equal(attr(out, "index"), 10300 * 1e8 / as.numeric(land$area_l)^2, tolerance = 1e-10)
  expect_equal(patch_imp_fast(land, "IIC", iic_rule = "lconnect")$prioritization, c(0, 100, 100))
})

test_that("decomposicao coincide com a remocao e com Floyd-Warshall", {
  for (seed in 1:3) {
    land <- random_engine_land(seed)
    D <- as.matrix(land$distance)
    expect_true(any(D[upper.tri(D)] == 0))
    for (metric in c("IIC", "PC")) {
      dec <- patch_imp_fast(land, metric)
      rem <- patch_imp_fast(land, metric, method = "removal")
      expect_equal(dec$prioritization, rem$prioritization, tolerance = 1e-8)
      expect_equal(dec$prioritization, floyd_importance(land, metric, attr(dec, "k")), tolerance = 1e-8)
      expect_equal(unname(rowSums(dec$fractions)), dec$prioritization, tolerance = 1e-12)
      expect_null(names(dec$prioritization))
      expect_true(all(dec$fractions$connector > -1e-9))
      expect_equal(attr(dec, "index"), attr(rem, "index"), tolerance = 1e-12)
      expect_null(rem$fractions)
    }
  }
})

test_that("a poda do PC preserva os melhores caminhos com e sem cada fragmento", {
  land <- random_engine_land(5)
  D <- unname(as.matrix(land$distance))
  full <- floyd(D)
  pruned <- .path_lengths(.path_graph(D, "PC", land$max_dist, prune = TRUE), "PC")
  expect_equal(pruned, full, tolerance = 1e-9)
  g <- .path_graph(D, "PC", land$max_dist, prune = TRUE)
  for (r in c(1, 7, 13)) {
    expect_equal(.path_lengths(igraph::delete_vertices(g, r), "PC"), floyd(D[-r, -r]), tolerance = 1e-9)
  }
  expect_lt(igraph::ecount(g), nrow(D) * (nrow(D) - 1) / 2)
})

test_that("PC capta o papel de trampolim que o AWF ignora", {
  # A e B (50 ha) a 450 m; S (1 ha) no meio, a 150 m de cada um.
  sA <- sqrt(50e4)
  land <- engine_land(c(0, sA + 150, sA + 400), c(0, 0, 0), c(sA, 100, sA),
                      max_dist = 200, area_l = units::set_units(1e8, "m^2"))
  pc <- patch_imp_fast(land, "PC")
  awf <- patch_imp_fast(land, "AWF")
  expect_gt(pc$fractions$connector[2], 0)
  expect_equal(awf$fractions$connector, c(0, 0, 0))
  expect_equal(awf$fractions$flux, awf$prioritization)
  expect_gt(pc$prioritization[2], pc$fractions$intra[2] + pc$fractions$flux[2])
})

test_that("k de PC e AWF pode ser calibrado pelo usuario", {
  land <- random_engine_land(7)
  md <- land$max_dist
  expect_equal(.calibrate_k(md), -log(0.5) / (md / 2))
  expect_equal(.calibrate_k(md, p_ref = 0.05, d_ref = 300), -log(0.05) / 300)
  expect_equal(.calibrate_k(md, k = 0.01, p_ref = 0.05), 0.01)
  expect_error(.calibrate_k(md, p_ref = 1), "p_ref")
  expect_error(.calibrate_k(md, d_ref = -1), "d_ref")
  expect_error(.calibrate_k(md, k = 0), "k deve")
  D <- unname(as.matrix(land$distance))
  a <- as.numeric(sf::st_area(land$landscape))
  for (kk in c(0.001, 0.02)) {
    out <- patch_imp_fast(land, "AWF", k = kk)
    expect_equal(attr(out, "k"), kk)
    full <- .awf_value(D, a, kk)
    brute <- vapply(seq_along(a), function(r) 100 * (full - .awf_value(D[-r, -r], a[-r], kk)) / full, 1)
    expect_equal(out$prioritization, brute, tolerance = 1e-10)
  }
  expect_equal(attr(patch_imp_fast(land, "PC", p_ref = 0.05, d_ref = 300), "k"), -log(0.05) / 300)
  expect_true(is.na(attr(patch_imp_fast(land, "IIC"), "k")))
})

test_that("regra lconnect reproduz lconnect::patch_imp", {
  skip_if_not_installed("lconnect")
  land <- random_engine_land(3, touch = FALSE)
  for (metric in c("IIC", "AWF")) {
    capture.output(orig <- suppressWarnings(lconnect::patch_imp(land, metric = metric)))
    fast <- patch_imp_fast(land, metric, iic_rule = "lconnect")
    expect_equal(fast$prioritization, as.numeric(orig$prioritization), tolerance = 1e-10)
  }
})

test_that("a poda do PC e exata, opcional e automatica so em redes densas", {
  land <- random_engine_land(6)
  for (method in c("decomposition", "removal")) {
    on <- patch_imp_fast(land, "PC", method = method, prune = TRUE)
    off <- patch_imp_fast(land, "PC", method = method, prune = FALSE)
    expect_equal(on$prioritization, off$prioritization, tolerance = 1e-10)
    expect_true(attr(on, "pruned"))
    expect_false(attr(off, "pruned"))
    expect_lt(attr(on, "edges"), attr(off, "edges"))
  }
  expect_equal(attr(patch_imp_fast(land, "PC", prune = FALSE), "edges"), 25 * 24 / 2)
  # "auto": below 50 patches nothing is pruned; IIC and AWF are never pruned.
  expect_false(attr(patch_imp_fast(land, "PC"), "pruned"))
  expect_false(attr(patch_imp_fast(land, "IIC", prune = TRUE), "pruned"))
  expect_false(attr(patch_imp_fast(land, "AWF", prune = TRUE), "pruned"))
  dense <- matrix(1, 60, 60)
  expect_true(.resolve_prune("auto", "PC", dense))
  sparse <- dense
  sparse[upper.tri(sparse)] <- Inf
  sparse[1, 2] <- 1
  expect_false(.resolve_prune("auto", "PC", sparse))
  expect_error(patch_imp_fast(land, "PC", prune = "sim"), "prune")
})

test_that("parallel coincide com o calculo sequencial", {
  skip_if_not_installed("future.apply")
  land <- random_engine_land(2)
  # TRUE with no plan opened by the user runs on the sequential plan.
  expect_equal(patch_imp_fast(land, "PC", parallel = TRUE)$prioritization,
               patch_imp_fast(land, "PC")$prioritization)
  expect_error(patch_imp_fast(land, "PC", parallel = 1.5), "parallel")
  expect_error(patch_imp_fast(land, "PC", parallel = NA), "parallel")
  expect_error(patch_imp_fast(land, "PC", parallel = 0), "parallel")
})

test_that("processos em paralelo reproduzem o sequencial e restauram o plano", {
  skip_on_cran()
  skip_if_not_installed("future.apply")
  land <- random_engine_land(3)
  before <- class(future::plan())[2]
  for (metric in c("IIC", "PC")) {
    seq_res <- patch_imp_fast(land, metric, prune = TRUE)
    par_res <- patch_imp_fast(land, metric, prune = TRUE, parallel = 2)
    expect_equal(par_res$prioritization, seq_res$prioritization, tolerance = 1e-12)
    expect_equal(par_res$fractions, seq_res$fractions, tolerance = 1e-12)
  }
  expect_equal(class(future::plan())[2], before)
  D <- unname(as.matrix(land$distance))
  future::plan(future::multisession, workers = 2)
  on.exit(future::plan(future::sequential), add = TRUE)
  expect_identical(.pc_edges(D, TRUE), .pc_edges(D))
  p <- sf::st_sf(value = 1, fragment_id = seq_len(nrow(land$landscape)),
                 geometry = sf::st_geometry(land$landscape))
  seq_lc <- land_connectivity(p, max_dist = c(150, 400), metric_patch = c("IIC", "PC"),
                              prune = TRUE, build_plot = FALSE)
  par_lc <- land_connectivity(p, max_dist = c(150, 400), metric_patch = c("IIC", "PC"),
                              prune = TRUE, parallel = TRUE, build_plot = FALSE)
  expect_equal(sf::st_drop_geometry(par_lc$patch_metrics),
               sf::st_drop_geometry(seq_lc$patch_metrics), tolerance = 1e-12)
  expect_equal(par_lc$metadata$parallel_workers, 2L)
  expect_equal(seq_lc$metadata$parallel_workers, 1L)
  expect_true(all(seq_lc$metrics$pc_pruned))
  expect_true(all(seq_lc$metrics$pc_edges < 25 * 24 / 2))
})

test_that("land_connectivity entrega PC, fracoes e graficos", {
  land <- random_engine_land(4)
  p <- sf::st_sf(value = 1, fragment_id = seq_len(nrow(land$landscape)),
                 geometry = sf::st_geometry(land$landscape))
  out <- land_connectivity(p, max_dist = c(150, 400), metric_patch = c("IIC", "PC", "AWF"),
                           p_ref = 0.05, d_ref = 600)
  expect_true(all(is.finite(out$metrics$PC)))
  expect_equal(out$metrics$k, rep(-log(0.05) / 600, 2))
  pm <- sf::st_drop_geometry(out$patch_metrics)
  for (metric in c("IIC", "PC", "AWF")) {
    parts <- pm[, paste0("d", metric, c("_intra", "_flux", "_connector"))]
    expect_equal(unname(rowSums(parts)), pm[[paste0("d", metric)]], tolerance = 1e-10)
  }
  expect_equal(pm$dAWF_connector, rep(0, nrow(pm)))
  sc <- out$scenarios[["400"]]
  expect_equal(sc$patch_importance$PC$prioritization,
               patch_imp_fast(sc$land, "PC", p_ref = 0.05, d_ref = 600)$prioritization, tolerance = 1e-10)
  fm <- out$fraction_metrics
  expect_equal(nrow(fm), 3 * 3 * nrow(pm))
  expect_setequal(levels(fm$fraction), c("intra", "flux", "connector"))
  for (name in c("fractions_plot", "connector_plot", "importance_plot", "range_plot")) {
    expect_s3_class(ggplot2::ggplot_build(out[[name]]), "ggplot_built")
  }
  awf_only <- land_connectivity(p, max_dist = 150, metric_patch = "AWF", language = "en")
  expect_null(awf_only$connector_plot)
  expect_s3_class(awf_only$fractions_plot, "ggplot")
  expect_error(land_connectivity(p, p_ref = 2), "p_ref")
  expect_error(land_connectivity(p, metric_patch = "XYZ"), "metric_patch")
})
