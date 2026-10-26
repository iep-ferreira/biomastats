# Quadrados (x0, y0, lado em m) como poligonos de reclass_to_polygons.
mk_patches <- function(x0, y0, side, ids = seq_along(x0), year = NULL) {
  geoms <- lapply(seq_along(x0), function(i) sf::st_polygon(list(rbind(
    c(x0[i], y0[i]), c(x0[i] + side[i], y0[i]), c(x0[i] + side[i], y0[i] + side[i]),
    c(x0[i], y0[i] + side[i]), c(x0[i], y0[i])))))
  p <- sf::st_sf(value = 1, fragment_id = ids, geometry = sf::st_sfc(geoms, crs = 32722))
  if (!is.null(year)) p$year <- year
  p
}
mk_box <- function(x0, y0, w, h) {
  sf::st_sfc(sf::st_polygon(list(rbind(c(x0, y0), c(x0 + w, y0), c(x0 + w, y0 + h),
    c(x0, y0 + h), c(x0, y0)))), crs = 32722)
}
# Fragmentos da linha: 10-20 a 10 m, 20-30 a 15 m, 40 isolado (como test-land-connectivity).
mk_line <- function() mk_patches(c(0, 20, 45, 120) + 500000, rep(7400000, 4), rep(10, 4),
                                 ids = c(10L, 20L, 30L, 40L))
mk_random <- function(seed, n = 18) {
  set.seed(seed)
  side <- stats::runif(n, 30, 120)
  x0 <- 500000 + (seq_len(n) - 1) %% 6 * 180 + stats::runif(n, 0, 40)
  y0 <- 7400000 + (seq_len(n) - 1) %/% 6 * 180 + stats::runif(n, 0, 40)
  mk_patches(x0, y0, side)
}
# Referencia por Floyd-Warshall do numerador do indice para uma matriz D.
mk_numerator <- function(D, a, metric, max_dist, k = NULL) {
  W <- if (metric == "IIC") ifelse(D < max_dist, 1, Inf) else D
  diag(W) <- 0
  for (m in seq_len(nrow(W))) W <- pmin(W, outer(W[, m], W[m, ], "+"))
  sum(outer(a, a) * if (metric == "IIC") 1 / (1 + W) else exp(-k * W))
}

test_that("external distances are aligned by id and reproduce the native analysis", {
  p <- mk_line()
  a <- land_connectivity(p, max_dist = 30, metric_patch = c("IIC", "PC", "AWF"), build_plot = FALSE)
  D <- a$distance
  b <- land_connectivity(p, max_dist = 30, metric_patch = c("IIC", "PC", "AWF"),
                         distance = D[4:1, 4:1], build_plot = FALSE)
  expect_equal(b$metrics, a$metrics)
  expect_equal(b$patch_metrics$dPC_connector, a$patch_metrics$dPC_connector)
  expect_equal(b$metadata$distance_source, "external")
  expect_equal(a$metadata$distance_unit, "m")
  d <- land_connectivity(p, max_dist = 30, metric_patch = "PC", distance = stats::as.dist(D),
                         build_plot = FALSE)
  expect_equal(d$metrics$PC, a$metrics$PC)
  # Tabela From/To/Distance (formato do Makurhini): pares ausentes nao tem ligacao.
  tab <- data.frame(From = c(10, 20, 30), To = c(20, 30, 40), Distance = c(10, 15, 65))
  t <- land_connectivity(p, max_dist = 30, metric_patch = "PC", distance = tab, build_plot = FALSE)
  expect_equal(unname(t$distance["10", "30"]), Inf)
  expect_equal(unname(t$distance["40", "30"]), 65)
  expect_equal(t$metrics$LNK, 2)
  # So faltam ligacoes dominadas (10-30, 20-40, 10-40): o PC nao muda.
  expect_equal(t$metrics$PC, a$metrics$PC)
  unit <- structure(D, unit = "km")
  expect_equal(land_connectivity(p, max_dist = 30, distance = unit,
                                 build_plot = FALSE)$metadata$distance_unit, "km")
  bad <- D; bad[1, 2] <- 99
  expect_error(land_connectivity(p, distance = bad, build_plot = FALSE), "simetrica")
  named <- D; dimnames(named) <- list(1:4, 1:4)
  expect_error(land_connectivity(p, distance = named, build_plot = FALSE), "fragment_id")
  expect_error(land_connectivity(p, distance = -D, build_plot = FALSE), "nao negativas")
  expect_error(land_connectivity(p, distance = D[1:3, 1:3], build_plot = FALSE), "quadrada")
  expect_error(land_connectivity(p, distance = data.frame(a = 1), build_plot = FALSE), "From")
})

test_that("ECA, landscape fractions and ProtConn indicators follow their definitions", {
  p <- mk_line()
  b <- mk_box(499900, 7399900, 400, 300)
  out <- land_connectivity(p, max_dist = 30, boundary = b, metric_patch = c("IIC", "PC"),
                           build_plot = FALSE)
  m <- out$metrics
  la <- 400 * 300
  # IIC: 4 a^2 intra, 2 pares a um passo (a^2 / 2 cada sentido), 1 par a dois passos.
  num <- 4 * 100^2 + 4 * 100^2 / 2 + 2 * 100^2 / 3
  expect_equal(m$IIC, num / la^2)
  expect_equal(m$ECA_IIC, sqrt(num))
  expect_equal(m$ECAnorm_IIC, 100 * sqrt(num) / 400)
  expect_equal(c(m$IIC_intra, m$IIC_direct, m$IIC_step), c(60, 30, 10))
  # PC: direct usa p_ij direto; step e o ganho de p*_ij sobre p_ij.
  k <- m$k
  D <- out$distance
  a <- rep(100, 4)
  P <- exp(-k * D)
  diag(P) <- 0
  pnum <- m$PC * la^2
  expect_equal(m$PC_intra, 100 * 4e4 / pnum)
  expect_equal(m$PC_direct, 100 * sum(outer(a, a) * P) / pnum)
  expect_equal(m$PC_intra + m$PC_direct + m$PC_step, 100)
  expect_gt(m$PC_step, 0)
  expect_equal(m$ECA_PC, sqrt(pnum))
  expect_equal(m$Prot, 100 * 400 / la)
  expect_equal(m$ProtConn, 100 * m$ECA_PC / la)
  expect_equal(m$ProtUnconn, m$Prot - m$ProtConn)
  expect_equal(m$RelConn, 100 * m$ProtConn / m$Prot)
  expect_equal(m$RelConn, m$ECAnorm_PC)
  only_iic <- land_connectivity(p, max_dist = 30, build_plot = FALSE)$metrics
  expect_true(all(is.na(only_iic[c("ECA_PC", "PC_intra", "ProtConn")])))
  expect_equal(out$metadata$schema_version, "1.3")
})

test_that("link importance equals explicit removal of each link", {
  p <- mk_random(7)
  md <- 150
  out <- land_connectivity(p, max_dist = md, metric_patch = c("IIC", "PC"),
                           link_importance = TRUE, prune = FALSE, build_plot = FALSE)
  D <- unname(out$distance)
  a <- out$patch_metrics$area_m2
  ids <- as.character(out$patch_metrics$fragment_id)
  for (metric in c("IIC", "PC")) {
    k <- if (metric == "PC") out$metrics$k
    l <- out$link_metrics[out$link_metrics$metric == metric, ]
    expect_gt(nrow(l), 0)
    full <- mk_numerator(D, a, metric, md, k)
    ref <- vapply(seq_len(nrow(l)), function(i) {
      u <- match(l$from[i], ids); v <- match(l$to[i], ids)
      cut <- D; cut[u, v] <- cut[v, u] <- Inf
      100 * (full - mk_numerator(cut, a, metric, md, k)) / full
    }, numeric(1))
    expect_equal(l$importance, ref, tolerance = 1e-9)
  }
  # IIC lista todas as arestas; as do PC sao so as que sao o melhor caminho.
  expect_equal(sum(out$link_metrics$metric == "IIC"), nrow(out$edge_metrics))
  pruned <- land_connectivity(p, max_dist = md, metric_patch = "PC", link_importance = TRUE,
                              prune = TRUE, build_plot = FALSE)$link_metrics
  expect_equal(pruned$importance, out$link_metrics$importance[out$link_metrics$metric == "PC"])
  plotted <- land_connectivity(mk_line(), max_dist = 30, link_importance = TRUE)
  expect_s3_class(plotted$link_plot, "ggplot")
  expect_null(land_connectivity(mk_line(), max_dist = 30, build_plot = FALSE)$link_metrics)
})

test_that("link importance runs in parallel blocks with the same result", {
  skip_if_not_installed("future.apply")
  p <- mk_random(3, n = 12)
  seq_run <- land_connectivity(p, max_dist = 150, metric_patch = c("IIC", "PC"),
                               link_importance = TRUE, build_plot = FALSE)
  future::plan(future::sequential)
  par_run <- land_connectivity(p, max_dist = 150, metric_patch = c("IIC", "PC"),
                               link_importance = TRUE, parallel = TRUE, build_plot = FALSE)
  expect_equal(par_run$link_metrics, seq_run$link_metrics)
})

# Superficie de codigos 10 m: classe 3 em toda parte.
mk_codes <- function(code = 3) {
  r <- terra::rast(xmin = 499950, xmax = 500200, ymin = 7399950, ymax = 7400100,
                   resolution = 10, crs = "EPSG:32722")
  terra::values(r) <- code
  r
}
mk_wall <- function(cost = NULL) {
  w <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(rbind(c(500035, 7399950),
    c(500035, 7400100))), crs = 32722))
  if (!is.null(cost)) w$cost <- cost
  w
}
mk_three <- function() {
  mk_patches(c(500000, 500050, 500100), c(7400000, 7400000, 7400040), c(20, 20, 20))
}

test_that("resistance_surface maps classes, defaults and barriers", {
  r <- mk_codes()
  r[1:3] <- 4
  r[4] <- 99
  out <- resistance_surface(r, resistance = data.frame(code = c(3, 4), cost = c(1, 5)),
                            default = 50, build_plot = FALSE)
  expect_s3_class(out, "biomastats_resistance")
  expect_s4_class(out$raster, "RasterLayer")
  v <- raster::getValues(out$raster)
  expect_equal(v[1:5], c(5, 5, 5, 50, 1))
  expect_equal(out$classes$code, c(3, 4, 99))
  expect_equal(out$classes$cost, c(1, 5, 50))
  expect_equal(out$classes$cells, c(371L, 3L, 1L))
  expect_equal(sum(out$classes$area_ha), 375 * 100 / 10000)
  # Barreira intransponivel vira NA; barreira finita fica com o maior custo.
  wall <- resistance_surface(mk_codes(), resistance = c("3" = 1), barriers = mk_wall())
  expect_equal(sum(is.na(raster::getValues(wall$raster))), 15)
  expect_s3_class(wall$plot, "ggplot")
  soft <- resistance_surface(mk_codes(), resistance = c("3" = 2), barriers = mk_wall(1.5),
                             build_plot = FALSE)
  expect_equal(sort(unique(raster::getValues(soft$raster))), 2)
  hard <- resistance_surface(mk_codes(), resistance = c("3" = 2), barriers = mk_wall(7),
                             build_plot = FALSE)
  expect_equal(sum(raster::getValues(hard$raster) == 7), 15)
  missing <- resistance_surface(mk_codes(), resistance = c("4" = 1), build_plot = FALSE)
  expect_true(all(is.na(raster::getValues(missing$raster))))
  roundtrip <- unserialize(serialize(out, NULL))
  expect_equal(raster::getValues(roundtrip$raster), v)
  expect_error(resistance_surface(r, resistance = c("3" = 0)), "positivos")
  expect_error(resistance_surface(r, resistance = list(1)), "data.frame")
  expect_error(resistance_surface(r, resistance = c("3" = 1), barrier_cost = -1), "barrier_cost")
  series <- list(raster = list(raster::raster(mk_codes(3)), raster::raster(mk_codes(4))),
                 time_range = c(2000, 2001), collection = 10)
  y <- resistance_surface(series, year = 2001, resistance = c("3" = 1, "4" = 9), build_plot = FALSE)
  expect_equal(unique(raster::getValues(y$raster)), 9)
  expect_error(resistance_surface(series, resistance = c("3" = 1)), "year")
  # Mapas em graus (MapBiomas) vao para a UTM local antes dos custos.
  geo <- resistance_surface(terra::project(mk_codes(), "EPSG:4326", method = "near"),
                            resistance = c("3" = 2), build_plot = FALSE)
  expect_true(sf::st_crs(geo$raster) == sf::st_crs(32722))
  expect_equal(unique(stats::na.omit(raster::getValues(geo$raster))), 2)
  expect_equal(unname(cost_distance(mk_three(), geo)["1", "2"]), 60, tolerance = 0.01)
})

test_that("cost_distance measures edge to edge and respects barriers and costs", {
  p <- mk_three()
  flat <- resistance_surface(mk_codes(), resistance = c("3" = 1), build_plot = FALSE)
  D <- cost_distance(p, flat)
  expect_equal(dimnames(D), list(c("1", "2", "3"), c("1", "2", "3")))
  expect_equal(attr(D, "unit"), "cost-weighted m")
  # Lacuna ortogonal: igual a distancia entre bordas; em diagonal, passos de 8 direcoes.
  expect_equal(D["1", "2"], 30)
  expect_equal(D["2", "3"], 20 * sqrt(2) + 10)
  expect_equal(D, t(D))
  wall <- resistance_surface(mk_codes(), resistance = c("3" = 1), barriers = mk_wall(),
                             build_plot = FALSE)
  Dw <- cost_distance(p, wall)
  expect_equal(unname(Dw["1", c("2", "3")]), c(Inf, Inf))
  expect_equal(Dw["2", "3"], D["2", "3"])
  soft <- resistance_surface(mk_codes(), resistance = c("3" = 1), barriers = mk_wall(5),
                             build_plot = FALSE)
  # Meio passo + 10 (1 a 5) + 10 (5 a 1) + meio passo, com a celula da barreira a 5.
  expect_equal(cost_distance(p, soft)["1", "2"], 5 + 30 + 30 + 5)
  touching <- mk_patches(c(500000, 500020), c(7400000, 7400020), c(20, 20))
  expect_equal(unname(cost_distance(touching, flat)[1, 2]), 0)
  con <- land_connectivity(p, max_dist = 50, distance = Dw, metric_patch = "PC",
                           build_plot = FALSE)
  expect_equal(con$metadata$distance_unit, "cost-weighted m")
  expect_match(con$metadata$distance_source, "least-cost edge-to-edge")
  expect_equal(con$metrics$LNK, 1)
  hole <- mk_codes(); hole[9, 6] <- NA
  expect_error(cost_distance(p, resistance_surface(hole, resistance = c("3" = 1),
    build_plot = FALSE)), "habitat")
  geo <- terra::project(mk_codes(), "EPSG:4326")
  expect_error(cost_distance(p, geo), "projetado")
})

test_that("cost_distance runs in parallel blocks with the same result", {
  skip_if_not_installed("future.apply")
  flat <- resistance_surface(mk_codes(), resistance = c("3" = 1), build_plot = FALSE)
  future::plan(future::sequential)
  expect_equal(cost_distance(mk_three(), flat, parallel = TRUE), cost_distance(mk_three(), flat))
})

test_that("as_makurhini maps patches, boundary and k calibration", {
  p <- mk_line()
  b <- mk_box(499900, 7399900, 400, 300)
  con <- land_connectivity(p, max_dist = c(30, 60), boundary = b, metric_patch = c("IIC", "PC"),
                           build_plot = FALSE)
  mk <- as_makurhini(con, "dPCIIC")
  expect_s3_class(mk, "biomastats_makurhini")
  expect_equal(mk$fun, "MK_dPCIIC")
  expect_named(mk$args, c("30", "60"))
  expect_equal(mk$mapping$distance_threshold, c(15, 30))
  expect_equal(mk$mapping$probability, c(0.5, 0.5))
  a <- mk$args[["30"]]
  expect_equal(exp(10 * log(a$probability) / a$distance_thresholds), exp(-con$metrics$k[1] * 10))
  expect_equal(a$nodes$area_m2, rep(100, 4))
  expect_equal(a$LA, 400 * 300)
  expect_equal(a$distance, unname(con$distance))
  expect_equal(a$pij_min, 0)
  iic <- as_makurhini(con, "dPCIIC", metric = "IIC", max_dist = 60)
  expect_equal(iic$args[["60"]]$distance_thresholds, 60)
  expect_null(iic$args[["60"]]$probability)
  fixed <- land_connectivity(p, max_dist = 30, k = 0.01, metric_patch = "PC", build_plot = FALSE)
  expect_equal(as_makurhini(fixed)$mapping$distance_threshold, log(2) / 0.01)
  pc <- as_makurhini(con, "ProtConn", max_dist = 30)
  expect_equal(pc$args[[1]]$distance, list(type = "edge"))
  expect_equal(as.numeric(sf::st_area(pc$args[[1]]$region)), 400 * 300)
  expect_equal(pc$metadata$protected, "habitat patches")
  expect_error(as_makurhini(con, max_dist = 45), "limiares")
  expect_error(as_makurhini(con, "dECA"), "connectivity_change")
  expect_error(as_makurhini(list(1)), "land_connectivity")
})

test_that("as_makurhini results agree with Makurhini", {
  skip_on_cran()
  skip_if_not(requireNamespace("Makurhini", quietly = TRUE), "Makurhini ausente")
  p <- mk_patches(c(500000, 500050, 500100, 500300), c(7400000, 7400000, 7400040, 7400200),
                  c(20, 20, 20, 35), ids = c(1L, 2L, 3L, 7L))
  con <- land_connectivity(p, max_dist = c(100, 300), boundary = mk_box(499900, 7399900, 600, 500),
                           metric_patch = c("IIC", "PC"), build_plot = FALSE)
  for (metric in c("IIC", "PC")) {
    res <- as_makurhini(con, "dPCIIC", metric = metric, run = TRUE)$result
    for (i in seq_along(res)) {
      overall <- res[[i]][[2]]
      m <- con$metrics[i, ]
      expect_equal(overall$Value[2], m[[paste0("ECA_", metric)]], tolerance = 1e-6)
      expect_equal(overall$Value[4:6], unlist(m[paste0(metric, c("_intra", "_direct", "_step"))]),
                   tolerance = 1e-4, ignore_attr = TRUE)
      nodes <- sf::st_drop_geometry(res[[i]][[1]])
      ours <- con$patch_metrics[con$patch_metrics$max_dist_m == m$max_dist_m, ]
      expect_equal(nodes[[paste0("d", metric)]], ours[[paste0("d", metric)]], tolerance = 1e-5)
    }
  }
  pc <- as_makurhini(con, "ProtConn", max_dist = 100, run = TRUE)$result[[1]]
  value <- function(name) pc$Percentage[pc[["ProtConn indicator"]] == name]
  expect_equal(value("Prot"), con$metrics$Prot[1], tolerance = 1e-3)
  expect_equal(value("ProtConn"), con$metrics$ProtConn[1], tolerance = 1e-3)
  expect_equal(value("RelConn"), con$metrics$RelConn[1], tolerance = 1e-3)
})
