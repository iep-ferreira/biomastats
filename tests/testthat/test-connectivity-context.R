# Mapa sintetico em UTM 22S (celulas de 10 m) com codigos MapBiomas: floresta (3)
# sobre pastagem (15). A e C ficam na area focal [0, 200] x [0, 200] m, a 80 m
# uma da outra; B, fora dela, liga as duas (36 m de cada); D fica longe.
ctx_x0 <- 500000
ctx_y0 <- 7400000
ctx_map <- function() {
  r <- terra::rast(xmin = ctx_x0, xmax = ctx_x0 + 600, ymin = ctx_y0, ymax = ctx_y0 + 300,
                   resolution = 10, crs = "EPSG:32722")
  terra::values(r) <- 15
  block <- function(r, x0, x1, y0, y1) {
    cells <- terra::cells(terra::crop(r, terra::ext(ctx_x0 + x0 + 1, ctx_x0 + x1 - 1,
      ctx_y0 + y0 + 1, ctx_y0 + y1 - 1)))
    xy <- terra::xyFromCell(terra::crop(r, terra::ext(ctx_x0 + x0 + 1, ctx_x0 + x1 - 1,
      ctx_y0 + y0 + 1, ctx_y0 + y1 - 1)), cells)
    r[terra::cellFromXY(r, xy)] <- 3
    r
  }
  r <- block(r, 150, 190, 20, 60)    # A
  r <- block(r, 150, 190, 140, 180)  # C
  r <- block(r, 220, 260, 80, 120)   # B (contexto)
  r <- block(r, 500, 540, 120, 160)  # D (contexto distante)
  raster::setValues(raster::raster(r), terra::values(r, mat = FALSE))
}
ctx_box <- function(x0, x1, y0, y1, crs = 32722) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
    c(ctx_x0 + x0, ctx_y0 + y0), c(ctx_x0 + x1, ctx_y0 + y0), c(ctx_x0 + x1, ctx_y0 + y1),
    c(ctx_x0 + x0, ctx_y0 + y1), c(ctx_x0 + x0, ctx_y0 + y0)))), crs = crs))
}
ctx_focal <- function() ctx_box(0, 200, 0, 200)
ctx_maps <- function() {
  list(shape = sf::st_transform(ctx_focal(), 4326), time_range = c(2020, 2020),
       raster = list(ctx_map()), collection = 10)
}
# Mapas da area de estudo (sem contexto) e origem maior em source.
ctx_run <- function(...) {
  connectivity_context(ctx_maps(), classes = 3, max_dist = 60, context = c(0, 50, 100, 400),
                       source = ctx_maps(), p_ref = 0.5, d_ref = 30, ...)
}

test_that("load_rasters accepts sf and adds a context buffer", {
  lib <- withr::local_tempdir()
  r <- terra::rast(xmin = -48.1, xmax = -47.9, ymin = -23.1, ymax = -22.9, resolution = 0.001,
                   crs = "EPSG:4326")
  terra::values(r) <- 15
  terra::writeRaster(r, file.path(lib, "brasil_coverage_2020.tif"), datatype = "INT1U")
  area <- sf::st_sf(geometry = sf::st_as_sfc(sf::st_bbox(c(xmin = -48.02, xmax = -47.98,
    ymin = -23.02, ymax = -22.98), crs = 4326)))
  plain <- load_rasters(area, start = 2020, end = 2020, method = "library",
                        import_folder_path = lib)
  expect_null(plain$focal)
  expect_equal(plain$source$method, "library")
  expect_equal(plain$source$context_m, 0)
  wide <- load_rasters(sf::st_geometry(area), start = 2020, end = 2020, method = "library",
                       import_folder_path = lib, context_m = 1000)
  expect_equal(sf::st_area(wide$focal), sf::st_area(plain$shape))
  utm <- sf::st_crs(32723)
  gap <- sf::st_distance(sf::st_boundary(sf::st_transform(wide$focal, utm)),
                         sf::st_boundary(sf::st_transform(wide$shape, utm)))
  expect_equal(as.numeric(gap), 1000, tolerance = 1e-3)
  expect_gt(raster::ncell(wide$raster[[1]]), raster::ncell(plain$raster[[1]]))
  expect_equal(wide$source$context_m, 1000)
  expect_true(length(compare_shapefiles(area)) >= 1)
  expect_error(load_rasters(area, method = "library", import_folder_path = lib, start = 2020,
                            end = 2020, context_m = -1), "context_m")
})

test_that("reclass_to_polygons carries the focal area", {
  obj <- list(rec_map = list(raster::setValues(ctx_map(), ifelse(raster::getValues(ctx_map()) == 3,
    3, 0))), start = 2020, end = 2020, classes = 3, collection = 10,
    boundary = ctx_box(0, 600, 0, 300), focal = ctx_focal())
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  expect_equal(attr(p, "focal"), ctx_focal())
  expect_equal(nrow(p[p$value == 1, ]), 4)
})

test_that("land_connectivity weighs focal habitat and uses the context as stepping stones", {
  obj <- list(rec_map = list(raster::setValues(ctx_map(), ifelse(raster::getValues(ctx_map()) == 3,
    3, 0))), start = 2020, end = 2020, classes = 3, collection = 10)
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  p <- p[p$value == 1, ]
  out <- land_connectivity(p, max_dist = 60, focal = ctx_focal(), metric_patch = c("IIC", "PC"),
                           d_ref = 30, build_plot = FALSE)
  pm <- out$patch_metrics
  expect_equal(sort(pm$focal_area_m2), c(0, 0, 1600, 1600))
  expect_equal(c(table(pm$focal_role)), c(context = 2L, focal = 2L))
  expect_equal(out$metadata$landscape_area_m2, 200^2)
  expect_equal(out$metadata$habitat_area_m2, 3200)
  a <- 1600
  # A e C ligadas por B: dois passos no IIC.
  expect_equal(out$metrics$IIC, (2 * a^2 + 2 * a^2 / 3) / 200^4)
  k <- out$metrics$k
  p_ac <- max(exp(-k * 80), exp(-k * 2 * sqrt(30^2 + 20^2)))
  expect_equal(out$metrics$PC, (2 * a^2 + 2 * a^2 * p_ac) / 200^4)
  context <- pm$focal_role == "context"
  expect_equal(pm$dIIC_intra[context], c(0, 0))
  expect_equal(pm$dIIC_flux[context], c(0, 0))
  expect_equal(max(pm$dIIC_connector[context]), 100 * (2 * a^2 / 3) / (2 * a^2 + 2 * a^2 / 3))
  expect_equal(out$metadata$focal$patches[["context"]], 2L)
  expect_equal(out$metadata$schema_version, "1.4")
  # Sem focal, nada muda; fragmento que cruza o limite entra com a parte interna.
  plain <- land_connectivity(p, max_dist = 60, build_plot = FALSE)
  expect_null(plain$focal)
  expect_false("focal_role" %in% names(plain$patch_metrics))
  half <- land_connectivity(p, max_dist = 60, focal = ctx_box(0, 170, 0, 200), build_plot = FALSE)
  expect_equal(sort(half$patch_metrics$focal_area_m2), c(0, 0, 800, 800))
  expect_setequal(half$patch_metrics$focal_role, c("edge", "context"))
  expect_error(land_connectivity(p, focal = ctx_box(0, 100, 200, 300), build_plot = FALSE),
               "area focal")
  expect_error(land_connectivity(p, focal = 1, build_plot = FALSE), "focal")
})

test_that("connectivity_context measures the gain of a stepping stone outside the area", {
  ctx <- ctx_run()
  expect_s3_class(ctx, "biomastats_connectivity_context")
  s <- ctx$sensitivity
  iic <- s[s$metric == "IIC", ]
  expect_equal(iic$radius_m, c(0, 50, 100, 400))
  expect_equal(iic$patches, c(2, 3, 3, 4))
  expect_equal(iic$context_patches, c(0, 1, 1, 2))
  a <- 1600
  expect_equal(iic$index, c(2 * a^2, rep(2 * a^2 + 2 * a^2 / 3, 3)) / 200^4)
  expect_equal(iic$ECA_ha, sqrt(iic$index) * 200^2 / 10000)
  expect_equal(iic$ProtConn, 100 * iic$ECA_ha * 10000 / 200^2)
  expect_equal(iic$ECA_change[4], 0)
  expect_lt(iic$ECA_change[1], 0)
  expect_true(all(iic$converged))
  # Os indices nunca caem com o raio.
  for (metric in c("IIC", "PC")) {
    expect_true(all(diff(s$ECA_ha[s$metric == metric]) >= -1e-9))
  }
  ss <- ctx$stepping_stones
  expect_equal(unique(ss$metric), c("IIC", "PC"))
  expect_equal(ss$radius_m[ss$metric == "IIC"], c(50, 100, 400))
  ps <- ctx$patch_sensitivity
  expect_setequal(unique(ps$focal_role), "focal")
  expect_equal(nrow(ps[ps$metric == "IIC", ]), 2 * 4)
  rs <- ctx$rank_stability
  expect_equal(rs$top10_overlap, rep(100, nrow(rs)))
  expect_equal(nrow(ctx$contexts), 4)
  expect_equal(names(ctx$connectivity[["2020"]]), c("0", "50", "100", "400"))
  expect_equal(ctx$metadata$maps_source, "source")
  for (name in c("sensitivity_plot", "rank_plot", "map_plot")) {
    expect_s3_class(ctx[[name]], "ggplot")
  }
  roundtrip <- unserialize(serialize(ctx, NULL))
  expect_equal(roundtrip$sensitivity, ctx$sensitivity)
})

test_that("reused distances match a direct analysis of each context", {
  ctx <- ctx_run(build_plot = FALSE)
  p <- ctx$polygons[["2020"]]
  p <- p[p$value == 1, ]
  for (r in c(50, 400)) {
    zone <- biomastats:::.context_buffer(sf::st_transform(ctx$focal, sf::st_crs(p)), r)
    sub <- p[lengths(sf::st_intersects(p, zone)) > 0, ]
    direct <- land_connectivity(sub, max_dist = 60, boundary = zone, focal = ctx$focal,
      metric_patch = c("IIC", "PC"), d_ref = 30, build_plot = FALSE)
    reused <- ctx$connectivity[["2020"]][[as.character(r)]]
    expect_equal(reused$metrics[c("IIC", "PC", "ECA_PC")], direct$metrics[c("IIC", "PC", "ECA_PC")])
    expect_equal(reused$patch_metrics$dPC, direct$patch_metrics$dPC)
  }
  # O raio 0 e a analise sem contexto: habitat recortado e limite focal.
  base <- ctx$connectivity[["2020"]][["0"]]
  expect_null(base$focal)
  expect_equal(base$metadata$landscape_area_m2, 200^2)
  expect_equal(nrow(base$analysis_polygons), 2)
})

test_that("connectivity_context reloads the context with the recorded origin", {
  lib <- withr::local_tempdir()
  wgs <- terra::project(terra::rast(ctx_map()), "EPSG:4326", method = "near")
  terra::writeRaster(wgs, file.path(lib, "brasil_coverage_2020.tif"), datatype = "INT1U")
  focal <- sf::st_transform(ctx_focal(), 4326)
  maps <- load_rasters(focal, start = 2020, end = 2020, method = "library",
                       import_folder_path = lib)
  ctx <- connectivity_context(maps, classes = 3, max_dist = 60, context = c(0, 100),
                              metric_patch = "IIC", build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "load_rasters (library)")
  s <- ctx$sensitivity
  expect_equal(s$context_patches, c(0, 1))
  expect_gt(s$ECA_ha[2], s$ECA_ha[1])
  # Mapas carregados com contexto suficiente sao reaproveitados, sem recarga.
  wide <- load_rasters(focal, start = 2020, end = 2020, method = "library",
                       import_folder_path = lib, context_m = 150)
  again <- connectivity_context(wide, classes = 3, max_dist = 60, context = c(0, 100),
                                metric_patch = "IIC", build_plot = FALSE)
  expect_equal(again$metadata$maps_source, "maps")
  expect_equal(again$sensitivity$ECA_ha, s$ECA_ha, tolerance = 1e-6)
})

test_that("connectivity_context accepts polygons and least-cost distances", {
  obj <- list(rec_map = list(raster::setValues(ctx_map(), ifelse(raster::getValues(ctx_map()) == 3,
    3, 0))), start = 2020, end = 2020, classes = 3, collection = 10,
    boundary = ctx_box(0, 600, 0, 300), focal = ctx_focal())
  p <- reclass_to_polygons(obj, save_shp = FALSE)
  ctx <- connectivity_context(p, max_dist = 60, context = c(0, 50, 400), metric_patch = "IIC",
                              build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "polygons")
  expect_equal(ctx$sensitivity$patches, c(2, 3, 4))
  # Pastagem com custo 3: a ligacao por B passa de 60 e A, C voltam a se separar.
  cost <- connectivity_context(ctx_maps(), classes = 3, max_dist = 60, context = c(0, 100),
    source = ctx_maps(), resistance = c("3" = 1, "15" = 3), metric_patch = "IIC",
    build_plot = FALSE)
  expect_match(cost$metadata$distance, "least-cost")
  expect_equal(cost$sensitivity$ECA_ha[1], cost$sensitivity$ECA_ha[2])
  flat <- connectivity_context(ctx_maps(), classes = 3, max_dist = 60, context = c(0, 100),
    source = ctx_maps(), resistance = c("3" = 1, "15" = 1), metric_patch = "IIC",
    build_plot = FALSE)
  expect_gt(flat$sensitivity$ECA_ha[2], flat$sensitivity$ECA_ha[1])
})

test_that("connectivity_context validates its input", {
  expect_error(connectivity_context(ctx_maps(), max_dist = 60, source = ctx_maps()), "classes")
  expect_error(connectivity_context(ctx_maps(), classes = 3, context = 100), "dois raios")
  expect_error(connectivity_context(ctx_maps(), classes = 3, context = c(0, 100)), "origem")
  expect_error(connectivity_context(ctx_maps(), classes = 3, years = 1999, source = ctx_maps()),
               "years")
  p <- reclass_to_polygons(list(rec_map = list(raster::setValues(ctx_map(),
    ifelse(raster::getValues(ctx_map()) == 3, 3, 0))), start = 2020, end = 2020,
    classes = 3), save_shp = FALSE)
  expect_error(connectivity_context(p, context = c(0, 50)), "focal")
  expect_error(connectivity_context(ctx_maps(), classes = 3, context = c(0, 50),
                                    source = ctx_maps(), tolerance = -1), "tolerance")
})

test_that("targets measure joint importance against the context", {
  ctx <- ctx_run(build_plot = FALSE)
  ids <- ctx$patch_sensitivity$fragment_id[ctx$patch_sensitivity$metric == "IIC" &
                                             ctx$patch_sensitivity$radius_m == 400]
  focal_ids <- sort(unique(ids))
  out <- ctx_run(targets = list(a = focal_ids[1], ac = focal_ids))
  t <- out$target_sensitivity
  iic <- t[t$metric == "IIC", ]
  a <- iic[iic$target == "a", ]
  ac <- iic[iic$target == "ac", ]
  expect_equal(a$radius_m, c(0, 50, 100, 400))
  # Sem B: N = 2 a^2. Com B: N = 2 a^2 + 2 a^2 / 3; sem A resta a^2.
  expect_equal(a$importance, c(50, rep(62.5, 3)))
  expect_equal(ac$importance, rep(100, 4))
  expect_equal(ac$individual_sum, c(100, rep(125, 3)))
  expect_equal(ac$interaction, c(0, rep(-25, 3)))
  expect_equal(ac$patches, rep(2L, 4))
  expect_equal(ac$focal_area_ha, rep(0.32, 4))
  expect_equal(a$importance_change, c(-12.5, 0, 0, 0))
  expect_true(all(a$converged))
  expect_s3_class(out$target_plot, "ggplot")
  # Trampolim fora da area focal, dado por geometria: so conta com contexto.
  b <- ctx_box(215, 265, 75, 125)
  b$target <- "B"
  sb <- ctx_run(targets = b, build_plot = FALSE)$target_sensitivity
  sb <- sb[sb$metric == "IIC", ]
  expect_equal(sb$patches, c(0L, 1L, 1L, 1L))
  expect_equal(sb$importance, c(0, rep(25, 3)))
  expect_equal(sb$focal_area_ha, rep(0, 4))
  # Sem coluna target, cada linha e um alvo; agrupadas, formam um conjunto.
  two <- rbind(ctx_box(140, 200, 10, 70), ctx_box(140, 200, 130, 190))
  expect_setequal(ctx_run(targets = two, build_plot = FALSE)$target_sensitivity$target, c("1", "2"))
  two$target <- "both"
  both <- ctx_run(targets = two, build_plot = FALSE)$target_sensitivity
  expect_equal(both$importance[both$metric == "IIC"], ac$importance)
  # PC: igual a remocao explicita no maior contexto.
  res <- out$connectivity[["2020"]][["400"]]
  keep <- !res$analysis_polygons$fragment_id %in% focal_ids
  w <- res$analysis_polygons$focal_area_m2
  D <- unname(res$distance)
  k <- res$metrics$k
  num <- function(D, w) {
    for (m in seq_len(nrow(D))) D <- pmin(D, outer(D[, m], D[m, ], "+"))
    sum(outer(w, w) * exp(-k * D))
  }
  pc <- t[t$metric == "PC" & t$target == "ac" & t$radius_m == 400, ]
  expect_equal(pc$importance, 100 * (num(D, w) - num(D[keep, keep], w[keep])) / num(D, w))
  expect_error(ctx_run(targets = 999), "ausentes")
  expect_error(ctx_run(targets = "x"), "targets")
  expect_null(ctx_run(build_plot = FALSE)$target_sensitivity)
  st <- ctx$stepping_stones
  expect_equal(sort(unique(st$radius_m)), c(50, 100, 400))
})

test_that("the context works for ESA WorldCover outside MapBiomas", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "v200", "2021", "map"), recursive = TRUE)
  wc <- terra::project(terra::rast(ctx_map()), "EPSG:4326", method = "near")
  wc <- terra::classify(wc, rbind(c(3, 10), c(15, 30)))
  terra::writeRaster(wc, file.path(root, "v200", "2021", "map",
                                   "ESA_WorldCover_10m_2021_v200_S24W051_Map.tif"),
                     datatype = "INT1U")
  withr::local_options(biomastats.worldcover_root = root)
  focal <- sf::st_transform(ctx_focal(), 4326)
  maps <- load_lulc(focal, years = 2021)
  expect_equal(maps$source$loader, "load_lulc")
  expect_null(maps$focal)
  wide <- load_lulc(focal, years = 2021, context_m = 100)
  expect_equal(sf::st_area(wide$focal), sf::st_area(maps$shape))
  ctx <- connectivity_context(maps, classes = 10, max_dist = 60, context = c(0, 100),
                              metric_patch = "IIC", build_plot = FALSE)
  expect_equal(ctx$metadata$maps_source, "load_lulc")
  expect_equal(ctx$sensitivity$context_patches, c(0, 1))
  expect_gt(ctx$sensitivity$ECA_ha[2], ctx$sensitivity$ECA_ha[1])
  expect_equal(attr(ctx$polygons[["2021"]], "class_dictionary")$class, "Tree cover")
})

test_that("a custom loader serves any collection, with or without a dictionary", {
  coded <- raster::setValues(ctx_map(), ifelse(raster::getValues(ctx_map()) == 3, 7, 2))
  loader <- function(area, years) {
    expect_s3_class(area, "sf")
    list(raster = list(coded), time_range = c(years, years), collection = "minha_colecao")
  }
  maps <- list(shape = sf::st_transform(ctx_focal(), 4326), time_range = c(2020, 2020),
               raster = list(coded))
  ctx <- connectivity_context(maps, classes = 7, max_dist = 60, context = c(0, 100),
                              source = loader, metric_patch = "IIC", build_plot = FALSE,
                              resistance = c("7" = 1, "2" = 1))
  expect_equal(ctx$metadata$maps_source, "source function")
  expect_equal(attr(ctx$polygons[["2020"]], "class_dictionary")$classe, "Classe 7")
  expect_gt(ctx$sensitivity$ECA_ha[2], ctx$sensitivity$ECA_ha[1])
  expect_error(connectivity_context(maps, classes = 7, context = c(0, 100),
    source = function(area, years) 1), "formato")
})
