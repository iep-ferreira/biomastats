#' Fragment (patch) metrics on the native grid
#'
#' Computes landscape metrics at the fragment level and their distributional
#' summaries per class and year, directly on the native grid of the maps
#' object returned by [load_rasters()]. The CRS is inherited; no raster is
#' reprojected and no zone or hemisphere needs to be given.
#'
#' Geometry: fragments are connected components ([terra::patches()]) of each
#' class. Cell areas come from [terra::cellSize()] (geodesic for geographic
#' grids). Edge lengths are measured on the WGS84 ellipsoid for geographic
#' grids (parallel arcs for north/south edges, meridian arcs for east/west
#' edges of each row) and, for projected grids, from the cell size in metres
#' corrected by the local scale factor. The same code therefore applies to any
#' spatial scale and CRS. Fragments are vectorized with the package
#' vectorization routine (the same used by [reclass_to_polygons()]) only to
#' measure edge-to-edge nearest-neighbour distances and to return geometries.
#'
#' @param maps Maps object from [load_rasters()] (`raster`, `time_range`,
#'   `collection`), or a single raster.
#' @param classes Class codes to analyse. `NULL` uses every class present
#'   (code 0, the background of reclassified rasters, is ignored).
#' @param years Years to analyse; `NULL` uses all years.
#' @param merge_classes If `TRUE`, the selected classes form a single habitat
#'   (as in the reclassification of [biomastats_metrics()]).
#' @param directions Fragment connectivity: 8 (default, as landscapemetrics) or 4.
#' @param vectorize Vectorize fragments to compute nearest-neighbour distances
#'   (`enn_m`) and return `polygons`.
#' @param lsm Optional landscapemetrics class-level functions computed on the
#'   same native grid, e.g. `c("lsm_c_ai", "lsm_c_cohesion")`. Default metrics
#'   are unit-free and valid in any CRS. `NULL` skips them.
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_fragment_metrics` list:
#' \itemize{
#'   \item `patches`: one row per fragment with `year`, `code`, `patch_id`,
#'     `n_cells`, `area_ha`, `perimeter_m`, `core_area_ha` (cells whose
#'     neighbours all belong to the fragment), `core_index` (%), `para` (m/m2),
#'     `shape_index` (landscapemetrics definition, 0.25 P / sqrt(A)),
#'     `shape_circle` (perimeter / circumference of a circle with the same
#'     area), `frac` (fractal dimension), `centroid_x`, `centroid_y` (native
#'     CRS, area-weighted), `lon`, `lat`, `gyration_m` (radius of gyration),
#'     `elongation` (1 - minor/major axis of the area-weighted second moments),
#'     `orientation_deg` (azimuth of the major axis), bounding box and
#'     `enn_m` (edge-to-edge geodesic distance to the nearest fragment of the
#'     same class, found with the s2 spatial index).
#'   \item `summary`: one row per year and class with landscape and class area,
#'     `pland`, `n_patches`, `patch_density` (per 100 ha), `edge_m` and
#'     `edge_density` (m/ha, edges with other classes), area mean, median, SD,
#'     CV, minimum, maximum, quantiles, skewness and Gini coefficient,
#'     largest patch index `lpi`, effective mesh size `mesh_ha`, area-weighted
#'     mean shape and gyration, core area and mean/median `enn_m`.
#'   \item `lsm`: tidy landscapemetrics results for `lsm` (or `NULL`).
#'   \item `polygons`: `sf` fragments with the patch attributes, in the
#'     inherited CRS (when `vectorize = TRUE`).
#'   \item `class_dictionary`, `metadata` (CRS, automatic UTM zone and
#'     hemisphere, cell geometry method) and `plots` (`size_distribution`,
#'     `rank_size`, `shape`, `map`, `trend`).
#' }
#' @export
fragment_metrics <- function(maps, classes = NULL, years = NULL, merge_classes = FALSE,
                             directions = 8L, vectorize = TRUE,
                             lsm = c("lsm_c_ai", "lsm_c_cohesion", "lsm_c_clumpy"),
                             language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  if (!directions %in% c(4, 8)) stop("directions deve ser 4 ou 8.")
  input <- .fragment_input(maps, years)
  collection <- input$collection
  out_patches <- out_summary <- out_lsm <- out_poly <- list()
  source <- list()
  for (i in seq_along(input$years)) {
    yr <- input$years[i]; r <- input$rasters[[i]]
    geom <- .grid_geometry(r)
    v <- matrix(terra::values(r, mat = FALSE), geom$nrow, geom$ncol, byrow = TRUE)
    present <- sort(unique(v[!is.na(v) & v != 0]))
    codes <- if (is.null(classes)) present else intersect(classes, present)
    groups <- if (merge_classes) list(codes) else as.list(codes)
    groups <- groups[lengths(groups) > 0]
    land_ha <- sum(geom$area[!is.na(v)]) / 1e4
    source[[as.character(yr)]] <- list(crs_wkt = geom$crs_wkt, resolution = geom$res,
                                       lonlat = geom$lonlat, utm = .utm_from_raster(r))
    for (g in groups) {
      res <- .fragment_patches(r, v, g, geom, directions, vectorize)
      if (is.null(res)) next
      label <- if (length(g) == 1) g else 1
      res$patches <- cbind(year = yr, code = label, res$patches)
      out_patches[[length(out_patches) + 1]] <- res$patches
      out_summary[[length(out_summary) + 1]] <- .fragment_summary(res$patches, land_ha, res$class_edge, yr, label)
      if (!is.null(res$polygons)) {
        res$polygons$year <- yr; res$polygons$code <- label
        out_poly[[length(out_poly) + 1]] <- res$polygons
      }
    }
    if (length(lsm)) out_lsm[[length(out_lsm) + 1]] <- .fragment_lsm(r, v, codes, merge_classes, lsm, directions, yr, geom$lonlat)
  }
  if (!length(out_patches)) stop("Nenhum fragmento das classes selecionadas foi encontrado.")
  patches <- do.call(rbind, out_patches); rownames(patches) <- NULL
  summary <- do.call(rbind, out_summary); rownames(summary) <- NULL
  codes_all <- sort(unique(if (merge_classes) unlist(classes %||% patches$code) else patches$code))
  dictionary <- if (!is.null(collection)) .biomastats_dictionary(collection, codes_all) else
    data.frame(code = codes_all, classe = paste("Classe", codes_all), class = paste("Class", codes_all),
               grupo = NA_character_, group = NA_character_, color = "#777777", known = FALSE)
  polygons <- NULL
  if (length(out_poly)) {
    same_crs <- length(unique(vapply(out_poly, function(p) sf::st_crs(p)$wkt, ""))) == 1
    polygons <- if (same_crs) do.call(rbind, out_poly) else stats::setNames(out_poly, NULL)
  }
  out <- list(patches = patches, summary = summary,
    lsm = if (length(out_lsm)) do.call(rbind, out_lsm) else NULL,
    polygons = polygons, class_dictionary = dictionary,
    metadata = list(schema_version = "1.0", collection = collection, years = input$years,
      classes = codes_all, merge_classes = merge_classes,
      merged_codes = if (merge_classes) unlist(classes %||% codes_all) else NULL,
      directions = directions, source = source,
      area_unit = "ha", length_unit = "m",
      method = paste("native grid, no raster reprojection; geodesic cell areas (terra::cellSize);",
                     "ellipsoidal edge lengths for geographic grids, scale-factor corrected edges for projected grids"),
      upstream = input$metadata, boundary = input$boundary),
    plots = NULL)
  class(out) <- "biomastats_fragment_metrics"
  if (isTRUE(build_plot)) out$plots <- .fragment_plots(out, language)
  out
}

.fragment_input <- function(maps, years) {
  if (inherits(maps, c("RasterLayer", "SpatRaster")))
    return(list(rasters = list(.as_terra(maps)), years = if (is.null(years)) NA_integer_ else years[1],
                collection = NULL, metadata = NULL, boundary = NULL))
  if (!is.list(maps) || is.null(maps$raster) || length(maps$time_range) != 2L)
    stop("maps deve ser o objeto de load_rasters() ou um raster.")
  all_years <- seq.int(maps$time_range[1], maps$time_range[2])
  if (length(maps$raster) != length(all_years)) stop("Anos e rasters incompat\u00edveis.")
  years <- if (is.null(years)) all_years else sort(unique(as.integer(years)))
  if (any(!years %in% all_years)) stop("Anos fora do intervalo dispon\u00edvel.")
  collection <- if (!is.null(maps$collection)) .biomastats_collection(maps$collection) else NULL
  list(rasters = lapply(maps$raster[match(years, all_years)], .as_terra), years = years,
       collection = collection, metadata = maps$metadata, boundary = maps$shape)
}

# Fragment geometry for one class (or merged class group) in one year.
.fragment_patches <- function(r, v, codes, geom, directions, vectorize) {
  mask <- !is.na(v) & v %in% codes
  if (!any(mask)) return(NULL)
  m <- r; terra::values(m) <- as.vector(t(ifelse(mask, 1, NA)))
  # Connected components by the C++ labelling of landscapemetrics: same partition
  # as terra::patches, orders of magnitude faster on large grids.
  P <- landscapemetrics::get_patches(m, class = 1, directions = directions, return_raster = FALSE)[[1]][[1]]
  cells <- which(!is.na(P)); id <- P[cells]
  ids <- sort(unique(id)); idx <- match(id, ids)
  sumby <- function(z) as.numeric(rowsum(as.numeric(z), idx, reorder = TRUE))
  w <- geom$area[cells]
  # Boundary edges: neighbour outside the fragment (other class, NA or grid edge).
  sides <- list(list(-1, 0, geom$edge_n), list(1, 0, geom$edge_s), list(0, -1, geom$edge_ew), list(0, 1, geom$edge_ew))
  perimeter <- edges <- numeric(length(ids)); class_edge <- 0
  for (s in sides) {
    nb <- .shift(P, s[[1]], s[[2]])
    b <- is.na(nb[cells]) | nb[cells] != id
    perimeter <- perimeter + sumby(b * s[[3]][cells]); edges <- edges + sumby(b)
    vc <- .shift(v, s[[1]], s[[2]])[cells]
    class_edge <- class_edge + sum(s[[3]][cells][!is.na(vc) & !vc %in% codes])
  }
  offsets <- if (directions == 8) expand.grid(dr = -1:1, dc = -1:1)[-5, ] else
    data.frame(dr = c(-1, 1, 0, 0), dc = c(0, 0, -1, 1))
  core <- rep(TRUE, length(cells))
  for (k in seq_len(nrow(offsets))) {
    nb <- .shift(P, offsets$dr[k], offsets$dc[k])[cells]
    core <- core & !is.na(nb) & nb == id
  }
  W <- sumby(w)
  cx <- sumby(w * geom$x[cells]) / W; cy <- sumby(w * geom$y[cells]) / W
  dx <- (geom$x[cells] - cx[idx]) * geom$kx[cells]; dy <- (geom$y[cells] - cy[idx]) * geom$ky[cells]
  sxx <- sumby(w * dx^2) / W; syy <- sumby(w * dy^2) / W; sxy <- sumby(w * dx * dy) / W
  tr <- sxx + syy; disc <- sqrt(pmax(0, (sxx - syy)^2 / 4 + sxy^2))
  l1 <- tr / 2 + disc; l2 <- pmax(0, tr / 2 - disc)
  n <- sumby(rep(1, length(cells)))
  half <- geom$res / 2
  bbox <- function(z, f) as.numeric(tapply(z, idx, f))
  patches <- data.frame(patch_id = seq_along(ids), n_cells = n, area_ha = W / 1e4, perimeter_m = perimeter,
    core_area_ha = sumby(w * core) / 1e4, core_index = 100 * sumby(w * core) / W,
    para = perimeter / W, shape_index = 0.25 * perimeter / sqrt(W),
    shape_circle = perimeter / (2 * sqrt(pi * W)),
    frac = ifelse(n > 1, 2 * log(0.25 * perimeter) / log(W), NA_real_),
    centroid_x = cx, centroid_y = cy, lon = NA_real_, lat = NA_real_,
    gyration_m = sqrt(tr), elongation = ifelse(l1 > 0, 1 - sqrt(l2 / l1), 0), orientation_deg = NA_real_,
    xmin = bbox(geom$x[cells], min) - half[1], xmax = bbox(geom$x[cells], max) + half[1],
    ymin = bbox(geom$y[cells], min) - half[2], ymax = bbox(geom$y[cells], max) + half[2],
    enn_m = NA_real_)
  # Orientation: azimuth (0 = north, clockwise) of the major axis.
  patches$orientation_deg <- ifelse(l1 <= 0, NA_real_, ifelse(abs(sxy) < 1e-12 * tr, ifelse(sxx >= syy, 90, 0),
                                    (atan2(sxy, l1 - sxx) * 180 / pi) %% 180))
  if (geom$lonlat) { patches$lon <- cx; patches$lat <- cy } else {
    ll <- sf::st_coordinates(sf::st_transform(sf::st_as_sf(data.frame(x = cx, y = cy), coords = c("x", "y"),
            crs = sf::st_crs(geom$crs_wkt)), 4326))
    patches$lon <- ll[, 1]; patches$lat <- ll[, 2]
  }
  polygons <- NULL
  if (vectorize) {
    pr <- m; terra::values(pr) <- as.vector(t(matrix(match(P, ids), geom$nrow, geom$ncol)))
    polygons <- .vectorize_raster(pr, dissolve = TRUE)
    names(polygons)[1] <- "patch_id"
    polygons <- polygons[order(polygons$patch_id), ]
    if (nrow(polygons) > 1) patches$enn_m[polygons$patch_id] <- .nearest_distance(polygons)
    polygons <- merge(polygons, patches, by = "patch_id", sort = TRUE)
  }
  list(patches = patches, class_edge = class_edge, polygons = polygons)
}

.fragment_summary <- function(p, land_ha, class_edge, year, code) {
  a <- p$area_ha; n <- length(a); s <- stats::sd(a)
  q <- stats::quantile(a, c(.1, .25, .75, .9), names = FALSE)
  skew <- if (n >= 3 && is.finite(s) && s > 0) mean((a - mean(a))^3) / mean((a - mean(a))^2)^1.5 else NA_real_
  gini <- if (n > 1) { z <- sort(a); sum((2 * seq_len(n) - n - 1) * z) / (n * sum(z)) } else 0
  enn <- p$enn_m[is.finite(p$enn_m)]
  data.frame(year = year, code = code, landscape_area_ha = land_ha, class_area_ha = sum(a),
    pland = 100 * sum(a) / land_ha, n_patches = n, patch_density = 100 * n / land_ha,
    edge_m = class_edge, edge_density = class_edge / land_ha,
    area_mean = mean(a), area_median = stats::median(a), area_sd = if (n > 1) s else NA_real_,
    area_cv = if (n > 1) 100 * s / mean(a) else NA_real_, area_min = min(a), area_max = max(a),
    area_q10 = q[1], area_q25 = q[2], area_q75 = q[3], area_q90 = q[4],
    area_skewness = skew, area_gini = gini, lpi = 100 * max(a) / land_ha,
    mesh_ha = sum(a^2) / land_ha, shape_circle_am = sum(a * p$shape_circle) / sum(a),
    frac_mean = mean(p$frac, na.rm = TRUE), gyration_am = sum(a * p$gyration_m) / sum(a),
    core_area_ha = sum(p$core_area_ha), enn_mean = if (length(enn)) mean(enn) else NA_real_,
    enn_median = if (length(enn)) stats::median(enn) else NA_real_)
}

# landscapemetrics on the same native grid (no reprojection).
.fragment_lsm <- function(r, v, codes, merge_classes, lsm, directions, year, lonlat) {
  unit_free <- paste0("lsm_c_", c("ai", "cohesion", "clumpy", "pland", "np", "pladj", "iji", "division", "split", "nlsi", "lsi"))
  if (lonlat && any(!lsm %in% unit_free))
    warning("M\u00e9tricas lsm com unidades de \u00e1rea ou dist\u00e2ncia n\u00e3o s\u00e3o v\u00e1lidas em CRS geogr\u00e1fico; use as m\u00e9tricas nativas de fragment_metrics().", call. = FALSE)
  x <- r
  if (merge_classes) {
    vv <- ifelse(is.na(v), NA, ifelse(v %in% codes, 1, 0)); codes <- 1
    terra::values(x) <- as.vector(t(vv))
  }
  res <- withCallingHandlers(
    landscapemetrics::calculate_lsm(x, what = lsm, directions = directions, progress = FALSE),
    warning = function(w) if (grepl("projected|units|degree|meter|check_landscape", conditionMessage(w), ignore.case = TRUE)) invokeRestart("muffleWarning"))
  res <- as.data.frame(res)
  res <- res[res$class %in% codes, c("class", "metric", "value"), drop = FALSE]
  if (!nrow(res)) return(NULL)
  cbind(year = year, res)
}

.fragment_plots <- function(out, language = "pt") {
  md <- out$metadata; dic <- out$class_dictionary
  if (isTRUE(md$merge_classes)) {
    keys <- "1"; merged <- md$merged_codes
    lab <- stats::setNames(paste(.tr(language, "Classes", "Classes"), paste(merged, collapse = "+")), keys)
    pal <- stats::setNames(dic$color[match(merged[1], dic$code)], keys)
  } else {
    keys <- as.character(dic$code)
    lab <- stats::setNames(paste(dic$code, if (language == "pt") dic$classe else dic$class), keys)
    pal <- stats::setNames(dic$color, keys)
  }
  class_name <- .tr(language, "Classe", "Class")
  fill <- ggplot2::scale_fill_manual(values = pal, labels = lab, name = class_name)
  colour <- ggplot2::scale_colour_manual(values = stats::setNames(.series_ink(pal), keys), labels = lab, name = class_name)
  p <- out$patches; p$group <- factor(as.character(p$code), levels = keys)
  multi_year <- length(unique(p$year)) > 1
  by_year <- if (multi_year) ggplot2::facet_wrap(~year) else NULL
  area_lab <- .tr(language, "\u00c1rea do fragmento (ha, escala log)", "Fragment area (ha, log scale)")

  size_distribution <- ggplot2::ggplot(p, ggplot2::aes(area_ha, fill = group)) +
    ggplot2::geom_histogram(bins = 25, colour = "white", linewidth = .2, position = "stack") +
    ggplot2::scale_x_log10() + fill + by_year + .series_theme() +
    ggplot2::labs(x = area_lab, y = .tr(language, "N\u00famero de fragmentos", "Number of fragments"))

  p <- p[order(p$year, p$group, -p$area_ha), ]
  p$rank <- stats::ave(p$area_ha, p$year, p$group, FUN = seq_along)
  rank_size <- ggplot2::ggplot(p, ggplot2::aes(rank, area_ha, colour = group)) +
    ggplot2::geom_line(linewidth = .8) + ggplot2::geom_point(size = 1.3) +
    ggplot2::scale_x_log10() + ggplot2::scale_y_log10() + colour + by_year + .series_theme() +
    ggplot2::labs(x = .tr(language, "Posi\u00e7\u00e3o (maior = 1, escala log)", "Rank (largest = 1, log scale)"),
                  y = area_lab,
                  caption = .tr(language, "Curvas mais \u00edngremes indicam domin\u00e2ncia de poucos fragmentos grandes.",
                                "Steeper curves indicate dominance by a few large fragments."))

  shape <- ggplot2::ggplot(p, ggplot2::aes(area_ha, shape_circle, fill = group)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "22", colour = "grey40") +
    ggplot2::geom_point(shape = 21, colour = "grey20", size = 2.2, alpha = .85) +
    ggplot2::scale_x_log10() + fill + by_year + .series_theme() +
    ggplot2::labs(x = area_lab, y = .tr(language, "Forma (per\u00edmetro / per\u00edmetro do c\u00edrculo)", "Shape (perimeter / circle perimeter)"),
                  caption = .tr(language, "Tracejado: c\u00edrculo (forma mais compacta).", "Dashed: circle (most compact shape)."))

  map <- NULL
  if (inherits(out$polygons, "sf")) {
    last <- max(out$polygons$year, na.rm = TRUE)
    poly <- out$polygons[is.na(out$polygons$year) | out$polygons$year == last, ]
    poly$group <- factor(as.character(poly$code), levels = keys)
    cen <- p[is.na(p$year) | p$year == last, ]
    cen <- sf::st_as_sf(cen, coords = c("centroid_x", "centroid_y"), crs = sf::st_crs(poly))
    map <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = poly, ggplot2::aes(fill = group), colour = "grey25", linewidth = .15) +
      ggplot2::geom_sf(data = cen, ggplot2::aes(size = area_ha), shape = 21, fill = "white", colour = "grey10", stroke = .4) +
      fill + ggplot2::scale_x_continuous(n.breaks = 4) + ggplot2::scale_size_area(max_size = 5, name = .tr(language, "\u00c1rea (ha)", "Area (ha)")) +
      .series_theme() + ggplot2::theme(axis.title = ggplot2::element_blank()) +
      ggplot2::labs(subtitle = if (is.finite(last)) last else NULL,
                    caption = .tr(language, "Pontos: centroides ponderados pela \u00e1rea, no CRS herdado.",
                                  "Points: area-weighted centroids, in the inherited CRS."))
  }

  trend <- NULL
  s <- out$summary
  if (length(unique(s$year)) > 1) {
    metrics <- c(n_patches = .tr(language, "N\u00famero de fragmentos", "Number of fragments"),
                 area_mean = .tr(language, "\u00c1rea m\u00e9dia (ha)", "Mean area (ha)"),
                 lpi = .tr(language, "Maior fragmento (% da paisagem)", "Largest patch (% of landscape)"),
                 edge_density = .tr(language, "Densidade de borda (m/ha)", "Edge density (m/ha)"))
    long <- do.call(rbind, lapply(names(metrics), function(m) data.frame(year = s$year,
      group = factor(as.character(s$code), levels = keys), metric = factor(metrics[[m]], levels = metrics), value = s[[m]])))
    trend <- ggplot2::ggplot(long, ggplot2::aes(year, value, colour = group)) +
      ggplot2::geom_line(linewidth = .9) + ggplot2::geom_point(size = 1.5) +
      ggplot2::facet_wrap(~metric, scales = "free_y") + colour + .series_theme() +
      ggplot2::labs(x = .tr(language, "Ano", "Year"), y = NULL)
  }
  list(size_distribution = size_distribution, rank_size = rank_size, shape = shape, map = map, trend = trend)
}

# Edge-to-edge geodesic distance (sphere) from each polygon to its nearest other
# polygon. Candidates come from the s2 index within a growing radius, so only
# nearby pairs are measured (no pairwise distance matrix); the result is exact.
.nearest_distance <- function(polygons, start = 250) {
  g <- sf::st_as_s2(sf::st_transform(sf::st_geometry(polygons), 4326))
  out <- rep(NA_real_, length(g)); todo <- seq_along(g); radius <- start
  while (length(todo) && radius < 4e7) {
    cand <- s2::s2_dwithin_matrix(g[todo], g, radius)
    from <- rep(todo, lengths(cand)); to <- unlist(cand)
    keep <- from != to
    if (any(keep)) {
      d <- s2::s2_distance(g[from[keep]], g[to[keep]])
      best <- tapply(d, from[keep], min)
      out[as.integer(names(best))] <- best
    }
    todo <- todo[is.na(out[todo])]; radius <- radius * 4
  }
  out
}
