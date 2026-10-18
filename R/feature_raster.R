#' Rasters of distance, density and extent of features on the land-cover grid
#'
#' Builds raster surfaces of a feature (roads, rivers, buildings, places...)
#' on the grid of a maps object, so they combine cell by cell with the
#' land-cover analyses. The metrics depend on the geometry type:
#' \itemize{
#'   \item points: `distance` (m to the nearest point), `count` (points per
#'     cell) and `density` (points per km2 in the window);
#'   \item lines: `distance` (m), `length` (m of line per cell, geodesic) and
#'     `density` (km of line per km2 in the window);
#'   \item polygons: `distance` (m to the nearest polygon, 0 inside),
#'     `coverage` (fraction of the cell covered), `area` (m2 per cell) and
#'     `density` (fraction of the window covered).
#' }
#' Computations use a grid extended to the features' extent, so features just
#' outside the study area (see `buffer_m` in [load_features()]) count for
#' distances and densities near the border; results are cropped back to the
#' reference grid and masked to the study area. Distances are measured between
#' cell centres on the grid (geodesic for geographic grids); the density window
#' is a square of side `2 * window_m` centred on each cell, and densities divide
#' by the true (geodesic) area of the window.
#' @param features An `sf` object of one geometry type or the result of
#'   [load_features()] (choose the layer with `feature`).
#' @param maps Maps object ([load_rasters()] or [load_lulc()]) giving the grid.
#' @param feature Name of the feature when `features` comes from
#'   [load_features()].
#' @param year Year of the reference grid; `NULL` uses the last year.
#' @param metrics Metrics to compute; `NULL` computes all metrics of the type.
#' @param window_m Half side of the density window, in metres (default 500).
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_feature_raster` list: `rasters` (named list of
#'   in-memory `RasterLayer`), `metrics` (metric, unit, description),
#'   `metadata` (feature, geometry, number of features, window, source and
#'   catalogue year when available) and `plots`.
#' @examples
#' \dontrun{
#' load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"))
#' vias <- load_features(mapas, "roads", source = "overture")
#' r <- feature_raster(vias, mapas, feature = "roads")
#' r$plots$distance
#' }
#' @export
feature_raster <- function(features, maps, feature = NULL, year = NULL, metrics = NULL, window_m = 500,
                           language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  if (!is.numeric(window_m) || length(window_m) != 1L || window_m <= 0) stop("window_m deve ser positivo (metros).")
  inp <- .feature_input(features, feature)
  ref <- .reference_raster(maps, year); r0 <- ref$raster
  family <- .geometry_family(inp$sf)
  allowed <- list(point = c("distance", "count", "density"), line = c("distance", "length", "density"),
                  polygon = c("distance", "coverage", "area", "density"))[[family]]
  if (is.null(metrics)) metrics <- allowed
  if (!all(metrics %in% allowed)) stop(sprintf("M\u00e9tricas v\u00e1lidas para %s: %s.", family, paste(allowed, collapse = ", ")))
  g <- sf::st_transform(inp$sf, sf::st_crs(terra::crs(r0)))
  tmpl <- .extended_template(r0, g)
  v <- terra::vect(g)
  u <- if (terra::is.lonlat(r0)) 1 else .crs_metres(r0)
  cell_m2 <- terra::cellSize(tmpl, unit = "m")
  measure <- switch(family,
    point = terra::rasterize(v, tmpl, fun = "count", background = 0),
    line = terra::rasterizeGeom(v, tmpl, fun = "length") * u,
    polygon = terra::rasterize(v, tmpl, cover = TRUE, background = 0) * cell_m2)
  out <- list()
  if ("distance" %in% metrics) {
    present <- if (family == "polygon") terra::rasterize(v, tmpl, touches = FALSE)
      else terra::rasterize(v, tmpl, touches = TRUE)
    if (family == "polygon") present <- terra::cover(present, terra::ifel(measure > 0, 1, NA))
    out$distance <- if (all(is.na(terra::values(present)))) terra::init(tmpl, NA) else terra::distance(present) * u
  }
  if ("count" %in% metrics) out$count <- measure
  if ("length" %in% metrics) out$length <- measure
  if ("coverage" %in% metrics) out$coverage <- measure / cell_m2
  if ("area" %in% metrics) out$area <- measure
  if ("density" %in% metrics) {
    k <- .window_cells(tmpl, window_m)
    num <- .box_sum(measure, k); den <- .box_sum(cell_m2, k)
    out$density <- switch(family, point = num / (den / 1e6), line = (num / 1000) / (den / 1e6), polygon = num / den)
  }
  info <- .feature_metric_info(family, language)
  rasters <- lapply(names(out), function(m) {
    r <- terra::mask(terra::crop(out[[m]], r0, snap = "near"), r0)
    names(r) <- m
    .to_rasterlayer(r, m)
  })
  names(rasters) <- names(out)
  res <- list(rasters = rasters, metrics = info[info$metric %in% names(out), , drop = FALSE],
              metadata = c(list(feature = inp$name, geometry = family, n_features = nrow(g), window_m = window_m,
                                year = ref$year, crs = terra::crs(r0)), inp$meta),
              plots = NULL)
  class(res) <- "biomastats_feature_raster"
  if (isTRUE(build_plot)) {
    cap <- if (!is.null(inp$meta$product)) sprintf("%s \u00b7 %s %s", inp$meta$product,
             if (identical(language, "en")) "catalogue" else "cat\u00e1logo", inp$meta$catalog_year %||% "") else NULL
    res$plots <- lapply(names(rasters), function(m) {
      lab <- sprintf("%s (%s)", info$label[info$metric == m], info$unit[info$metric == m])
      .layer_raster_plot(rasters[[m]], maps$shape, lab, language, caption = cap)
    })
    names(res$plots) <- names(rasters)
  }
  res
}

# Normalises the feature input: sf of one feature, with its source metadata.
.feature_input <- function(features, feature = NULL) {
  if (inherits(features, "biomastats_features")) {
    if (is.null(feature)) {
      if (length(features$features) != 1L) stop("Informe feature: um dos nomes em features$features.")
      feature <- names(features$features)
    }
    g <- features$features[[feature]]
    if (is.null(g) || !nrow(g)) stop(sprintf("A fei\u00e7\u00e3o '%s' n\u00e3o est\u00e1 dispon\u00edvel (bloco ignorado ou vazio).", feature))
    st <- features$status[features$status$feature == feature, ]
    return(list(sf = g, name = feature, meta = list(source = st$source, product = st$product,
                                                   version = st$version, catalog_year = st$catalog_year)))
  }
  if (!inherits(features, c("sf", "sfc"))) stop("features deve ser um objeto sf ou o resultado de load_features().")
  g <- sf::st_as_sf(features)
  if (!nrow(g)) stop("features n\u00e3o tem geometrias.")
  list(sf = g, name = feature %||% "feature", meta = list())
}

# Reference grid extended (on the same cell lattice) to cover the features.
.extended_template <- function(r0, g) {
  bb <- sf::st_bbox(g); e0 <- terra::ext(r0)
  e <- terra::ext(min(e0$xmin, bb[["xmin"]]), max(e0$xmax, bb[["xmax"]]), min(e0$ymin, bb[["ymin"]]), max(e0$ymax, bb[["ymax"]]))
  t <- terra::extend(terra::rast(r0), e, snap = "out")
  terra::init(t, NA)
}

# Half-window in cells (rows, columns) for a metric distance on the grid.
.window_cells <- function(r, window_m) {
  res <- terra::res(r)
  if (terra::is.lonlat(r)) {
    lat <- mean(c(terra::ext(r)$ymin, terra::ext(r)$ymax)) * pi / 180
    a <- 6378137; e2 <- 0.00669438
    mx <- pi / 180 * a * cos(lat) / sqrt(1 - e2 * sin(lat)^2)
    my <- pi / 180 * a * (1 - e2) / (1 - e2 * sin(lat)^2)^1.5
  } else mx <- my <- .crs_metres(r)
  c(rows = max(0, round(window_m / (res[2] * my))), cols = max(0, round(window_m / (res[1] * mx))))
}

# Moving-window sum with a summed-area table (constant cost per cell).
.box_sum <- function(r, k) {
  m <- matrix(terra::values(r, mat = FALSE), terra::nrow(r), terra::ncol(r), byrow = TRUE)
  m[is.na(m)] <- 0
  nr <- nrow(m); nc <- ncol(m)
  s <- matrix(0, nr + 1, nc + 1)
  cs <- matrix(apply(m, 2, cumsum), nr, nc)
  s[-1, -1] <- matrix(t(apply(cs, 1, cumsum)), nr, nc)
  i1 <- pmax(1, seq_len(nr) - k[["rows"]]); i2 <- pmin(nr, seq_len(nr) + k[["rows"]])
  j1 <- pmax(1, seq_len(nc) - k[["cols"]]); j2 <- pmin(nc, seq_len(nc) + k[["cols"]])
  out <- s[i2 + 1, j2 + 1] - s[i1, j2 + 1] - s[i2 + 1, j1] + s[i1, j1]
  terra::setValues(terra::rast(r), as.vector(t(out)))
}

.feature_metric_info <- function(family, language) {
  en <- identical(language, "en")
  d <- data.frame(
    metric = c("distance", "count", "length", "coverage", "area", "density"),
    unit = c("m", if (en) "points/cell" else "pontos/c\u00e9lula", "m", if (en) "fraction" else "fra\u00e7\u00e3o", "m\u00b2",
             switch(family, point = if (en) "points/km\u00b2" else "pontos/km\u00b2", line = "km/km\u00b2",
                    polygon = if (en) "fraction" else "fra\u00e7\u00e3o")),
    label = if (en) c("Distance to the nearest feature", "Points", "Line length", "Cover", "Feature area", "Density")
            else c("Dist\u00e2ncia at\u00e9 a fei\u00e7\u00e3o mais pr\u00f3xima", "Pontos", "Comprimento de linhas", "Cobertura",
                   "\u00c1rea da fei\u00e7\u00e3o", "Densidade"),
    stringsAsFactors = FALSE)
  d
}
