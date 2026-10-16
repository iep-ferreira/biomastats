#' Land use and land cover (LULC) metrics on the native grid
#'
#' Reclassifies a raster to the class(es) of interest and computes landscape,
#' class and patch metrics directly on its native grid: no reprojection, CRS
#' inherited, UTM zone and hemisphere detected automatically. Areas use
#' geodesic cell areas and edges use ellipsoidal (geographic grids) or
#' scale-factor corrected (projected grids) lengths; see [fragment_metrics()].
#' The aggregation index uses landscapemetrics on the same grid (it depends
#' only on cell adjacencies). Tables keep the landscapemetrics layout
#' (`layer`, `level`, `class`, `id`, `metric`, `value`).
#'
#' @param biomastats_raster Raster of land-cover classes.
#' @param classe Class code or codes; several codes are merged into class 1.
#' @param zone,hemisphere Deprecated and ignored: the UTM zone and hemisphere
#'   are detected from the raster and reported in `utm`.
#' @param export.raster If `TRUE`, returns the reclassified raster.
#' @return A list with `classe`, `proj` (native CRS WKT), `utm` (detected zone,
#'   hemisphere and EPSG), `total_area` (ha), `area_dos_fragmentos` (ha per
#'   patch, background included), `edge_length` (m, edges between different
#'   classes, landscape boundary excluded), `edge_density` (m/ha),
#'   `total_area_for_the_class` (ha), `average_area`, `coeficient_of_variation`,
#'   `standard_deviation` (ha), `area_of_the_patches` (selected class),
#'   `index_of_aggregation` (%), `fragments` (detailed patch table of the
#'   selected class), `reclassified_raster` and `reclassified`.
#' @examples
#' \dontrun{
#' lulc_metrics(biomastats_raster, classe = 3)
#' }
#' @export
lulc_metrics <- function(biomastats_raster, classe, zone = NULL, hemisphere = NULL, export.raster = FALSE) {
  values <- raster::getValues(biomastats_raster)
  values[(!values %in% classe) & !is.na(values)] <- 0
  if (length(classe) > 1) values[values %in% classe] <- 1
  aux_raster <- raster::setValues(biomastats_raster, values)
  target <- if (length(classe) == 1) classe else 1

  r <- .as_terra(aux_raster); geom <- .grid_geometry(r)
  v <- matrix(terra::values(r, mat = FALSE), geom$nrow, geom$ncol, byrow = TRUE)
  land_ha <- sum(geom$area[!is.na(v)]) / 1e4
  tib <- function(level, class, id, metric, value)
    dplyr::tibble(layer = 1L, level = level, class = as.integer(class), id = as.integer(id), metric = metric, value = value)

  classes <- intersect(c(0, target), unique(v[!is.na(v)]))
  frag <- lapply(classes, function(k) .fragment_patches(r, v, k, geom, 8L, FALSE)$patches)
  offset <- cumsum(c(0, vapply(frag, nrow, 1L)))
  area_frag <- do.call(rbind, lapply(seq_along(classes), function(i)
    tib("patch", classes[i], offset[i] + frag[[i]]$patch_id, "area", frag[[i]]$area_ha)))
  per_class <- function(metric, f) do.call(rbind, lapply(seq_along(classes), function(i)
    tib("class", classes[i], NA, metric, f(frag[[i]]$area_ha))))

  # Landscape edge: shared edges between different classes, each counted once.
  right <- .shift(v, 0, 1); down <- .shift(v, 1, 0)
  te <- sum(geom$edge_ew[!is.na(v) & !is.na(right) & v != right]) +
        sum(geom$edge_s[!is.na(v) & !is.na(down) & v != down])

  ai <- withCallingHandlers(landscapemetrics::lsm_c_ai(aux_raster),
    warning = function(w) if (grepl("projected|units|degree|meter|check_landscape", conditionMessage(w), ignore.case = TRUE)) invokeRestart("muffleWarning"))
  frag_sizes <- area_frag[area_frag$class == target, ]

  list(classe = classe, proj = geom$crs_wkt, utm = .utm_from_raster(r),
       total_area = tib("landscape", NA, NA, "ta", land_ha),
       area_dos_fragmentos = area_frag,
       edge_length = tib("landscape", NA, NA, "te", te),
       edge_density = tib("landscape", NA, NA, "ed", te / land_ha),
       total_area_for_the_class = per_class("ca", sum),
       average_area = per_class("area_mn", mean),
       coeficient_of_variation = per_class("area_cv", function(a) if (length(a) > 1) 100 * stats::sd(a) / mean(a) else NA_real_),
       standard_deviation = per_class("area_sd", function(a) if (length(a) > 1) stats::sd(a) else NA_real_),
       area_of_the_patches = frag_sizes,
       index_of_aggregation = ai,
       fragments = frag[[match(target, classes)]],
       reclassified_raster = if (isTRUE(export.raster)) aux_raster else NULL,
       reclassified = ifelse(length(classe) == 1, "No", "Yes"))
}
