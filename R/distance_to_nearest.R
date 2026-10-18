#' Minimum distance from coordinates to features or land-cover classes
#'
#' Computes, for each point, the geodesic distance (metres) to the nearest
#' feature of a layer or to the nearest area of one or more land-cover classes.
#' The nearest feature is found with a spatial index (s2), so it scales to many
#' points and features. Works worldwide.
#' \itemize{
#'   \item Features: an `sf` object or the result of [load_features()]
#'     (choose the layer with `feature`). Distance is 0 inside polygons.
#'   \item Land cover: a maps object ([load_rasters()] or [load_lulc()]) with
#'     `classes`; the class cells of `year` are vectorized with the package
#'     routine (as in [reclass_to_polygons()]) and the distance is 0 inside the
#'     class. Only the classes inside the study area are considered.
#' }
#' @param points Coordinates: an `sf` object of points, or a data frame or
#'   matrix with longitude/latitude columns (`lon`/`lat`, `longitude`/`latitude`
#'   or `x`/`y`, in the CRS given by `crs`).
#' @param target Features (`sf` or [load_features()] result) or a maps object.
#' @param feature Feature name when `target` comes from [load_features()].
#' @param classes Class codes when `target` is a maps object.
#' @param year Year of the land-cover map; `NULL` uses the last year.
#' @param crs CRS of tabular coordinates (default 4326).
#' @return A data frame with `id`, `lon`, `lat`, `target`, `distance_m`,
#'   `nearest` (row of the nearest feature or class polygon) and `inside`
#'   (distance 0). The attribute `metadata` records the source, catalogue year
#'   or classes and year used.
#' @examples
#' load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"))
#' pts <- data.frame(lon = c(-48.53, -48.52), lat = c(-23.60, -23.61))
#' distance_to_nearest(pts, mapas, classes = 3)
#' @export
distance_to_nearest <- function(points, target, feature = NULL, classes = NULL, year = NULL, crs = 4326) {
  p <- .points_input(points, crs)
  if (inherits(target, c("biomastats_features", "sf", "sfc"))) {
    inp <- .feature_input(target, feature)
    g <- sf::st_transform(sf::st_geometry(inp$sf), 4326)
    label <- inp$name
    meta <- c(list(type = "feature"), inp$meta)
  } else {
    if (is.null(classes)) stop("Informe classes ao usar um objeto de mapas como alvo.")
    ref <- .reference_raster(target, year)
    r <- ref$raster
    m <- terra::rast(r)
    terra::values(m) <- ifelse(terra::values(r, mat = FALSE) %in% classes, 1, NA)
    if (all(is.na(terra::values(m)))) stop("Nenhuma c\u00e9lula das classes informadas no ano escolhido.")
    g <- sf::st_transform(sf::st_geometry(.vectorize_raster(m, dissolve = TRUE)), 4326)
    g <- sf::st_cast(sf::st_union(g), "POLYGON")
    label <- paste(classes, collapse = "+")
    meta <- list(type = "land_cover", classes = classes, year = ref$year, collection = target$collection)
  }
  nn <- sf::st_nearest_feature(p, g)
  d <- as.numeric(sf::st_distance(p, g[nn], by_element = TRUE))
  xy <- sf::st_coordinates(p)
  out <- data.frame(id = p$id, lon = xy[, 1], lat = xy[, 2], target = label, distance_m = d, nearest = nn,
                    inside = d == 0, stringsAsFactors = FALSE)
  attr(out, "metadata") <- meta
  out
}

.points_input <- function(points, crs) {
  if (inherits(points, c("sf", "sfc"))) {
    p <- sf::st_transform(sf::st_as_sf(points), 4326)
    if (!all(as.character(sf::st_geometry_type(p)) == "POINT")) stop("points deve conter apenas pontos.")
    if (is.null(p$id)) p$id <- seq_len(nrow(p))
    return(p[, "id"])
  }
  d <- as.data.frame(points)
  nm <- tolower(names(d))
  pick <- function(opts) { i <- match(opts, nm); i <- i[!is.na(i)]; if (length(i)) i[1] else NA }
  ix <- pick(c("lon", "longitude", "x")); iy <- pick(c("lat", "latitude", "y"))
  if (is.na(ix) || is.na(iy)) { if (ncol(d) >= 2) { ix <- 1; iy <- 2 } else stop("points precisa de duas colunas de coordenadas.") }
  p <- sf::st_as_sf(data.frame(id = if ("id" %in% nm) d[[match("id", nm)]] else seq_len(nrow(d)),
                               x = d[[ix]], y = d[[iy]]), coords = c("x", "y"), crs = crs)
  sf::st_transform(p, 4326)
}
