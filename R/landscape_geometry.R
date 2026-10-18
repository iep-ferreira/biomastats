# Internal geometry of a categorical grid in its native CRS.
#
# No raster is reprojected. Cell areas come from terra::cellSize (geodesic for
# geographic grids; true area for projected grids). Edge lengths are computed
# on the WGS84 ellipsoid for geographic grids (parallel and meridian arcs of
# each row) and, for projected grids, from the cell size in metres corrected by
# the local scale factor k = sqrt(true area / planar area), exact for conformal
# projections such as UTM.

.as_terra <- function(r) if (inherits(r, "SpatRaster")) r else terra::rast(r)

.crs_metres <- function(r) {
  crs <- sf::st_crs(terra::crs(r))
  u <- crs$ud_unit
  if (is.null(u)) return(1)
  as.numeric(units::set_units(u, "m"))
}

.grid_geometry <- function(r) {
  r <- .as_terra(r)
  if (!nzchar(terra::crs(r))) stop("O raster precisa de CRS definido.")
  nr <- terra::nrow(r); nc <- terra::ncol(r); res <- terra::res(r)
  x <- terra::xFromCol(r, seq_len(nc)); y <- terra::yFromRow(r, seq_len(nr))
  area <- matrix(terra::values(terra::cellSize(r, unit = "m", mask = FALSE), mat = FALSE),
                 nrow = nr, ncol = nc, byrow = TRUE)
  lonlat <- terra::is.lonlat(r)
  if (lonlat) {
    a <- 6378137; f <- 1 / 298.257223563; e2 <- f * (2 - f)
    rad <- pi / 180
    prime <- function(phi) a / sqrt(1 - e2 * sin(phi)^2)                   # N
    meridional <- function(phi) a * (1 - e2) / (1 - e2 * sin(phi)^2)^1.5   # M
    top <- (y + res[2] / 2) * rad; bottom <- (y - res[2] / 2) * rad; mid <- y * rad
    north <- prime(top) * cos(top) * res[1] * rad
    south <- prime(bottom) * cos(bottom) * res[1] * rad
    side <- meridional(mid) * res[2] * rad
    edge_n <- matrix(north, nr, nc); edge_s <- matrix(south, nr, nc); edge_ew <- matrix(side, nr, nc)
    kx <- matrix(prime(mid) * cos(mid) * rad, nr, nc)   # metres per degree of longitude
    ky <- matrix(meridional(mid) * rad, nr, nc)         # metres per degree of latitude
  } else {
    u <- .crs_metres(r)
    k <- sqrt(area / (res[1] * res[2] * u^2))
    edge_n <- edge_s <- res[1] * u * k; edge_ew <- res[2] * u * k
    kx <- ky <- u * k
  }
  list(lonlat = lonlat, nrow = nr, ncol = nc, res = res, area = area,
       edge_n = edge_n, edge_s = edge_s, edge_ew = edge_ew,
       x = matrix(x, nr, nc, byrow = TRUE), y = matrix(y, nr, nc), kx = kx, ky = ky,
       crs_wkt = terra::crs(r))
}

# UTM zone and hemisphere of the grid centre; only the centre point is transformed.
.utm_from_raster <- function(r) {
  r <- .as_terra(r)
  e <- terra::ext(r)
  xy <- c(mean(c(e$xmin, e$xmax)), mean(c(e$ymin, e$ymax)))
  if (!terra::is.lonlat(r)) {
    pt <- sf::st_sfc(sf::st_point(xy), crs = sf::st_crs(terra::crs(r)))
    xy <- as.numeric(sf::st_coordinates(sf::st_transform(pt, 4326)))
  }
  zone <- floor((xy[1] + 180) / 6) + 1
  hemisphere <- if (xy[2] < 0) "south" else "north"
  list(zone = as.character(zone), hemisphere = hemisphere,
       epsg = (if (hemisphere == "south") 32700 else 32600) + zone,
       centre_lon = xy[1], centre_lat = xy[2])
}

# Neighbour of every cell in a matrix (NA outside the grid).
.shift <- function(m, dr, dc) {
  nr <- nrow(m); nc <- ncol(m)
  out <- matrix(NA, nr, nc)
  r0 <- max(1, 1 + dr); r1 <- min(nr, nr + dr); c0 <- max(1, 1 + dc); c1 <- min(nc, nc + dc)
  if (r0 > r1 || c0 > c1) return(out)
  out[(r0:r1) - dr, (c0:c1) - dc] <- m[r0:r1, c0:c1]
  out
}
