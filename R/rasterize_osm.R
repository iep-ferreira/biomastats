#' Rasterize OSM geometries on a reference raster
#'
#' Converts point, line, multiline, polygon, and multipolygon geometries
#' returned by `osmdata` to
#' rasters aligned with `reference_raster`. Every OSM feature cell receives
#' value `1`. When more than one geometry type is available, polygons are
#' retained first, followed by lines and points.
#'
#' @param osm_data An `osmdata` object, usually returned by
#'   [osmdata::osmdata_sf()].
#' @param reference_raster A `RasterLayer` used to define the extent,
#'   resolution, and coordinate reference system of the result.
#'
#' @return A `RasterLayer` containing the rasterized OSM geometries, or
#'   `NULL` when `osm_data` contains no supported features.
#' @importFrom raster cover rasterize
#' @importFrom sf st_crs st_transform
#' @export
#'
#' @examples
#' \dontrun{
#' osm <- osmdata::osmdata_sf(query)
#' rasterize_osm(osm, reference_raster)
#' }
rasterize_osm <- function(osm_data, reference_raster) {
  if (!inherits(reference_raster, "RasterLayer")) {
    stop("'reference_raster' must be a raster::RasterLayer.", call. = FALSE)
  }
  target_crs <- raster::projection(reference_raster)
  if (is.na(target_crs) || !nzchar(target_crs)) {
    stop("'reference_raster' must have a CRS before rasterization.",
         call. = FALSE)
  }

  prepare_geometry <- function(features) {
    if (is.na(sf::st_crs(features))) {
      stop("OSM geometries must have a CRS before rasterization.",
           call. = FALSE)
    }
    sf::st_transform(features, crs = target_crs)
  }

  # Rasterize each geometry collection only when it has features.
  raster_points <- NULL
  if (!is.null(osm_data$osm_points) && nrow(osm_data$osm_points) > 0) {
    raster_points <- raster::rasterize(
      prepare_geometry(osm_data$osm_points),
      reference_raster,
      field = 1
    )
  }

  raster_lines <- NULL
  if (!is.null(osm_data$osm_lines) && nrow(osm_data$osm_lines) > 0) {
    raster_lines <- raster::rasterize(
      prepare_geometry(osm_data$osm_lines),
      reference_raster,
      field = 1
    )
  }

  raster_multilines <- NULL
  if (!is.null(osm_data$osm_multilines) &&
      nrow(osm_data$osm_multilines) > 0) {
    raster_multilines <- raster::rasterize(
      prepare_geometry(osm_data$osm_multilines),
      reference_raster,
      field = 1
    )
  }

  raster_polygons <- NULL
  if (!is.null(osm_data$osm_polygons) && nrow(osm_data$osm_polygons) > 0) {
    raster_polygons <- raster::rasterize(
      prepare_geometry(osm_data$osm_polygons),
      reference_raster,
      field = 1
    )
  }

  raster_multipolygons <- NULL
  if (!is.null(osm_data$osm_multipolygons) &&
      nrow(osm_data$osm_multipolygons) > 0) {
    raster_multipolygons <- raster::rasterize(
      prepare_geometry(osm_data$osm_multipolygons),
      reference_raster,
      field = 1
    )
  }

  # Combine rasters in the documented priority order.
  combined_raster <- raster_polygons
  if (!is.null(raster_multipolygons)) {
    combined_raster <- if (is.null(combined_raster)) {
      raster_multipolygons
    } else {
      raster::cover(combined_raster, raster_multipolygons)
    }
  }

  combined_lines <- raster_lines
  if (!is.null(raster_multilines)) {
    combined_lines <- if (is.null(combined_lines)) {
      raster_multilines
    } else {
      raster::cover(combined_lines, raster_multilines)
    }
  }

  if (!is.null(combined_lines)) {
    combined_raster <- if (is.null(combined_raster)) {
      combined_lines
    } else {
      raster::cover(combined_raster, combined_lines)
    }
  }
  if (!is.null(raster_points)) {
    combined_raster <- if (is.null(combined_raster)) {
      raster_points
    } else {
      raster::cover(combined_raster, raster_points)
    }
  }

  combined_raster
}
