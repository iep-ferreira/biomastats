#' Distance to the nearest OpenStreetMap feature
#'
#' Downloads an OpenStreetMap feature within the extent of a reference raster,
#' rasterizes all matching point, line, and polygon geometries, and calculates
#' the straight-line distance from each valid reference cell to the nearest
#' feature cell.
#'
#' OSM uses an open key/value tagging system rather than a closed feature list.
#' Common keys applicable to distance calculations include `highway`,
#' `railway`, `waterway`, `barrier`, and `power` for predominantly linear
#' features; `landuse`, `leisure`, `building`, and `natural` for predominantly
#' areal features; and `amenity`, `shop`, `tourism`, and `place` for point or
#' mixed geometries. Use [osmdata::available_features()] to retrieve current
#' keys and [osmdata::available_tags()] to inspect values for a selected key.
#' These discovery functions require network access.
#'
#' Distances are returned in metres for longitude/latitude rasters and in the
#' map units of projected rasters. Cells that are `NA` in `reference_raster`
#' remain `NA` in the result.
#'
#' @param reference_raster A `RasterLayer` defining the analysis extent,
#'   resolution, CRS, and valid-cell mask.
#' @param key_feature A non-empty OSM key such as `"highway"`, `"waterway"`,
#'   `"natural"`, or `"amenity"`.
#' @param value_feature An optional OSM value such as `"primary"`, `"river"`,
#'   `"water"`, or `"hospital"`. Use `NULL` to match every value for the key;
#'   a specific value is recommended to avoid very large Overpass queries.
#' @param provider Data provider. Only `"osm"` is currently supported.
#'
#' @return A list with one element, `raster`, containing a `RasterLayer` of
#'   distances to the nearest matching OSM feature.
#' @export
#' @importFrom raster distance getValues projection setValues
#'
#' @examples
#' \dontrun{
#' road_distance <- distance_to_feature(
#'   reference_raster,
#'   key_feature = "highway",
#'   value_feature = "primary"
#' )
#'
#' water_distance <- distance_to_feature(
#'   reference_raster,
#'   key_feature = "natural",
#'   value_feature = "water"
#' )
#' }
distance_to_feature <- function(reference_raster, key_feature,
                                value_feature = NULL, provider = "osm") {
  if (!inherits(reference_raster, "RasterLayer")) {
    stop("'reference_raster' must be a raster::RasterLayer.", call. = FALSE)
  }

  raster_crs <- raster::projection(reference_raster)
  if (is.na(raster_crs) || !nzchar(raster_crs)) {
    stop(
      "'reference_raster' must have a CRS so distance units are defined.",
      call. = FALSE
    )
  }

  reference_values <- raster::getValues(reference_raster)
  if (!any(!is.na(reference_values))) {
    stop("'reference_raster' must contain at least one valid cell.",
         call. = FALSE)
  }

  raster_distance <- load_osm_data(
    reference_raster = reference_raster,
    key_feature = key_feature,
    value_feature = value_feature,
    provider = provider
  )

  if (!inherits(raster_distance, "RasterLayer")) {
    stop("The rasterized OSM feature must be a single RasterLayer.",
         call. = FALSE)
  }

  feature_values <- raster::getValues(raster_distance)
  if (!any(!is.na(feature_values))) {
    stop("The rasterized OSM feature contains no occupied cells.",
         call. = FALSE)
  }

  distances <- raster::distance(raster_distance)
  distance_values <- raster::getValues(distances)

  # Preserve the study-area mask from the reference raster.
  distance_values[is.na(reference_values)] <- NA_real_
  raster_distance_result <- raster::setValues(
    reference_raster,
    distance_values
  )

  list(raster = raster_distance_result)
}
