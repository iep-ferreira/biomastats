#' Load and rasterize an OpenStreetMap feature
#'
#' Queries OpenStreetMap for a feature inside the extent of a reference
#' raster, then rasterizes the returned geometries using the same extent and
#' resolution. The OSM query is made with `osmdata::opq()` and therefore may
#' require network access and an available Overpass provider.
#'
#' The current implementation supports OpenStreetMap through `osmdata` only.
#' The Overpass endpoint is selected by the `osmdata` configuration.
#'
#' @param reference_raster A `RasterLayer` defining the query bounding box
#'   and the output raster geometry.
#' @param key_feature A character string with the OSM feature key, such as
#'   `"highway"` or `"natural"`.
#' @param value_feature Optional OSM feature value, such as `"primary"` or
#'   `"water"`. If `NULL`, all values for `key_feature` are queried.
#' @param provider Data provider. Only `"osm"` is currently supported.
#'
#' @return A `Raster` object with the rasterized OSM feature aligned to
#'   `reference_raster`. Cells without OSM features are `NA`.
#' @export
#' @importFrom magrittr %>%
#' @importFrom osmdata add_osm_feature opq osmdata_sf
#' @importFrom raster extent getValues projection setValues
#' @importFrom sf st_as_sfc st_bbox st_set_crs st_transform
#'
#' @examples
#' \dontrun{
#' roads <- load_osm_data(reference_raster, key_feature = "highway")
#' water <- load_osm_data(
#'   reference_raster,
#'   key_feature = "natural",
#'   value_feature = "water"
#' )
#' }
load_osm_data <- function(reference_raster, key_feature, value_feature = NULL,
                          provider = "osm") {
  if (!inherits(reference_raster, "Raster")) {
    stop("'reference_raster' must be a raster::Raster object.", call. = FALSE)
  }
  raster_crs <- raster::projection(reference_raster)
  if (is.na(raster_crs) || !nzchar(raster_crs)) {
    stop(
      "'reference_raster' must have a CRS for an OpenStreetMap query.",
      call. = FALSE
    )
  }
  if (!is.character(key_feature) || length(key_feature) != 1L ||
      is.na(key_feature) || !nzchar(key_feature)) {
    stop("'key_feature' must be one non-empty character string.", call. = FALSE)
  }
  if (!is.character(provider) || length(provider) != 1L ||
      is.na(provider) || !nzchar(provider)) {
    stop("'provider' must be one non-empty character string.", call. = FALSE)
  }
  if (!identical(tolower(provider), "osm")) {
    stop("Only provider = 'osm' is currently supported.", call. = FALSE)
  }
  if (!is.null(value_feature) &&
      (!is.character(value_feature) || length(value_feature) != 1L ||
       is.na(value_feature) || !nzchar(value_feature))) {
    stop("'value_feature' must be NULL or one non-empty character string.",
         call. = FALSE)
  }

  # Overpass expects longitude/latitude coordinates. Convert a projected
  # raster extent to WGS84 before constructing the query.
  bbox_geometry <- sf::st_as_sfc(
    sf::st_bbox(raster::extent(reference_raster))
  )
  bbox_geometry <- sf::st_set_crs(bbox_geometry, raster_crs)
  bbox <- sf::st_bbox(sf::st_transform(bbox_geometry, 4326))
  query <- opq(bbox = bbox)
  if (is.null(value_feature)) {
    query <- add_osm_feature(query, key = key_feature)
  } else {
    query <- add_osm_feature(
      query,
      key = key_feature,
      value = value_feature
    )
  }
  osm_data <- osmdata_sf(query)

  # Set all occupied cells in the reference to zero before rasterization.
  valores <- raster::getValues(reference_raster)
  valores[!is.na(valores)] <- 0
  reference_raster <- raster::setValues(reference_raster, valores)

  raster_osm <- rasterize_osm(osm_data, reference_raster)
  if (is.null(raster_osm)) {
    stop(
      "No OSM features were found for the requested key/value in the raster extent.",
      call. = FALSE
    )
  }

  # Keep the output grid and explicitly set feature cells to zero, matching
  # the intended "zerando o raster_distancia" behavior in the original code.
  valoresr <- raster::getValues(raster_osm)
  valoresr[!is.na(valoresr)] <- 0
  raster::setValues(raster_osm, valoresr)
}
