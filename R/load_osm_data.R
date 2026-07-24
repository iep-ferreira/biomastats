#' Load and rasterize an OpenStreetMap feature
#'
#' Queries OpenStreetMap for a feature inside the extent of a reference
#' raster, then rasterizes the returned geometries using the same extent and
#' resolution. The query is reduced to the smallest bounding box containing
#' valid reference cells, then expanded by a buffer equal to the diagonal
#' diameter of that box. The buffer brings features crossing the study-area
#' border into the query and reduces edge effects in the distance raster. The
#' OSM query is made with `osmdata::opq()` and therefore may require network
#' access and an available Overpass provider. If the installed
#' `osmdata` version cannot assemble the returned feature metadata, the same
#' query is read from OSM XML through the GDAL driver used by `sf`.
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

  reference_values <- raster::getValues(reference_raster)
  valid_cells <- which(!is.na(reference_values))
  if (!length(valid_cells)) {
    stop("'reference_raster' must contain at least one valid cell.",
         call. = FALSE)
  }

  # Query only the smallest raster-aligned extent containing valid cells.
  # This avoids sending nodata margins to Overpass for clipped rasters.
  number_columns <- raster::ncol(reference_raster)
  valid_rows <- ((valid_cells - 1L) %/% number_columns) + 1L
  valid_columns <- ((valid_cells - 1L) %% number_columns) + 1L
  raster_resolution <- raster::res(reference_raster)
  valid_extent <- raster::extent(
    raster::xFromCol(reference_raster, min(valid_columns)) - raster_resolution[1] / 2,
    raster::xFromCol(reference_raster, max(valid_columns)) + raster_resolution[1] / 2,
    raster::yFromRow(reference_raster, max(valid_rows)) - raster_resolution[2] / 2,
    raster::yFromRow(reference_raster, min(valid_rows)) + raster_resolution[2] / 2
  )

  # Add a buffer equal to the diagonal diameter of the valid raster extent.
  # Buffering in a projected CRS keeps the distance meaningful for both
  # projected and longitude/latitude reference rasters.
  bbox_geometry <- sf::st_as_sfc(
    sf::st_bbox(valid_extent)
  )
  bbox_geometry <- sf::st_set_crs(bbox_geometry, raster_crs)
  buffer_crs <- if (raster::isLonLat(reference_raster)) 3857 else raster_crs
  bbox_projected <- sf::st_transform(bbox_geometry, buffer_crs)
  bbox_coordinates <- sf::st_bbox(bbox_projected)
  bbox_width <- bbox_coordinates[["xmax"]] - bbox_coordinates[["xmin"]]
  bbox_height <- bbox_coordinates[["ymax"]] - bbox_coordinates[["ymin"]]
  bbox_diameter <- sqrt(bbox_width^2 + bbox_height^2)
  bbox_buffered <- sf::st_buffer(bbox_projected, dist = bbox_diameter)

  # Overpass expects longitude/latitude coordinates.
  bbox <- sf::st_bbox(sf::st_transform(bbox_buffered, 4326))
  query <- opq(
    bbox = bbox,
    timeout = 60,
    out = "body"
  )
  if (is.null(value_feature)) {
    query <- add_osm_feature(query, key = key_feature)
  } else {
    query <- add_osm_feature(
      query,
      key = key_feature,
      value = value_feature
    )
  }
  osm_data <- tryCatch(
    osmdata_sf(query),
    error = function(error) {
      metadata_error <- grepl(
        "arguments imply differing number of rows",
        conditionMessage(error),
        fixed = TRUE
      )
      if (!metadata_error) {
        stop(error)
      }

      # osmdata 0.4.0 can receive valid geometry while producing an empty
      # metadata table. Reading the same OSM XML with GDAL preserves the
      # geometry and avoids discarding a successful Overpass response.
      read_osm_xml_as_sf(query)
    }
  )

  # Set all occupied cells in the reference to zero before rasterization.
  valores <- reference_values
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

# Read the standard GDAL OSM layers into the list consumed by rasterize_osm().
read_osm_xml_as_sf <- function(query) {
  osm_file <- tempfile(fileext = ".osm")
  on.exit(unlink(osm_file), add = TRUE)

  osmdata::osmdata_xml(query, filename = osm_file)
  available_layers <- sf::st_layers(osm_file)[["name"]]

  read_layer <- function(layer) {
    if (!layer %in% available_layers) {
      return(NULL)
    }
    features <- sf::st_read(osm_file, layer = layer, quiet = TRUE)
    if (nrow(features) == 0L) NULL else features
  }

  list(
    osm_points = read_layer("points"),
    osm_lines = read_layer("lines"),
    osm_multilines = read_layer("multilinestrings"),
    osm_polygons = NULL,
    osm_multipolygons = read_layer("multipolygons")
  )
}
