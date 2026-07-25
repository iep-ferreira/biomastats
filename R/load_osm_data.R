#' Load and rasterize an OpenStreetMap feature
#'
#' Queries OpenStreetMap for a feature inside the extent of a reference
#' raster, then rasterizes the returned geometries using the same extent and
#' resolution. The query is reduced to the smallest bounding box containing
#' valid reference cells, then expanded by buffer_distance metres. The default
#' buffer is 2 km, which brings nearby features crossing the study-area border
#' into the query without querying an unnecessarily large area. The
#' OSM query is made with `osmdata::opq()` and therefore may require network
#' access and an available Overpass provider. Successful responses are cached
#' for seven days by default. If the preferred Overpass endpoint fails, the
#' function tries alternative global endpoints and can use an expired cache
#' entry as a final availability fallback. If the installed
#' `osmdata` version cannot assemble the returned feature metadata, the same
#' query is read from OSM XML through the GDAL driver used by `sf`.
#'
#' The current implementation supports OpenStreetMap through `osmdata` only.
#' For repeated large-area or batch processing, a regional PBF extract remains
#' more appropriate than public Overpass instances.
#'
#' @param reference_raster A `RasterLayer` defining the query bounding box
#'   and the output raster geometry.
#' @param key_feature A character string with the OSM feature key, such as
#'   `"highway"` or `"natural"`.
#' @param value_feature Optional OSM feature value, such as `"primary"` or
#'   `"water"`. If `NULL`, all values for `key_feature` are queried.
#' @param provider Data provider. Only `"osm"` is currently supported.
#' @param buffer_distance Buffer around the valid reference extent, in metres.
#'   Defaults to `2000` (2 km). Use `0` to disable the buffer.
#' @param timeout Positive number of seconds allowed by each Overpass query.
#'   Defaults to `60`.
#' @param overpass_urls Optional character vector of Overpass interpreter URLs,
#'   tried in order. When `NULL`, the endpoint configured in `osmdata` is tried
#'   first, followed by global fallback instances.
#' @param use_cache Logical. If `TRUE`, reuse a successful query with the same
#'   rounded bounding box, key, and value. An expired entry is only used when
#'   every Overpass endpoint fails.
#' @param cache_ttl_days Non-negative cache validity period in days. Defaults
#'   to `7`. The cache directory can be customized with the
#'   `biomastats.osm_cache_dir` option.
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
                          provider = "osm", buffer_distance = 2000,
                          timeout = 60, overpass_urls = NULL,
                          use_cache = getOption("biomastats.osm_use_cache", TRUE),
                          cache_ttl_days = 7) {
  validate_osmdata_version()
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
  if (!is.numeric(buffer_distance) || length(buffer_distance) != 1L ||
      is.na(buffer_distance) || !is.finite(buffer_distance) ||
      buffer_distance < 0) {
    stop("'buffer_distance' must be one finite non-negative value in metres.",
         call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0) {
    stop("'timeout' must be one finite positive number of seconds.",
         call. = FALSE)
  }
  if (!is.null(overpass_urls) &&
      (!is.character(overpass_urls) || !length(overpass_urls) ||
       anyNA(overpass_urls) || any(!nzchar(overpass_urls)) ||
       any(!grepl("^https?://", overpass_urls)))) {
    stop("'overpass_urls' must be NULL or HTTP(S) endpoint URLs.",
         call. = FALSE)
  }
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    stop("'use_cache' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(cache_ttl_days) || length(cache_ttl_days) != 1L ||
      is.na(cache_ttl_days) || !is.finite(cache_ttl_days) ||
      cache_ttl_days < 0) {
    stop("'cache_ttl_days' must be one finite non-negative number.",
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

  # Buffer in metres after transforming to Web Mercator. This keeps the
  # public buffer_distance unit independent of the input CRS.
  bbox_geometry <- sf::st_as_sfc(
    sf::st_bbox(valid_extent)
  )
  bbox_geometry <- sf::st_set_crs(bbox_geometry, raster_crs)
  buffer_crs <- 3857
  bbox_projected <- sf::st_transform(bbox_geometry, buffer_crs)
  bbox_buffered <- sf::st_buffer(bbox_projected, dist = buffer_distance)

  # Overpass expects longitude/latitude coordinates.
  bbox <- sf::st_bbox(sf::st_transform(bbox_buffered, 4326))
  cache_file <- if (isTRUE(use_cache)) {
    osm_cache_file(bbox, key_feature, value_feature)
  } else {
    NULL
  }
  cached <- read_osm_cache(cache_file, cache_ttl_days)

  if (!is.null(cached) && isTRUE(cached$fresh)) {
    osm_data <- cached$data
  } else {
    endpoints <- resolve_overpass_urls(overpass_urls)
    previous_endpoint <- osmdata::get_overpass_url()
    on.exit(
      try(osmdata::set_overpass_url(previous_endpoint), silent = TRUE),
      add = TRUE
    )

    osm_data <- NULL
    endpoint_errors <- character(0)
    successful_endpoint <- NULL
    for (endpoint in endpoints) {
      attempt <- tryCatch(
        fetch_osm_feature(
          bbox = bbox,
          key_feature = key_feature,
          value_feature = value_feature,
          timeout = timeout,
          endpoint = endpoint
        ),
        error = identity
      )
      if (!inherits(attempt, "error")) {
        osm_data <- attempt
        successful_endpoint <- endpoint
        break
      }
      endpoint_errors <- c(
        endpoint_errors,
        sprintf("%s: %s", endpoint, conditionMessage(attempt))
      )
    }

    if (is.null(osm_data)) {
      if (!is.null(cached)) {
        warning(
          "All Overpass endpoints failed; using an expired OSM cache entry.",
          call. = FALSE
        )
        osm_data <- cached$data
      } else {
        stop(
          paste0(
            "All Overpass endpoints failed. ",
            paste(endpoint_errors, collapse = " | ")
          ),
          call. = FALSE
        )
      }
    } else if (!is.null(cache_file)) {
      write_osm_cache(cache_file, osm_data, successful_endpoint)
    }
  }

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

validate_osmdata_version <- function() {
  validated_version <- package_version("0.4.0")
  installed_version <- utils::packageVersion("osmdata")
  if (installed_version < validated_version) {
    warning(
      sprintf(
        paste0(
          "biomastats requires osmdata >= %s, but %s is installed. ",
          "Update it with install.packages('osmdata')."
        ),
        validated_version,
        installed_version
      ),
      call. = FALSE
    )
  }
  invisible(installed_version)
}

# Query one endpoint. Keeping this operation separate makes failover explicit
# and allows deterministic tests without contacting public Overpass servers.
fetch_osm_feature <- function(bbox, key_feature, value_feature, timeout,
                              endpoint) {
  osmdata::set_overpass_url(endpoint)
  query <- opq(bbox = bbox, timeout = timeout, out = "body")
  if (is.null(value_feature)) {
    query <- add_osm_feature(query, key = key_feature)
  } else {
    query <- add_osm_feature(query, key = key_feature, value = value_feature)
  }

  tryCatch(
    osmdata_sf(query),
    error = function(error) {
      metadata_error <- grepl(
        "arguments imply differing number of rows",
        conditionMessage(error),
        fixed = TRUE
      )
      if (!metadata_error) stop(error)

      # osmdata 0.4.0 can receive valid geometry while producing an empty
      # metadata table. GDAL reuses the same query and preserves its geometry.
      read_osm_xml_as_sf(query)
    }
  )
}

resolve_overpass_urls <- function(overpass_urls = NULL) {
  current <- osmdata::get_overpass_url()
  if (!is.null(overpass_urls)) return(unique(overpass_urls))

  unique(c(
    current,
    "https://overpass.kumi.systems/api/interpreter",
    "https://api.openstreetmap.fr/oapi/interpreter"
  ))
}

osm_cache_file <- function(bbox, key_feature, value_feature) {
  cache_dir <- getOption(
    "biomastats.osm_cache_dir",
    file.path(tools::R_user_dir("biomastats", which = "cache"), "osm")
  )
  cache_ready <- dir.exists(cache_dir) ||
    isTRUE(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE))
  if (!cache_ready) return(NULL)

  cache_key <- list(
    format = 1L,
    bbox = round(unname(as.numeric(
      bbox[c("xmin", "ymin", "xmax", "ymax")]
    )), 5),
    key = key_feature,
    value = value_feature
  )
  key_file <- tempfile("biomastats-osm-key-", fileext = ".bin")
  on.exit(unlink(key_file), add = TRUE)
  writeBin(serialize(cache_key, NULL, version = 2), key_file)
  hash <- unname(tools::md5sum(key_file))
  file.path(cache_dir, paste0("osm_", hash, ".rds"))
}

read_osm_cache <- function(cache_file, cache_ttl_days) {
  if (is.null(cache_file) || !file.exists(cache_file)) return(NULL)
  cached <- tryCatch(readRDS(cache_file), error = function(error) NULL)
  if (!is.list(cached) || is.null(cached$data) ||
      is.null(cached$fetched_at)) return(NULL)

  age_days <- as.numeric(
    difftime(Sys.time(), cached$fetched_at, units = "days")
  )
  cached$fresh <- is.finite(age_days) && age_days <= cache_ttl_days
  cached
}

write_osm_cache <- function(cache_file, osm_data, endpoint) {
  cache_dir <- dirname(cache_file)
  temporary_file <- tempfile("osm-cache-", tmpdir = cache_dir, fileext = ".rds")
  on.exit(unlink(temporary_file), add = TRUE)
  record <- list(
    fetched_at = Sys.time(),
    endpoint = endpoint,
    data = osm_data
  )
  written <- tryCatch({
    saveRDS(record, temporary_file, compress = TRUE)
    isTRUE(file.rename(temporary_file, cache_file))
  }, error = function(error) FALSE)
  invisible(written)
}

normalize_osm_options <- function(osm_options) {
  if (!is.list(osm_options)) {
    stop("'osm_options' must be a list.", call. = FALSE)
  }
  if (!length(osm_options)) return(osm_options)
  option_names <- names(osm_options)
  if (is.null(option_names) || any(!nzchar(option_names)) ||
      anyDuplicated(option_names)) {
    stop("'osm_options' must have unique, non-empty names.", call. = FALSE)
  }
  allowed <- c("timeout", "overpass_urls", "use_cache", "cache_ttl_days")
  unsupported <- setdiff(option_names, allowed)
  if (length(unsupported)) {
    stop(
      sprintf(
        "Unsupported OSM option(s): %s.",
        paste(unsupported, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  osm_options
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
