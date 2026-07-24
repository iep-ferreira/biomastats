#' Integrate OSM features and spatial metrics
#'
#' Loads each requested OpenStreetMap feature once and reuses its rasterized
#' representation across all requested metrics. This avoids repeated Overpass
#' queries when, for example, distance and density are calculated for the same
#' road or waterway feature.
#'
#' `features` is a named list. Each element must contain `key` and may contain
#' `value` and `provider`. `metrics` can be a character vector or a named list
#' of metric options. Available metrics are `"distance"` and `"density"`;
#' function-name aliases `"distance_to_feature"` and `"density_of_feature"`
#' are also accepted. Density options include `window_size` and
#' `window_shape`.
#'
#' @param reference_raster A `RasterLayer` defining the analysis grid, CRS,
#'   and valid-cell mask.
#' @param features Named list of feature specifications, for example
#'   `list(roads = list(key = "highway", value = "primary"))`.
#' @param metrics Character vector of metric names or a named list whose values
#'   are option lists. For example,
#'   `list(distance = list(), density = list(window_size = 9))`.
#' @param provider Default data provider for features that do not define their
#'   own provider. Only `"osm"` is currently supported.
#' @param buffer_distance Default buffer around each valid reference extent,
#'   in metres. Defaults to `2000` (2 km). A feature specification may define
#'   its own `buffer_distance`.
#' @param plot Logical; if `TRUE`, create and return a plot for every
#'   feature/metric combination. If `FALSE`, metric plots are `NULL` and the
#'   top-level `plots` element is also `NULL`.
#'
#' @return An object of class `biomastats_feature_integration` with three
#'   elements: `features`, the rasterized features loaded once; `results`, a
#'   nested list indexed by feature and metric; and `plots`, a named list of
#'   `ggplot2` objects when `plot = TRUE`.
#' @export
#'
#' @examples
#' \dontrun{
#' integrated <- integrate_feature(
#'   reference_raster,
#'   features = list(
#'     roads = list(key = "highway", value = "primary"),
#'     rivers = list(key = "waterway", value = "river")
#'   ),
#'   metrics = list(
#'     distance = list(),
#'     density = list(window_size = 9, window_shape = "circle")
#'   ),
#'   plot = TRUE
#' )
#'
#' integrated$results$roads$distance$raster
#' integrated$results$roads$density$global
#' integrated$plots$roads_distance
#' }
integrate_feature <- function(reference_raster, features,
                              metrics = c("distance", "density"),
                              provider = "osm", buffer_distance = 2000,
                              plot = TRUE) {
  if (!inherits(reference_raster, "RasterLayer")) {
    stop("'reference_raster' must be a raster::RasterLayer.", call. = FALSE)
  }
  if (!is.list(features) || length(features) == 0L) {
    stop("'features' must be a non-empty list.", call. = FALSE)
  }
  if (!is.logical(plot) || length(plot) != 1L || is.na(plot)) {
    stop("'plot' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.character(provider) || length(provider) != 1L ||
      is.na(provider) || !nzchar(provider)) {
    stop("'provider' must be one non-empty character string.", call. = FALSE)
  }
  validate_osm_buffer_distance(buffer_distance)

  feature_names <- names(features)
  if (is.null(feature_names)) {
    feature_names <- paste0("feature_", seq_along(features))
  } else {
    missing_names <- !nzchar(feature_names)
    feature_names[missing_names] <- paste0(
      "feature_", which(missing_names)
    )
  }
  if (anyDuplicated(feature_names)) {
    stop("'features' must have unique names.", call. = FALSE)
  }
  names(features) <- feature_names

  feature_specs <- lapply(feature_names, function(feature_name) {
    normalize_feature_spec(
      features[[feature_name]], feature_name, provider, buffer_distance
    )
  })
  names(feature_specs) <- feature_names
  metric_specs <- normalize_metric_specs(metrics)

  loaded_features <- vector("list", length(feature_specs))
  names(loaded_features) <- feature_names
  results <- vector("list", length(feature_specs))
  names(results) <- feature_names
  plots <- list()

  for (feature_name in feature_names) {
    specification <- feature_specs[[feature_name]]
    loaded_feature <- tryCatch(
      load_osm_data(
        reference_raster = reference_raster,
        key_feature = specification$key,
        value_feature = specification$value,
        provider = specification$provider,
        buffer_distance = specification$buffer_distance
      ),
      error = function(error) {
        stop(
          sprintf("Could not load feature '%s': %s", feature_name,
                  conditionMessage(error)),
          call. = FALSE
        )
      }
    )
    loaded_features[[feature_name]] <- loaded_feature
    loaded_values <- raster::getValues(loaded_feature)
    feature_results <- vector("list", length(metric_specs))
    names(feature_results) <- names(metric_specs)

    for (metric_name in names(metric_specs)) {
      # Some raster operations may update an in-memory RasterLayer by
      # reference. Give every metric an independent value copy while keeping
      # the single OSM request and rasterization performed above.
      metric_feature <- raster::setValues(
        raster::raster(loaded_feature),
        loaded_values
      )
      common_arguments <- list(
        reference_raster = reference_raster,
        key_feature = specification$key,
        value_feature = specification$value,
        provider = specification$provider,
        feature_raster = metric_feature,
        plot = plot
      )
      metric_result <- tryCatch(
        if (identical(metric_name, "distance")) {
          do.call(distance_to_feature, common_arguments)
        } else {
          do.call(
            density_of_feature,
            c(common_arguments, metric_specs[[metric_name]])
          )
        },
        error = function(error) {
          stop(
            sprintf("Metric '%s' failed for feature '%s': %s",
                    metric_name, feature_name, conditionMessage(error)),
            call. = FALSE
          )
        }
      )
      feature_results[[metric_name]] <- metric_result
      if (isTRUE(plot)) {
        plot_name <- paste(feature_name, metric_name, sep = "_")
        plots[[plot_name]] <- metric_result$plot
      }
    }
    results[[feature_name]] <- feature_results
  }

  structure(
    list(
      features = loaded_features,
      results = results,
      plots = if (isTRUE(plot)) plots else NULL
    ),
    class = "biomastats_feature_integration"
  )
}

normalize_feature_spec <- function(specification, feature_name,
                                   default_provider, default_buffer_distance) {
  if (is.character(specification) && length(specification) == 1L) {
    specification <- list(key = specification)
  }
  if (!is.list(specification)) {
    stop(sprintf("Feature '%s' must be a list or one OSM key.", feature_name),
         call. = FALSE)
  }

  key <- specification$key
  if (is.null(key)) key <- specification$key_feature
  value <- specification$value
  if (is.null(value)) value <- specification$value_feature
  feature_provider <- specification$provider
  if (is.null(feature_provider)) feature_provider <- default_provider
  feature_buffer_distance <- specification$buffer_distance
  if (is.null(feature_buffer_distance)) {
    feature_buffer_distance <- default_buffer_distance
  }

  if (!is.character(key) || length(key) != 1L || is.na(key) || !nzchar(key)) {
    stop(sprintf("Feature '%s' must define one non-empty 'key'.",
                 feature_name), call. = FALSE)
  }
  if (!is.null(value) &&
      (!is.character(value) || length(value) != 1L ||
       is.na(value) || !nzchar(value))) {
    stop(sprintf("Feature '%s' has an invalid 'value'.", feature_name),
         call. = FALSE)
  }
  if (!is.character(feature_provider) || length(feature_provider) != 1L ||
      is.na(feature_provider) || !nzchar(feature_provider)) {
    stop(sprintf("Feature '%s' has an invalid 'provider'.", feature_name),
         call. = FALSE)
  }
  validate_osm_buffer_distance(feature_buffer_distance, feature_name)

  list(
    key = key,
    value = value,
    provider = feature_provider,
    buffer_distance = feature_buffer_distance
  )
}

validate_osm_buffer_distance <- function(buffer_distance,
                                         feature_name = NULL) {
  if (!is.numeric(buffer_distance) || length(buffer_distance) != 1L ||
      is.na(buffer_distance) || !is.finite(buffer_distance) ||
      buffer_distance < 0) {
    label <- if (is.null(feature_name)) "'buffer_distance'" else
      sprintf("buffer_distance for feature '%s'", feature_name)
    stop(
      sprintf("%s must be one finite non-negative value in metres.", label),
      call. = FALSE
    )
  }
  invisible(buffer_distance)
}

normalize_metric_specs <- function(metrics) {
  normalize_name <- function(metric) {
    aliases <- c(
      distance = "distance",
      distance_to_feature = "distance",
      density = "density",
      density_of_feature = "density"
    )
    normalized <- unname(aliases[metric])
    if (length(normalized) != 1L || is.na(normalized)) {
      stop(
        sprintf(
          "Unsupported metric '%s'. Available metrics are 'distance' and 'density'.",
          metric
        ),
        call. = FALSE
      )
    }
    normalized
  }

  if (is.character(metrics)) {
    if (!length(metrics) || anyNA(metrics) || any(!nzchar(metrics))) {
      stop("'metrics' must contain at least one valid metric name.",
           call. = FALSE)
    }
    metric_names <- vapply(metrics, normalize_name, character(1))
    metric_names <- unique(metric_names)
    output <- lapply(metric_names, function(metric) list())
    names(output) <- metric_names
    return(output)
  }

  if (!is.list(metrics) || !length(metrics) || is.null(names(metrics)) ||
      any(!nzchar(names(metrics)))) {
    stop("'metrics' must be a character vector or a named list.",
         call. = FALSE)
  }
  metric_names <- vapply(names(metrics), normalize_name, character(1))
  if (anyDuplicated(metric_names)) {
    stop("'metrics' contains duplicated metric definitions.", call. = FALSE)
  }

  output <- vector("list", length(metrics))
  names(output) <- metric_names
  for (index in seq_along(metrics)) {
    metric_name <- unname(metric_names[index])
    options <- metrics[[index]]
    if (is.null(options)) options <- list()
    if (!is.list(options)) {
      stop(sprintf("Options for metric '%s' must be a list.",
                   names(metrics)[index]), call. = FALSE)
    }
    if (identical(metric_name, "distance") && length(options)) {
      stop("The 'distance' metric does not currently accept options.",
           call. = FALSE)
    }
    if (identical(metric_name, "density")) {
      unsupported <- setdiff(names(options), c("window_size", "window_shape"))
      if (length(unsupported)) {
        stop(sprintf("Unsupported density option: %s.", unsupported[1]),
             call. = FALSE)
      }
    }
    output[[index]] <- options
  }
  output
}
