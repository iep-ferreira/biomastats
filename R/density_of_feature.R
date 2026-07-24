#' Local density of an OpenStreetMap feature
#'
#' Downloads an OpenStreetMap feature, rasterizes it on the reference grid,
#' and calculates the proportion of occupied cells within a moving window.
#' The same window is used at every valid cell, while the denominator is
#' adjusted near the border and around `NA` cells to avoid treating nodata as
#' part of the landscape.
#'
#' This is a cell-coverage density. It is especially useful for comparing the
#' local concentration of roads, waterways, built-up areas, or other mapped
#' features at the resolution of the reference raster. For linear features,
#' it should not be interpreted as physical line length per area unless the
#' raster resolution and the rasterization method are explicitly accounted
#' for.
#'
#' @param reference_raster A `RasterLayer` defining the analysis extent,
#'   resolution, CRS, and valid-cell mask.
#' @param key_feature A non-empty OSM key such as `"highway"`, `"waterway"`,
#'   `"natural"`, or `"landuse"`.
#' @param value_feature An optional OSM value. Use `NULL` to match every value
#'   for the key; a specific value is recommended for large study areas.
#' @param provider Data provider. Only `"osm"` is currently supported.
#' @param window_size Odd positive integer defining the side of the moving
#'   window in raster cells. For example, `5` means a 5 by 5 window.
#' @param window_shape Shape of the moving window: `"square"` or `"circle"`.
#'
#' @return A list with `raster`, a `RasterLayer` of local feature density,
#'   `plot`, an adaptable `ggplot2` map with English labels, and `global`, the
#'   feature coverage proportion across all valid cells in the reference
#'   raster.
#' @export
#' @importFrom raster focal getValues projection raster setValues
#'
#' @examples
#' \dontrun{
#' roads_density <- density_of_feature(
#'   reference_raster,
#'   key_feature = "highway",
#'   value_feature = "primary",
#'   window_size = 9,
#'   window_shape = "circle"
#' )
#' roads_density$global
#' roads_density$plot
#' ggplot2::ggsave("road-density.png", roads_density$plot,
#'                 width = 8, height = 6, dpi = 300)
#' }
density_of_feature <- function(reference_raster, key_feature,
                                value_feature = NULL, provider = "osm",
                                window_size = 5L,
                                window_shape = c("square", "circle")) {
  if (!inherits(reference_raster, "RasterLayer")) {
    stop("'reference_raster' must be a raster::RasterLayer.", call. = FALSE)
  }

  raster_crs <- raster::projection(reference_raster)
  if (is.na(raster_crs) || !nzchar(raster_crs)) {
    stop("'reference_raster' must have a CRS so density units are defined.",
         call. = FALSE)
  }

  reference_values <- raster::getValues(reference_raster)
  valid_reference <- !is.na(reference_values)
  if (!any(valid_reference)) {
    stop("'reference_raster' must contain at least one valid cell.",
         call. = FALSE)
  }

  if (!is.numeric(window_size) || length(window_size) != 1L ||
      is.na(window_size) || window_size < 1 ||
      window_size != as.integer(window_size) ||
      window_size %% 2L == 0L) {
    stop("'window_size' must be one odd positive integer.", call. = FALSE)
  }
  window_size <- as.integer(window_size)
  window_shape <- match.arg(window_shape)

  feature_raster <- load_osm_data(
    reference_raster = reference_raster,
    key_feature = key_feature,
    value_feature = value_feature,
    provider = provider
  )
  if (!inherits(feature_raster, "RasterLayer")) {
    stop("The rasterized OSM feature must be a single RasterLayer.",
         call. = FALSE)
  }

  feature_values <- raster::getValues(feature_raster)
  occupied <- !is.na(feature_values) & valid_reference
  if (!any(occupied)) {
    stop("The rasterized OSM feature contains no occupied cells.",
         call. = FALSE)
  }

  # Use zeros for valid non-feature cells and NA for the study-area mask.
  feature_binary <- raster::setValues(
    reference_raster,
    ifelse(valid_reference, as.numeric(occupied), NA_real_)
  )
  valid_binary <- raster::setValues(
    reference_raster,
    ifelse(valid_reference, 1, NA_real_)
  )

  window <- matrix(1, nrow = window_size, ncol = window_size)
  if (identical(window_shape, "circle")) {
    centre <- (window_size + 1) / 2
    coordinates <- expand.grid(
      row = seq_len(window_size),
      column = seq_len(window_size)
    )
    window[as.matrix(coordinates)] <- 0
    inside <- (coordinates$row - centre)^2 +
      (coordinates$column - centre)^2 <= (window_size / 2)^2
    window[as.matrix(coordinates)[inside, , drop = FALSE]] <- 1
  }

  local_feature <- raster::focal(
    feature_binary, w = window, fun = sum, na.rm = TRUE,
    pad = TRUE, padValue = NA_real_
  )
  local_valid <- raster::focal(
    valid_binary, w = window, fun = sum, na.rm = TRUE,
    pad = TRUE, padValue = NA_real_
  )
  density_values <- raster::getValues(local_feature) /
    raster::getValues(local_valid)
  density_values[!valid_reference] <- NA_real_
  density_raster <- raster::setValues(reference_raster, density_values)

  global_density <- sum(occupied) / sum(valid_reference)
  density_plot <- plot_feature_density(
    density_raster = density_raster,
    key_feature = key_feature,
    value_feature = value_feature,
    window_size = window_size,
    window_shape = window_shape
  )

  list(
    raster = density_raster,
    plot = density_plot,
    global = global_density
  )
}

# Construct an exportable ggplot2 map for the local density raster.
plot_feature_density <- function(density_raster, key_feature,
                                 value_feature = NULL, window_size,
                                 window_shape) {
  plot_data <- as.data.frame(raster::rasterToPoints(density_raster))
  names(plot_data) <- c("x", "y", "density")
  longitude_latitude <- raster::isLonLat(density_raster)
  feature_label <- if (is.null(value_feature)) {
    key_feature
  } else {
    paste0(key_feature, " = ", value_feature)
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y, fill = density)
  ) +
    ggplot2::geom_raster() +
    ggplot2::coord_sf(
      crs = sf::st_crs(raster::projection(density_raster)),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(
      width_hint = 0.2, style = "bar", location = "br",
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr", which_north = "true",
      pad_x = grid::unit(0.1, "in"), pad_y = grid::unit(0.2, "in"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C", limits = c(0, 1), name = "Local density",
      na.value = "transparent"
    ) +
    ggplot2::labs(
      title = paste0("Local density of OSM feature: ", feature_label),
      subtitle = paste0(window_size, " x ", window_size, " ", window_shape,
                        " moving window"),
      x = if (longitude_latitude) "Longitude" else "Easting",
      y = if (longitude_latitude) "Latitude" else "Northing",
      caption = "Source: OpenStreetMap"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
}
