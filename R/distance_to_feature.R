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
#' remain `NA` in the result. The returned `plot` is a regular `ggplot2`
#' object with English labels, so it can be customized with `ggplot2` layers
#' and exported with [ggplot2::ggsave()].
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
#' @return A list with two elements: `raster`, a `RasterLayer` of distances to
#'   the nearest matching OSM feature, and `plot`, an adaptable `ggplot` map.
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
#' road_distance$plot
#' ggplot2::ggsave("road-distance.png", road_distance$plot,
#'                 width = 8, height = 6, dpi = 300)
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

  distance_plot <- plot_feature_distance(
    raster_distance = raster_distance_result,
    key_feature = key_feature,
    value_feature = value_feature
  )

  list(
    raster = raster_distance_result,
    plot = distance_plot
  )
}

# Build an exportable map while keeping plotting concerns separate from the
# distance calculation performed by distance_to_feature().
plot_feature_distance <- function(raster_distance, key_feature,
                                  value_feature = NULL) {
  plot_data <- as.data.frame(raster::rasterToPoints(raster_distance))
  names(plot_data) <- c("x", "y", "distance")

  longitude_latitude <- raster::isLonLat(raster_distance)
  x_label <- if (longitude_latitude) "Longitude" else "Easting"
  y_label <- if (longitude_latitude) "Latitude" else "Northing"
  legend_label <- if (longitude_latitude) {
    "Distance (m)"
  } else {
    "Distance (map units)"
  }
  feature_label <- if (is.null(value_feature)) {
    key_feature
  } else {
    paste0(key_feature, " = ", value_feature)
  }

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(x = x, y = y, fill = distance)
  ) +
    ggplot2::geom_raster() +
    ggplot2::coord_sf(
      crs = sf::st_crs(raster::projection(raster_distance)),
      expand = FALSE
    ) +
    ggspatial::annotation_scale(
      width_hint = 0.2,
      style = "bar",
      location = "br",
      unit_category = "metric"
    ) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      which_north = "true",
      pad_x = grid::unit(0.1, "in"),
      pad_y = grid::unit(0.2, "in"),
      style = ggspatial::north_arrow_fancy_orienteering
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "C",
      direction = -1,
      name = legend_label,
      na.value = "transparent"
    ) +
    ggplot2::labs(
      title = paste0("Distance to OSM feature: ", feature_label),
      x = x_label,
      y = y_label,
      caption = "Source: OpenStreetMap"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "right"
    )
}
