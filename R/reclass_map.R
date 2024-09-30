#' Reclassify and Visualize Land Use Map
#'
#' This function reclassifies a raster map based on a provided object and year, 
#' and returns a ggplot visualization of the raster data with annotations for scale 
#' and north arrow. The raster data is expected to represent land use over time.
#'
#' @param obj A list object containing the raster data (`rec_map`), the start year (`start`), 
#'   end year (`end`), and land use classes (`classes`).
#' @param year An integer representing the specific year for which the land use map should 
#'   be reclassified and visualized. Must be within the range of `start` and `end` in `obj`.
#' @return A ggplot object visualizing the reclassified land use map for the given year.
#' @details The function generates a reclassification of the raster data based on the year
#'   and provided classes. It uses the `ggspatial` package to add annotations for scale and
#'   north arrow to the plot. The output is a ggplot object which can be further customized.
#'   A warning is printed if `obj` is `NULL` or if `year` is out of the valid range.
#' @seealso \code{\link[ggplot2]{ggplot}}, \code{\link[ggspatial]{annotation_scale}},
#'   \code{\link[ggspatial]{annotation_north_arrow}}
#' @import ggplot2
#' @import ggspatial
#' @importFrom grid unit
#' @importFrom sf st_crs
#' @export
#' @examples
#' # Example usage:
#' # Assuming `land_use_obj` is a pre-loaded object containing raster data.
#' # reclass_map(obj = land_use_obj, year = 2015)

reclass_map <- function(obj = NULL, year = NULL){
 
  raster_data <- obj$rec_map
  start <- obj$start
  end <- obj$end
  time_range <- end - start
  classes <- obj$classes
  
  dicti <- biomastats:::dict_build()
  name_classes <- paste(dicti$class[dicti$code %in% classes], collapse = "\n ")
 
  if (is.null(obj)) {
    print("Error. obj is NULL!")
    return(NULL)
  }
  if (year < start || year > end) {
    print("Year out of bounds!")
    return(NULL)
  }
  raster <- raster_data[[year - (start - 1)]]
  crs_input <- sf::st_crs(raster)$input
  #raster <- raster::projectRaster(raster, crs = "+proj=longlat +datum=WGS84")
  #dd <- data.frame(table(raster::getValues(raster)))
  #colnames(dd) <- c("class", "area")
  #names_classes <- NULL
  #cores_classes <- NULL
  #for (j in 1:length(dd$class)) {
  #  pos <- dd$class[j] == dic$code
  #  names_classes[j] <- dic$class[pos]
  #  cores_classes[j] <- dic$color[pos]
  #}

    raster_aux <- as(raster, "SpatialPixelsDataFrame")
  dat <- as.data.frame(raster_aux)
  names(dat) <- c("value", "x", "y")
  dat$value <- dat$value+1
  p <- ggplot2::ggplot(data = dat, ggplot2::aes(x = x, y = y, fill = factor(value) )) + 
    ggplot2::geom_tile() + ggplot2::coord_sf(crs = crs_input) + 
    ggspatial::annotation_scale(width_hint = 0.2, style = "bar", 
                                location = "br", unit_category = "metric") + 
    ggspatial::annotation_north_arrow(location = "tr", 
    which_north = "true", pad_x = grid::unit(0.1, "in"), 
    pad_y = grid::unit(0.2, "in"), style = ggspatial::north_arrow_fancy_orienteering) + 
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + 
    ggplot2::ggtitle(paste("Year: ", as.character(year))) + 
    #scale_fill_viridis_d()
    ggplot2::scale_fill_manual(values = c("white", "black"), name = "Land Uses", 
                               labels = c("", name_classes), 
                               guide = ggplot2::guide_legend(
                                 reverse = TRUE,
                                 override.aes = list(fill = "white", color = NA), 
                                 keywidth = 0.1, keyheight = 0.1
                               )
                               )
  return(p)
}
