#' Land Class Visualization
#'
#' This function generates a map visualization for land classes in a specified year.
#'
#' @param data A list containing at least a raster list element with the raster images. If NULL, the function will stop and print an error message.
#' @param year A numeric value indicating the year to be visualized. Should be between 1985 and 2020. If out of these bounds, the function will stop and print an error message. Default is 1985.
#'
#' @return A ggplot object displaying the land classes for the specified year.
#'
#' @examples
#' \dontrun{
#' land_vis(data = my_data, year = 1990)
#' }
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_tile coord_sf xlab ylab ggtitle scale_fill_manual
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orienteering
#' @importFrom raster getValues
#' @importFrom grid unit
# This function generates a map visualization for land classes in a specified year.
land_vis <- function(data = NULL, year = 1985){

  #Dictionary
  dic <- dict_build()

  # Check if data is NULL
  if(is.null(data)) {
    print("Error. Data is NULL!")
    return(NULL)
  }

  # Check if year is within allowed range
  if(year < 1985 || year > 2020){
    print("Year out of bounds!")
    return(NULL)
  }

  # Retrieve raster data for the specified year
  raster <- data$raster[[year - 1984]]
  dd <- data.frame(table(raster::getValues(raster)))
  colnames(dd) <- c("class", "area")

  names_classes <- NULL
  cores_classes <- NULL

  # Assign names and colors for each class
  for(j in 1:length(dd$class)){
    pos <- dd$class[j] == dic$code
    names_classes[j] <- dic$class[pos]
    cores_classes[j] <- dic$color[pos]
  }

  # Convert the raster object to a data frame with columns "value", "x", and "y".
  raster <- as(raster, "SpatialPixelsDataFrame")
  dat <- as.data.frame(raster)
  names(dat) <- c("value", "x", "y")

  # Create a plot of the data frame, mapping "x" and "y" to the x and y axis and "value" to fill color
  # Add a north arrow, x and y axis labels, plot title, and a custom color scale with manual values and labels
  p <-  ggplot2::ggplot(data=dat, ggplot2::aes(x=x, y=y, fill=factor(value))) +
    ggplot2::geom_tile() + ggplot2::coord_sf(crs = "+proj=longlat +datum=WGS84") +
    ggspatial::annotation_scale(width_hint = 0.2,
                     style = "bar",
                     location = "br",
                     unit_category = "metric") +
    ggspatial::annotation_north_arrow(location = "tr", which_north = "true",
                           pad_x = grid::unit(0.1, "in"), pad_y = grid::unit(0.2, "in"),
                           style = ggspatial::north_arrow_fancy_orienteering) +
    ggplot2::xlab("Longitude") + ggplot2::ylab("Latitude") + ggplot2::ggtitle(paste("Year: ", as.character(year))) +
    ggplot2::scale_fill_manual(
      values = cores_classes,
      name = "Land Uses",
      labels = names_classes,
      guide = ggplot2::guide_legend(reverse = TRUE))

  return(p)
}
