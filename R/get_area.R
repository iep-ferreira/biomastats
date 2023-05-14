#' Calculate Land Classes Area
#'
#' This function calculates the area of land classes over a specified time range.
#'
#' @param data A list containing two elements: 'time_range': a vector of two elements specifying the start and end years, and 'raster': a list of raster objects for each year in the time range.
#' @return A list with four elements: 'Years':the vector of years, 'Occupied area': a list with the calculated areas for each land class and year, 'aggregate_data': a data frame with the calculated areas for each land class and year, 'time_series': a ggplot2 object with the time series of areas.
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @importFrom graphics par
#' @importFrom raster area getValues
#' @export
#'
#' @examples
#' \dontrun{
#' data <- ... to specify ....
#' result <- get_area(data)
#' }
get_area <- function(data = NULL) {

  # Dictionary
  dic <- dict_build()

  # Time span of study
  time_span <- data$time_range[2] - data$time_range[1]

  areas <- NULL
  for(yr in (data$time_range[1] - 1984):(time_span + 1)) {
    areas_y <- raster::area(data$raster[[yr]])
    v_r <- raster::getValues(data$raster[[yr]])
    areas[[yr]] <- tapply(areas_y, v_r, sum)
  }

  df <- NULL
  for(yr in (data$time_range[1] - 1984):(time_span + 1)) {
    aux <-  data.frame(rep(yr+1984, dim(areas[[yr]])),  round(areas[[yr]],3))
    df <- rbind(df, cbind(names(areas[[yr]]), aux))
  }
  colnames(df) <- c("land_class", "year", "area")

  land_class_name <- NULL
  land_class_color <- NULL
  for(j in 1:length(df$land_class)) {
    pos <- df$land_class[j] == dic$code
    land_class_name[j] <- dic$class[pos]
    land_class_color[j] <- dic$color[pos]
  }
  df$land_class_name <- land_class_name

  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = area, group = land_class_name, color = land_class_name)) +
    ggplot2::scale_color_manual(values = land_class_color, limits = land_class_name, name = "Land Use") +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::ylab(expression(paste("Area in ", km^2))) +
    ggplot2::xlab("Years")

  return(
    list(
      "Years" = data$time_range[1]:(data$time_range[1] + time_span),
      "Occupied area" = areas,
      "aggregate_data" = df,
      "time_series" = p
    )
  )
}
