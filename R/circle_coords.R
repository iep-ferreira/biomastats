#' Circle Coordinates
#'
#' This function generates the coordinates for a circle given a center (latitude and longitude) and an adjustment size.
#' The circle is defined using the parametric representation, x = rcos(theta) and y = rsin(theta).
#'
#' @param lat Latitude of the center of the circle. Default is 'lat'.
#' @param lon Longitude of the center of the circle. Default is 'lon'.
#' @param adj_size Adjustment size for the circle. This will affect the radius of the circle. Default is 'adj_size'.
#'
#' @return A Polygon object representing the circle.
#' @export
#' @examples
#' \dontrun{
#' circle_coords(lat = 40.7128, lon = -74.0060, adj_size = 1)
#' }
#' @importFrom sf st_polygon st_sfc
circle_coords <- function(lat = lat, lon = lon, adj_size = adj_size){

  x <- y <- seq(0, 2*pi, length.out = 100)
  circle.coords <- cbind(adj_size * cos(x) + lon, adj_size * sin(x) + lat)

  # Create an sf object
  polygon <- sf::st_polygon(list(circle.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)
}




