#' Circle Coordinates
#'
#' This function generates the coordinates for a circle given a center (latitude and longitude) and a size.
#' The 100 vertices are placed at the radius, on the WGS84 ellipsoid, every 3.6 degrees of bearing
#' (\code{geosphere::destPoint}), so the circle has the same size and stays round on the ground at any latitude.
#'
#' @param lat Latitude of the center of the circle. Default is 'lat'.
#' @param lon Longitude of the center of the circle. Default is 'lon'.
#' @param size Diameter of the circle, in kilometers. Default is 'size'.
#'
#' @return A Polygon object representing the circle.
#' @export

#' @examples
#' \dontrun{
#' circle_coords(lat = 40.7128, lon = -74.0060, size = 2)
#' }
#' @importFrom sf st_polygon st_sfc
#' @importFrom geosphere destPoint
circle_coords <- function(lat = lat, lon = lon, size = size){
  radius <- size * 1000 / 2

  # Bearings run counterclockwise from east
  bearings <- 90 - seq(0, 360, length.out = 101)[-101]
  circle.coords <- geosphere::destPoint(c(lon, lat), b = bearings, d = radius)
  circle.coords <- unname(rbind(circle.coords, circle.coords[1, ]))  # Close the polygon

  # Create an sf object
  polygon <- sf::st_polygon(list(circle.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)
}




