#' Square Coordinates
#'
#' This function generates the coordinates for a square given a center (latitude and longitude) and a size.
#' The corners are placed at half the diagonal from the center, on the WGS84 ellipsoid, at bearings
#' 45, 135, 225 and 315 degrees (\code{geosphere::destPoint}), so the square has the same size and
#' stays square on the ground at any latitude.
#'
#' @param lat Latitude of the center of the square. Default is 'lat'.
#' @param lon Longitude of the center of the square. Default is 'lon'.
#' @param size Side of the square, in kilometers. Default is 'size'.
#'
#' @return A Polygon object representing the square.
#' @export
#' @examples
#' \dontrun{
#' square_coords(lat = 40.7128, lon = -74.0060, size = 2)
#' }
#' @importFrom sf st_polygon st_sfc
#' @importFrom geosphere destPoint
square_coords <- function(lat = lat, lon = lon, size = size){
  half_diagonal <- size * 1000 / 2 * sqrt(2)

  # Square: northeast, southeast, southwest and northwest corners
  square.coords <- geosphere::destPoint(c(lon, lat), b = c(45, 135, 225, 315), d = half_diagonal)
  square.coords <- unname(rbind(square.coords, square.coords[1, ]))  # Close the polygon


  # Create an sf object
  polygon <- sf::st_polygon(list(square.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)

}
