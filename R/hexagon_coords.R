#' Hexagon Coordinates
#'
#' This function generates the coordinates for a regular hexagon given a center (latitude and longitude) and a size.
#' The vertices are placed at half the size from the center, on the WGS84 ellipsoid, every 60 degrees of
#' bearing starting due east (\code{geosphere::destPoint}), so the hexagon has the same size and stays
#' regular on the ground at any latitude.
#'
#' @param lat Latitude of the center of the hexagon. Default is 'lat'.
#' @param lon Longitude of the center of the hexagon. Default is 'lon'.
#' @param size Distance between opposite vertices of the hexagon, in kilometers. Default is 'size'.
#'
#' @return A Polygon object representing the hexagon.
#' @export

#' @examples
#' \dontrun{
#' hexagon_coords(lat = 40.7128, lon = -74.0060, size = 2)
#' }
#' @importFrom sf st_polygon st_sfc
#' @importFrom geosphere destPoint
hexagon_coords <- function(lat = lat, lon = lon, size = size){
  radius <- size * 1000 / 2

  # Hexagon: vertices counterclockwise from east
  hexagon.coords <- geosphere::destPoint(c(lon, lat), b = 90 - 60 * (0:5), d = radius)
  hexagon.coords <- unname(rbind(hexagon.coords, hexagon.coords[1, ]))  # Close the polygon

  # Create an sf object
  polygon <- sf::st_polygon(list(hexagon.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)
}
