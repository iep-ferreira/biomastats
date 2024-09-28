#' Hexagon Coordinates
#'
#' This function generates the coordinates for a regular hexagon given a center (latitude and longitude) and an adjustment size.
#' The hexagon's vertices are calculated based on the geometric properties of a regular hexagon.
#'
#' @param lat Latitude of the center of the hexagon. Default is 'lat'.
#' @param lon Longitude of the center of the hexagon. Default is 'lon'.
#' @param adj_size Adjustment size for the hexagon. This will affect the size of the hexagon. Default is 'adj_size'.
#'
#' @return A Polygon object representing the hexagon.
#' @export

#' @examples
#' \dontrun{
#' hexagon_coords(lat = 40.7128, lon = -74.0060, adj_size = 1)
#' }
#' @importFrom sf st_polygon st_sfc
hexagon_coords <- function(lat = lat, lon = lon, adj_size = adj_size){
  # Hexagon
  hexagon.coords <- rbind(
    c(adj_size + lon, lat),
    c(lon + adj_size/2, lat + adj_size*sqrt(3)/2),
    c(lon - adj_size/2, lat + adj_size*sqrt(3)/2),
    c(lon - adj_size, lat),
    c(lon - adj_size/2, lat - adj_size*sqrt(3)/2),
    c(lon + adj_size/2, lat - adj_size*sqrt(3)/2),
    c(adj_size + lon, lat)  # Add the first point at the end to close the polygon
  )

  # Create an sf object
  polygon <- sf::st_polygon(list(hexagon.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)
}
