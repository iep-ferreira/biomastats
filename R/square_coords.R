#' Square Coordinates
#'
#' This function generates the coordinates for a square given a center (latitude and longitude) and an adjustment size.
#' The square's vertices are calculated based on the geometric properties of a square.
#'
#' @param lat Latitude of the center of the square. Default is 'lat'.
#' @param lon Longitude of the center of the square. Default is 'lon'.
#' @param adj_size Adjustment size for the square. This will affect the size of the square. Default is 'adj_size'.
#'
#' @return A Polygon object representing the square.
#' @export
#' @examples
#' \dontrun{
#' square_coords(lat = 40.7128, lon = -74.0060, adj_size = 1)
#' }
#' @importFrom sf st_polygon st_sfc
square_coords <- function(lat = lat, lon = lon, adj_size = adj_size){
  # Square
  square.coords <- rbind(
    c(lon + adj_size, lat + adj_size),
    c(lon + adj_size, lat - adj_size),
    c(lon - adj_size, lat - adj_size),
    c(lon - adj_size, lat + adj_size),
    c(lon + adj_size, lat + adj_size)  # Add the first point at the end to close the polygon
  )


  # Create an sf object
  polygon <- sf::st_polygon(list(square.coords))
  sf_object <- sf::st_sfc(polygon)

  return(sf_object)

}
