#' Create a Geographical Polygon with Specified Size and Shape
#'
#' @param lat A numeric latitude value of the center of the polygon.
#' @param lon A numeric longitude value of the center of the polygon.
#' @param size Total width of the shape, in kilometers: diameter of the circle,
#'   side of the square or distance between opposite vertices of the hexagon.
#' @param shape A character string specifying the shape of the polygon, either "circle", "square", or "hexagon".
#'
#' @return The created polygon
#' @details The vertices are placed on the WGS84 ellipsoid by
#'   [circle_coords()], [square_coords()] and [hexagon_coords()], so the
#'   polygon has the same size and proportions on the ground at any latitude.
#'   It is returned and written in WGS84 (EPSG:4326).
#' @importFrom sf st_crs st_write st_set_crs
#' @export
#'
#' @examples
#' \dontrun{
#' make_polygon(lat = 40.7128, lon = -74.0060, size = 10, shape = "circle")
#' }
make_polygon <- function(lat = NULL, lon = NULL, size = NULL, shape = c("circle", "square", "hexagon") ){
  # Check if latitude and longitude are provided
  if (is.null(lat) | is.null(lon)){
    stop("Please provide latitude and longitude.")
  }

  # Check if buffer size is provided
  if(length(size)==0){
    stop("Please provide buffer size, in km.")
  }

  # Check if the specified buffer shape is valid
  if(shape %in% c("circle", "square", "hexagon") != 1 ){
    stop("Invalid buffer shape. Please enter 'circle', 'square', or 'hexagon'.")
  }

  # Create polygons based on the specified size (total width, in km) and geometry options
  poly_sf <- switch(shape,
                        "circle" = circle_coords(lat, lon, size),
                        "square" = square_coords(lat, lon, size),
                        "hexagon" = hexagon_coords(lat, lon, size),
                        stop("Invalid geometry. Please enter 'circle', 'square', or 'hexagon'.")
  )

  # the EPSG code 4326 corresponds to the WGS84 CRS, which is commonly used for representing latitude
  # and longitude coordinates on a spherical globe
  # Set the coordinate reference system (CRS)
  sf_object <- sf::st_set_crs(poly_sf, 4326)

  sys_path <- system.file(package = "biomastats")

  download_path <- file.path(sys_path, "shp/polygon.shp")

  # Export the shapefile
  sf::st_write(sf_object, download_path , delete_layer = TRUE)

  message("Shapefile created")
  message("Shapefile stored in ./shp as polygon.shp")
  cat("biomastats directory: ", sys_path)

  # Return the created buffer
  return(list("polygon" = sf_object))
}
