#' Compare Shapefiles and Identify Required Fragments
#'
#' This function compares the provided shapefile of interest with the fragmented shapefile of Brazil
#' and identifies which fragments are necessary for the analysis.
#'
#' @param shape_path Path to the shapefile of interest
#' @return Vector of fragment IDs required for the analysis
#' @importFrom sf st_read
#' @importFrom sf st_transform
#' @importFrom sf st_crs
#' @importFrom sf st_intersection
#' @export
#'
#' @examples
#' \dontrun{
#' compare_shapefiles(shape_path = "path/to/your/shapefile.shp")
#' }


compare_shapefiles <- function(shape_path) {

  sys_path <- system.file(package = "biomastats")

  # Read the shapefiles
  s <- sf::st_read(shape_path)
  brasil_cortado_path <- paste0(sys_path, "/ex_shp/brasil_cortado.shp" )
  s_brasil <- sf::st_read(brasil_cortado_path)

  # Transform the shapefiles to the same coordinate reference system (CRS)
  s <- sf::st_transform(s, sf::st_crs(s_brasil))

  # Find the feature intersections
  overlaps <- sf::st_intersection(s_brasil, s)

  # Return the IDs of the required fragments
  ids <- as.integer(rownames(overlaps))

  return(ids)
}
