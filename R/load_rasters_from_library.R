#' Load Rasters From Library
#'
#' This function loads raster files from a specified library path for a given range of years.
#' The raster files are expected to be named in the format "map-year.tif".
#'
#' @param start The start year for which raster files will be loaded.
#' @param end The end year for which raster files will be loaded.
#'
#' @return A list of raster files loaded from the specified library path for each year in the range from start to end.
#' @export
#' @examples
#' \dontrun{
#' load_rasters_from_library(2000, 2005)
#' }
#' @importFrom raster raster
load_rasters_from_library <- function(start, end){

  sys_path <- system.file(package = "biomastats")

  biome_rasters <- NULL
  for(i in start:end){
    path_raster <- file.path(sys_path, paste0("maps-library/map-", i, ".tif")  )

    if(!file.exists(path_raster)){
      stop("Map doesn't exist in your library. Download it first!")
    }

    biome_rasters[[i - (start - 1)]] <- raster::raster(path_raster)
  } # end-for

return(biome_rasters)

}
