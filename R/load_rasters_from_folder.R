#' Load Rasters From Folder
#'
#' This function loads raster files from a specified folder path for a given range of years.
#' The raster files are expected to be named in the format "brasil_coverage_year.tif".
#'
#' @param start The start year for which raster files will be loaded.
#' @param end The end year for which raster files will be loaded.
#' @param folder_path The path to the folder containing the raster files.
#'
#' @return A list of raster files loaded from the specified folder path for each year in the range from start to end.
#' @export
#' @examples
#' \dontrun{
#' load_rasters_from_folder(2000, 2005, "/path/to/rasters/")
#' }
#' @importFrom raster raster
#'

load_rasters_from_folder <- function(start, end, folder_path){


  if(is.null(folder_path)){stop("Folder path not specified. NULL value. Provide a valid path!")}

  # loading images of land uses
  biome_rasters <- NULL

  for(i in start:end){

    path_raster <- paste0(folder_path,"/brasil_coverage_", i, ".tif" )

    if(!file.exists(path_raster)){
      stop("Map doesn't exist in your folder or file name doesn't macth the required pattern brasil_coverage_year.tif")
    }

    biome_rasters[[i - (start - 1)]] <- raster::raster(path_raster)
  } # end-for


return(biome_rasters)

}
