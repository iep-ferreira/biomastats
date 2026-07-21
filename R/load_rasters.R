#' Load Rasters from Shapefile and Raster Data
#'
#' This function loads raster data from either an online source, a local folder, or a library.
#' It then crops and masks the raster data using a provided shapefile.
#'
#' @param shape_path A character string specifying the path to the shapefile.
#' @param start A numeric value indicating the start year for raster data.
#' @param end A numeric value indicating the end year for raster data.
#' @param method Raster source: `"download"` or `"library"`.
#' @param export_folder_path Directory where downloaded raster files are stored.
#' @param import_folder_path Directory containing an existing raster library.
#' @param type Data type requested from the Biomastats API. Default is "cover".
#' @param collection The collection version. Default is "10".
#'
#' @return A list containing the shapefile, time range, and masked raster data.
#' @importFrom sf st_read st_crs
#' @importFrom raster crop mask crs
#' @export
#'
#' @examples
#' \dontrun{
#' load_rasters("path/to/shapefile.shp", start = 1985, end = 2020, method = "download", export_folder_path = "./maps-personal-library/")
#' }

load_rasters <- function(shape_path = NULL,
                         start = 1985, end = 2020,
                         method = c("download", "library"),
                         export_folder_path = NULL,
                         import_folder_path = NULL,
                         type = "cover",
                         collection = "10"
) { # starts function

  method <- match.arg(method)

  sys_path <- system.file(package = "biomastats")

  if(is.null(shape_path)){
    shape_path <- file.path(sys_path, "shp/polygon.shp")
  }

  # Load the shapefile
  s <- sf::st_read(shape_path)
  s <- sf::st_transform(s, crs=4326)

  s_utm <- sf::st_crs(s)$proj4string
  
  # Identify the fragments required to recompose the study area.
  ids <- unique(compare_shapefiles(shape_path))

  if (method == "download") {
    if (is.null(export_folder_path)) {
      stop("Error! Path for the export maps library is undefined.")
    }
    if (length(ids) == 0L) {
      stop("No map fragments intersect the study area.", call. = FALSE)
    }
    biome_rasters <- check_maps(
      ids = ids,
      start = start,
      end = end,
      export_folder_path = export_folder_path,
      type = type,
      collection = collection
    )
  } else if (method == "library") {
    if (is.null(import_folder_path)) {
      stop("Error! Path for the import maps library is undefined.")
    }
    biome_rasters <- load_rasters_from_folder(start, end, import_folder_path)
  }

    biome_utm <- sf::st_crs(biome_rasters[[1]])$proj4string

  # Check if the CRSs of shapefile and rasters are identical
  if (biome_utm != s_utm) {
    stop("Error. CRSs differed from shp and rasters. CRS must be identical!")
  }

  maps <- NULL
  map <- NULL
  
  # Crop and mask raster data using the shapefile
  for (i in start:end) {
    maps[[i - (start - 1)]] <- raster::crop(biome_rasters[[i - (start - 1)]], s)
    map[[i - (start - 1)]] <- raster::mask(maps[[i - (start - 1)]], s)
  }
  
  return(list("shape" = s, "time_range" = c(start, end), "raster" = map, "collection" = collection))
}




