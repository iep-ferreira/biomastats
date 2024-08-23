#' Check if Required Maps Exist in Directory and Construct Download Queue
#'
#' @param ids Vector of map fragment IDs
#' @param start Starting year
#' @param end Ending year
#' @return List of downloaded maps
#' @importFrom raster raster
#' @importFrom raster merge
#' @importFrom raster writeRaster
#' @export
#'
#' @examples
#' \dontrun{
#' check_maps(ids = c(1, 2, 3), start = 1990, end = 2000)
#' }

check_maps <- function(ids, start, end) {

  sys_path <- system.file(package = "biomastats")

  # Define the output directories
  dir_path <- paste0(sys_path,"/maps-library")
  data_path <- paste0(sys_path, "/data")

  # Create the output directories if they don't exist
  if (!dir.exists(data_path)) {
    dir.create(data_path)
  }
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }

  # Path to the file storing the metadata
  mosaic_id_file <- file.path(data_path, "mosaic.rds")

  # Read the existing mosaic IDs or initialize an empty list
  if (file.exists(mosaic_id_file)) {
    mosaic_ids <- readRDS(mosaic_id_file)
  } else {
    mosaic_ids <- list()
  }

  maps <- list()  # Initialize the list of downloaded maps


  for (year in start:end) {

    # remover depois o que está comentado
    #map_name <- file.path(dir_path, paste0("map-", year, ".tif"))
    any.change <- FALSE # There are changes in maps


   # if (!file.exists(map_name)) {  # If the map doesn't exist yet
   # Trabalharemos apenas com a condição de que o mapa não exista, pois não queremos manter mapas anuais. 
   
      any.change <- TRUE # There are changes in maps

      for (id in ids) {
        file_name <- file.path(dir_path, paste0("coverage-frag-", id, "-year-", year, ".tif"))

        if (id == ids[1]) {  # Download the first fragment
          message("Downloading the map fragment!")
          cat(file_name, "\n")
          download_maps(num_year = year, fragment_id = id)
          current_fig <- raster::raster(file_name)
          mosaic_ids[[year]] <- c(id)
          saveRDS(mosaic_ids, mosaic_id_file)
        } else {
          message("Downloading the map fragment!")
          cat(file_name, "\n")
          download_maps(num_year = year, fragment_id = id)
          temp_fig <- raster::raster(file_name)
          current_fig <- raster::merge(current_fig, temp_fig)
          mosaic_ids[[year]] <- c(mosaic_ids[[year]], id)
          saveRDS(mosaic_ids, mosaic_id_file)
        }

      } # end - for

 #    } else {  # If the map already exists

   #   current_fig <- raster::raster(map_name)

   #   for (id in ids) {

   #     file_name <- file.path(dir_path, paste0("coverage-frag-", id, "-year-", year, ".tif"))

   #     if (id %in% mosaic_ids[[year]]) {
   #       message("The map fragment is already downloaded.")
   #       cat(file_name, "\n")
   #     } else {
   #       message("Downloading the map fragment!")
   #       cat(file_name, "\n")
   #       download_maps(num_year = year, fragment_id = id)
   #       temp_fig <- raster::raster(file_name)
   #       current_fig <- raster::merge(current_fig, temp_fig) # está lento por causa do merge!
   #       mosaic_ids[[year]] <- c(mosaic_ids[[year]], id)
   #       saveRDS(mosaic_ids, mosaic_id_file)
   #       any.change <- TRUE # There are changes in maps
   #     } # end-else
   #   } # end-for

 #    } # end-else (if maps already exists!!)

 #   if(any.change){
 #     cat("Mapas atualizados! /n")

 #     writeRaster(current_fig, map_name, format = "GTiff", overwrite = TRUE)

 #   } # end-if

    maps[[year - (start - 1)]] <- current_fig

  } # end-for year

  saveRDS(mosaic_ids, mosaic_id_file)

  return(maps)

} # end-function
