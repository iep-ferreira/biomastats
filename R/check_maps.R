#' Check if Required Maps Exist in Directory and Construct Download Queue
#'
#' @param ids Vector of map fragment IDs
#' @param start Starting year
#' @param end Ending year
#' @return List of downloaded maps
#' @importFrom raster raster
#' @importFrom raster merge
#'
#' @examples
#' \dontrun{
#' check_maps(ids = c(1, 2, 3), start = 1990, end = 2000, export_folder_path)
#' }

check_maps <- function(ids, start, end, export_folder_path) {

dir_path <- export_folder_path

maps <- list()

print(ids)

 for (year in start:end) {

 for (id in ids) {
        
        file_name <- file.path(dir_path, paste0("coverage-frag-", id, "-year-", year, ".tif"))

        if(!file.exists(file_name)){
          message("Downloading the map fragment!")
          cat(file_name, "\n")
          download_maps(num_year = year, fragment_id = id, file_name = file_name)
          } else{
          message("The map fragment is already downloaded.")
          cat(file_name, "\n")
        } # end-if
        
        if (id == ids[1]) {  
        # Download the first fragment
          current_fig <- raster::raster(file_name)
          
        } else {
          
          temp_fig <- raster::raster(file_name)
          current_fig <- raster::merge(current_fig, temp_fig)          
        
        } # end-for ids

      } # end-for years
 
    maps[[year - (start - 1)]] <- current_fig

  } # end-for year

  return(maps)

} # end-function
