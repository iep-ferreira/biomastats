#' Download Maps
#'
#' This function downloads maps using a list of links.
#'
#' @param num_year Year number of the map to be downloaded.
#' @param fragment_id ID of the map fragment to be downloaded.
#' @return No return value, called for side effects.
#' @importFrom readxl read_excel
#' @importFrom dplyr filter

#'
#' @examples
#' \dontrun{
#'   download_maps(num_year = 2022, fragment_id = 1, file_name)
#' }

download_maps <- function(num_year, fragment_id, file_name){


obj <-  post_to_biomastats(fragment_id , num_year)

  # Check if there is a link available for the specified file
  if(is.null(obj)){
    cat("No available link for the following file \n")
    cat(file_name)
    cat("\n")
    stop("Error.")
  }

response <- get_api_values(obj)

## Falta adicionar verificação de soma MD5 

  # Download the file
  download_public_gdrive_file(file_link = response$shareable_link, local_path = file_name)
  
}
