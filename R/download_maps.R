#' Download Maps
#'
#' This function downloads maps using a list of links.
#'
#' @param num_year Year number of the map to be downloaded.
#' @param fragment_id ID of the map fragment to be downloaded.
#' @return No return value, called for side effects.
#' @importFrom readxl read_excel
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' \dontrun{
#'   download_maps(num_year = 2022, fragment_id = 1)
#' }

download_maps <- function(num_year, fragment_id){

  sys_path <- system.file(package = "biomastats")

  links_path <- file.path(sys_path, "shared-links/all-links.xlsx")

  # Attempt to read the file
  if (file.exists(links_path)) {
    df <- readxl::read_excel(links_path)
  } else {
    # Display error message if file not found
    stop("Error: Links not found. The file `all-links` should be copied from the GitHub repository!")
  }

  dir_path <- file.path(sys_path, "maps-library")

  # Construct file name
  file_name <- paste0(dir_path,"/coverage-frag-", fragment_id, "-year-", num_year, ".tif")

  # Filter for the specific year and fragment
  link_info <- filter(df, year == num_year & fragment == fragment_id)

  # Check if there is a link available for the specified file
  if(is.null(link_info)){
    cat("No available link for the following file \n")
    cat(file_name)
    cat("\n")
    stop("Error.")
  }

  # Download the file
  download_public_gdrive_file(file_link = link_info$shareable_link[1], local_path = file_name)

}
