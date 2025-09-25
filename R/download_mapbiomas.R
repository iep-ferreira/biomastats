#' Download MapBiomas data
#'
#' This function downloads MapBiomas land use and land cover data for specified parameters.
#' It supports different data types, collections, and time ranges.
#'
#' @param dest_dir Directory where files will be saved. Must be provided by user.
#' @param data_type Type of data to download. Currently supports "cover" (land cover classification).
#'   Default is "cover".
#' @param collection Collection version. Default is 10.
#' @param time_range Vector with start and end years. Default is c(1985, 2024).
#' @param quiet Logical. If TRUE, suppresses progress messages. Default is FALSE.
#'
#' @return Invisible vector of downloaded file paths
#' @export
#'
#' @examples
#' \dontrun{
#' # Download cover data for collection 10, years 1985-2024 to specific directory
#' download_mapbiomas(dest_dir = "~/mapbiomas_data")
#'
#' # Download only for specific years
#' download_mapbiomas(dest_dir = "~/meus_dados", time_range = c(2020, 2022))
#'
#' # Download with silent mode
#' download_mapbiomas(dest_dir = "~/mapbiomas", quiet = TRUE)
#' }
download_mapbiomas <- function(dest_dir, 
                              data_type = "cover", 
                              collection = 10, 
                              time_range = c(1985, 2024),
                              quiet = FALSE) {
  
  # Validate required parameters
  if (missing(dest_dir)) {
    stop("dest_dir must be provided. Please specify the directory where files will be saved.")
  }
  
  # Validate other parameters
  if (!data_type %in% c("cover")) {
    stop("Currently only 'cover' data type is supported")
  }
  
  if (collection != 10) {
    stop("Currently only collection 10 is supported")
  }
  
  if (length(time_range) != 2 || !is.numeric(time_range)) {
    stop("time_range must be a numeric vector of length 2: c(start_year, end_year)")
  }
  
  if (time_range[1] > time_range[2]) {
    stop("Start year must be less than or equal to end year")
  }
  
  # Create destination directory if it doesn't exist
  if (!dir.exists(dest_dir)) {
    if (!quiet) {
      message("Creating directory: ", dest_dir)
    }
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Verify directory was created successfully
  if (!dir.exists(dest_dir)) {
    stop("Failed to create destination directory: ", dest_dir)
  }
  
  # Create years sequence
  anos <- seq(time_range[1], time_range[2])
  
  # Initialize vector to store downloaded files
  downloaded_files <- character(0)
  
  if (!quiet) {
    message("Starting download of MapBiomas collection ", collection, " ", data_type, " data")
    message("Years: ", time_range[1], " to ", time_range[2])
    message("Destination: ", dest_dir)
  }
  
  # Loop through each year
  for(ano in anos) {
    # Construct URL
    url <- paste0("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_", 
                  collection, "/lulc/coverage/brazil_coverage_", ano, ".tif")
    
    # Define destination filename
    dest_file <- file.path(dest_dir, paste0("brasil_coverage_", ano, ".tif"))
    
    # Skip if file already exists
    if (file.exists(dest_file)) {
      if (!quiet) {
        message("✓ ", ano, " already exists, skipping download")
      }
      downloaded_files <- c(downloaded_files, dest_file)
      next
    }
    
    tryCatch({
      # Download file
      if (!quiet) {
        message("Downloading ", ano, "...")
      }
      
      download.file(url = url, 
                    destfile = dest_file, 
                    mode = "wb", 
                    quiet = quiet)
      
      # Check if file was downloaded successfully
      if (file.exists(dest_file) && file.size(dest_file) > 0) {
        downloaded_files <- c(downloaded_files, dest_file)
        if (!quiet) {
          message("✓ ", ano, " downloaded successfully")
        }
      } else {
        if (!quiet) {
          warning("✗ Download may have failed for year ", ano, " - file is empty or missing")
        }
      }
      
    }, error = function(e) {
      if (!quiet) {
        message("✗ Error downloading year ", ano, ": ", conditionMessage(e))
      }
    })
  }
  
  # Summary message
  if (!quiet) {
    message("\nDownload completed!")
    message("Successfully downloaded: ", length(downloaded_files), " files")
    message("Location: ", dest_dir)
  }
  
  return(invisible(downloaded_files))
}
