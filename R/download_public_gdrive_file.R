#' Download Public Google Drive File
#'
#' This function downloads a file from a public Google Drive link.
#'
#' @param file_link Link to the file on Google Drive.
#' @param local_path Path to save the file locally.
#' @return No return value, called for side effects.
#' @importFrom httr GET
#' @importFrom httr write_disk
#' @importFrom httr status_code
#' @export
#'
#' @examples
#' \dontrun{
#'   download_public_gdrive_file(file_link = "https://drive.google.com/file/d/xxxx", local_path = "./local_dir/my_file.txt")
#' }
download_public_gdrive_file <- function(file_link, local_path) {

  # Extract the file ID from the link
  file_id <- sub(".*file/d/([^/]+).*", "\\1", file_link)
  download_url <- paste0("https://drive.google.com/uc?export=download&id=", file_id)

  max_tries <- 10  # maximum number of download attempts

  for (k in 1:max_tries) {

    # Attempt to download the file
    response <- httr::GET(download_url, httr::write_disk(local_path, overwrite = TRUE))

    # Check if the download was successful
    if (response$status_code == 200) {
      cat("File downloaded successfully.\n")
      cat(get_gdrive_filename(file_link))
      cat("\n")
      break
    } else {
      # Handle different error types
      error_file <- get_gdrive_filename(file_link)
      if(response$status_code == 403) cat("Transfer rate limit reached.\n")
      if(response$status_code == 429) cat("Too many requests.\n")
      cat(paste0("Failed attempt ", k, " to download the file.\n"))
      cat(error_file)
      cat("\n")
      if (k == max_tries) {
        cat(error_file)
        cat("\n")
      } # end of internal if
    } # end of else

    Sys.sleep(5)  # optional: wait 5 seconds before the next attempt
  } # end of for loop
}
