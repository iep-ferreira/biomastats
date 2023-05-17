#' Get Google Drive Filename
#'
#' This function retrieves the filename of a Google Drive file from its shareable link.
#'
#' @param file_link The shareable link of the Google Drive file.
#' @return The filename of the Google Drive file.
#' @importFrom rvest read_html html_nodes html_attr
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' \dontrun{
#' get_gdrive_filename("https://drive.google.com/file/d/1m9xxBPaDQz9xxBPaDQz9/view")
#' }
#' @param file_link The shareable link of the Google Drive file.
#'
#' @return The filename of the Google Drive file.
#' @export
#' @examples
#' \dontrun{
#' get_gdrive_filename("https://drive.google.com/file/d/1m9xxBPaDQz9xxBPaDQz9/view")
#' }
#' @importFrom rvest read_html html_nodes html_attr
get_gdrive_filename <- function(file_link) {
  webpage <- rvest::read_html(file_link)
  file_name <- rvest::html_nodes(webpage, xpath = '//meta[@property="og:title"]') %>% rvest::html_attr('content')
  return(file_name)
}

