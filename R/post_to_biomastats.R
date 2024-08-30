#' Send Fragment Data to Biomastats API
#'
#' This function sends a POST request to the Biomastats API with the specified fragment and year. 
#' It handles different URL options and returns the response content in JSON format.
#'
#' @param fragment A numeric or character value representing the fragment ID.
#' @param year A numeric value representing the year associated with the fragment.
#' @param url An optional character string specifying the URL for the API. Defaults to the Biomastats API URL.
#' @return The content of the API response in JSON format as a character string, or an error message if the request fails.
#' @importFrom httr POST add_headers content status_code
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' post_to_biomastats(fragment = 1, year = 2024)
#' post_to_biomastats(fragment = 2, year = 2023, url = "https://custom-api.com/api/link")
#' }

post_to_biomastats <- function(fragment, year, url = NULL) {
  
  url_1 <- "https://biomastats.rgestor.com.br/api/link"
  url_2 <- "https://2chat.pro/api/link"
  
  if(is.null(url)){
    url <- url_1
  } 
  if(url == 1) {url <- url_1}
  if(url == 2) {stop("Connection failure!")}
  
  
  # Parâmetros a serem enviados na solicitação POST
  params <- list(
    fragment = fragment,
    year = year
  )
  
  # Fazer a solicitação POST
  response <- httr::POST(url, 
                         add_headers(
                           'accept' = 'application/json',
                           'Content-Type' = 'application/x-www-form-urlencoded'
                         ),
                         body = params, 
                         encode = "form")
  content <- NULL
  if (status_code(response) == 200) {
    # Converter o conteúdo da resposta para JSON
    content <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(content)
  } 
  #else{
  #  url <- url_2
  #  response <- POST(url, 
  #                 add_headers(
  #                   'accept' = 'application/json',
  #                   'Content-Type' = 'application/x-www-form-urlencoded'
  #                 ),
  #                 body = params, 
  #                 encode = "form")
  # Converter o conteúdo da resposta para JSON
  #  content <- content(response, "text", encoding = "UTF-8")
  #  json_data <- fromJSON(content)
  #}
  
  return(content)
}