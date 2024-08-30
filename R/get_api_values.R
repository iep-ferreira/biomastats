#' Parse JSON String and Extract API Values
#'
#' This function attempts to parse a JSON string and extract relevant API values. If the parsing fails,
#' it returns an error message indicating the issue.
#'
#' @param json_string A character string representing the JSON data to be parsed.
#' @return A list containing the extracted values if successful, or an error message if the parsing fails.
#' @importFrom jsonlite fromJSON
#' @export
#'
#' @examples
#' \dontrun{
#' json_input <- '{"result": {"value1": 100, "value2": 200}}'
#' get_api_values(json_string = json_input)
#' }

get_api_values <- function(json_string) {
  # Tentar parsear o JSON
  data <- tryCatch(
    {
      jsonlite::fromJSON(json_string)
    },
    error = function(e) {
      return(list(error = "Erro ao processar o JSON"))
    }
  )
  
  # Verificar se houve erro ao processar o JSON
  if (!is.null(data$error) && data$error == "") {
    return(data$result)
  } else {
    return(paste("Erro de comunicação:", data$error))
  }
}