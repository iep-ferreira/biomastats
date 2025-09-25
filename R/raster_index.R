#' Índice de Classes de Uso e Cobertura da Terra (LULC)
#'
#' Esta função cria um índice de classes de uso e cobertura da terra (LULC) com base em um dicionário de dados predefinido, 
#' permitindo ao usuário selecionar múltiplas classes para análise. A função atualmente suporta apenas dados do tipo "lulc" e da coleção 7.
#'
#' @param raster_type Tipo de raster. O valor padrão é "lulc". Outros tipos ainda não estão disponíveis.
#' @param collection Coleção de dados a ser utilizada. Atualmente, apenas a coleção 7 está disponível.
#' @param language Idioma utilizado para os rótulos de seleção. As opções são "Portuguese" ou "English". O valor padrão é "English".
#'
#' @return Retorna uma lista com as seguintes informações:
#' \itemize{
#'   \item \code{groups}: O grupo de classes selecionadas.
#'   \item \code{codes}: Os códigos das classes selecionadas.
#'   \item \code{codes_color}: As cores associadas aos códigos selecionados.
#'   \item \code{class}: Os nomes das classes selecionadas.
#' }
#'
#' @importFrom dplyr filter
#' @importFrom utils select.list
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' raster_index(raster_type = "lulc", collection = 7, language = "English")
#' }
#' @export


raster_index <- function(raster_type = "lulc", collection = 7, language = "English") {
  
  if(raster_type != "lulc"){stop("Only 'lulc' data is avalaible in this version.")} 
  
  if(collection != 7){stop("Only the collection 7 is avalaible in this version.")} 
  
  if(!language %in% c("Portuguese", "English")){stop("Only 'Portuguese' or 'English' are valid options.")}
  
  # no futuro, o dicionário será escolhido de acordo com o 
  # tipo de mapa e coleção
  dict <- biomastats:::dict_build(collection)
  dict <- dict_sorted <- dict[order(dict$code), ]
  dict <- dplyr::filter(dict, !code  %in% c(1, 10, 14, 22, 26) )
  
  if(language == "Portuguese"){  
    selector_text <- "Selecione os itens (use espaço para selecionar múltiplos itens):"} else{
      selector_text <- "Select the items (use space to select multiple items):"  
    }
  
  selected_class <- utils::select.list(dict$class, graphics = TRUE, title = selector_text, multiple = TRUE)
  dict_filtered <- dplyr::filter(dict, class %in% selected_class)
  
  if (length(selected_class) == 0) {
    return(NULL)
  }
  
  if(language == "Portuguese"){
    selected_group <- dict_filtered$grupo
    selected_class <- dict_filtered$classe
    selected_codes <- dict_filtered$code
    selected_color <- dict_filtered$color
  } else{
    selected_group <- dict_filtered$group
    selected_class <- dict_filtered$class
    selected_codes <- dict_filtered$code
    selected_color <- dict_filtered$color  
  }
  
  return(list(groups = selected_group, codes = selected_codes, codes_color = selected_color, class = selected_class))
}
