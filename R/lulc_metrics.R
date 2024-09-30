#' Métricas de Land Use and Land Cover (LULC)
#'
#' Esta função calcula várias métricas de paisagem (Land Use and Land Cover - LULC) com base em um raster de entrada e uma classe de interesse. 
#' As métricas incluem área total da paisagem, área de fragmentos, borda, densidade de borda, e índice de agregação, entre outras. A função 
#' permite a reprojeção do raster para coordenadas métricas UTM e oferece a opção de exportar o raster reclassificado.
#'
#' @param biomastats_raster Raster de entrada que será utilizado para o cálculo das métricas.
#' @param classe Classe ou classes de LULC para análise. Pode ser um valor ou um vetor de valores representando as classes no raster.
#' @param zone Vetor de zonas UTM, com valores padrão entre "18" e "25". Define a zona UTM usada na projeção do raster.
#' @param hemisphere Hemisfério para a projeção UTM. Pode ser "south" (padrão) ou "north".
#' @param export.raster Lógico. Se \code{TRUE}, o raster reclassificado será retornado na saída. O padrão é \code{FALSE}.
#'
#' @return Retorna uma lista com as seguintes métricas:
#' \itemize{
#'   \item \code{classe}: A classe ou classes selecionadas.
#'   \item \code{proj}: A projeção UTM utilizada.
#'   \item \code{total_area}: Área total da paisagem.
#'   \item \code{area_dos_fragmentos}: Área dos fragmentos calculados.
#'   \item \code{edge_length}: Comprimento total da borda.
#'   \item \code{edge_density}: Densidade da borda (metros por hectare).
#'   \item \code{total_area_for_the_class}: Área total da(s) classe(s) selecionada(s).
#'   \item \code{average_area}: Área média dos fragmentos.
#'   \item \code{coeficient_of_variation}: Coeficiente de variação das áreas dos fragmentos.
#'   \item \code{standard_deviation}: Desvio padrão das áreas dos fragmentos.
#'   \item \code{area_of_the_patches}: Tamanho de cada mancha para a(s) classe(s) selecionada(s).
#'   \item \code{index_of_aggregation}: Índice de agregação da classe.
#'   \item \code{reclassified_raster}: O raster reclassificado, se \code{export.raster = TRUE}.
#'   \item \code{reclassified}: Indica se o raster foi reclassificado para múltiplas classes ("Yes" ou "No").
#' }
#'
#' @importFrom dplyr filter mutate
#' @importFrom raster projectRaster getValues setValues
#' @importFrom landscapemetrics lsm_l_ta lsm_p_area lsm_l_te lsm_l_ed lsm_c_ca lsm_c_area_mn lsm_c_area_cv lsm_c_area_sd lsm_c_ai
#' @examples
#' \dontrun{
#' # Exemplo de uso:
#' # Calcular métricas para a classe 3 no raster de entrada:
#' lulc_metrics(biomastats_raster, classe = 3, zone = "23", hemisphere = "south")
#' }
#' @export

lulc_metrics <- function(biomastats_raster, classe, zone = c("18", "19", "20", "21", "22", "23", "24", "25"), 
hemisphere = c("south", "north"), export.raster = FALSE)
{
  
  ##Definindo a projeção
  proj_raster <- paste0("+proj=utm +zone=", zone, " +", hemisphere, " +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  ##Selecionando a classe de lulc para análise
  braster_selected <- biomastats_raster
  braster_values <- raster::getValues(braster_selected)
  
  # Selecionando os valores que estão na classe
  braster_values[(!braster_values %in% classe) & !is.na(braster_values)] <- 0
  
  # Se classe tiver mais de um valor, atribui 1 para os que estão em classe
  if(length(classe) > 1) {
    braster_values[braster_values %in% classe] <- 1
  }
  
  # Atribuindo os valores de volta ao raster
  braster_selected <- aux_raster <- raster::setValues(braster_selected, braster_values)
  
  braster_selected  <- raster::projectRaster(braster_selected, crs = proj_raster, method="ngb")
  
  ##Calculando a área da paisagem
  total_area<-  landscapemetrics::lsm_l_ta(braster_selected, directions=8)
  
  ## Calculando a área total da classificação 3
  area_frag <- landscapemetrics::lsm_p_area(braster_selected, directions=8)
  
  ## Calculando a área de borda
  
  total_edge <- landscapemetrics::lsm_l_te(braster_selected, count_boundary=FALSE)
  
  ## Calculando a densidade da borda. Importante, aqui o resultado nos é dado em metros por hectare.
  
  edge_density <- landscapemetrics::lsm_l_ed(braster_selected, count_boundary = FALSE, directions = 8)
  
  ##Área total da classe
  
  class_area <- landscapemetrics::lsm_c_ca(braster_selected, directions = 8)
  
  
  ##Estatística de resumo dos fragmentos
  mean_frag <- landscapemetrics::lsm_c_area_mn(braster_selected, directions = 8)
  cv_frag <- landscapemetrics::lsm_c_area_cv(braster_selected, directions = 8)
  sd_frag <- landscapemetrics::lsm_c_area_sd(braster_selected, directions = 8)
  
  ## Métricas para as manchas
  ##Calculando o tamanho de cada mancha agora, utilizando a regra de ligação dos 8.
  aux_sizes <- landscapemetrics::lsm_p_area(braster_selected, directions = 8)
  if(length(classe) == 1){
    frag_sizes<-aux_sizes %>% dplyr::filter(class == classe)
  } else{ frag_sizes<-aux_sizes %>% dplyr::filter(class == 1)}
  
  
  #Índice de agregação
  ai <- landscapemetrics::lsm_c_ai (braster_selected)
  
  rec_raster <- NULL
  if(export.raster == TRUE){rec_raster <- aux_raster}
  
  return( list(classe = classe, 
               proj=proj_raster, 
               total_area=total_area, 
               area_dos_fragmentos=area_frag,
               edge_length=total_edge,
               edge_density=edge_density,
               total_area_for_the_class=class_area,
               average_area=mean_frag,
               coeficient_of_variation=cv_frag,
               standard_deviation=sd_frag,
               area_of_the_patches=frag_sizes,
               index_of_aggregation=ai,
               reclassified_raster=rec_raster,  
               reclassified=ifelse(length(classe) == 1, "No", "Yes") 
  ) )
}