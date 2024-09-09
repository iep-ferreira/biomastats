#' Calculate Land Classes Area
#'
#' This function calculates the area of land classes over a specified time range.
#'
#' @param data A list containing two elements: 
#' \describe{
#'   \item{time_range}{A vector of two elements specifying the start and end years.}
#'   \item{raster}{A list of raster objects for each year in the time range.}
#' }
#' @param dec_places An integer indicating the number of decimal places to round the area values.
#' @param plot_type A character string indicating the type of plot to generate, either "profile" (default) or "areaplot".
#'
#' @return A list with four elements:
#' \describe{
#'   \item{Years}{A vector of years covered by the time range.}
#'   \item{Occupied area}{A list with the calculated areas for each land class and year.}
#'   \item{aggregate_data}{A data frame with the calculated areas for each land class and year.}
#'   \item{time_series}{A ggplot2 object with the time series of areas, either a line plot or an area plot.}
#' }
#' @importFrom ggplot2 ggplot aes scale_color_manual scale_fill_manual geom_line geom_point geom_area theme_minimal theme element_line element_text scale_y_continuous
#' @importFrom graphics par
#' @importFrom raster area getValues
#' @importFrom scales pretty_breaks
#' @export
#'
#' @examples
#' \dontrun{
#' result <- get_area(data = biomastats_raster, plot_type = "areaplot")
#' }

get_area <- function(data = NULL, dec_places = 3, plot_type = c("areaplot")) {

  # Dictionary
  dic <- dict_build()

  # Time span of study
  time_span <- (data$time_range[2] - data$time_range[1]) + 1
  time_start <- data$time_range[1]
  
  areas <- NULL
  for(pos in 1:time_span) {
    areas_y <- raster::area(data$raster[[pos]])
    v_r <- raster::getValues(data$raster[[pos]])
    areas[[pos]] <- tapply(areas_y, v_r, sum)
  } # fim for

  df <- NULL
  for(pos in 1:time_span) {
    aux <-  data.frame( rep(pos + time_start - 1, dim(areas[[pos]])) ,  round(areas[[pos]],dec_places))
    df <- rbind(df, cbind(names(areas[[pos]]), aux))
  } # fim for
  colnames(df) <- c("land_class", "year", "area")

  land_class_name <- NULL
  land_class_color <- NULL
  for(j in 1:length(df$land_class)) {
    aux_2 <- df$land_class[j] == dic$code
    land_class_name[j] <- dic$class[aux_2]
    land_class_color[j] <- dic$color[aux_2]
  }
  df$land_class_name <- land_class_name

  if(plot_type == "profile"){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = area, group = land_class_name, color = land_class_name)) +
    ggplot2::scale_color_manual(values = land_class_color, limits = land_class_name, name = "Land Use") +
    ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
    ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
    ggplot2::ylab(expression(paste("Area (", km^2, ")"))) +
    ggplot2::xlab("Years") +
    
    # Escala de Y com pelo menos 8 divisões
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
    
    # Aumenta a espessura das linhas da grade
    ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
    
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
      panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
      axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
      axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
      axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
      axis.text.y = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo Y
      legend.position = "right"
    )
  }

  if(plot_type == "areaplot"){
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = area, group = land_class_name, fill = land_class_name)) +
    ggplot2::scale_fill_manual(values = land_class_color, limits = land_class_name, name = "Land Use") +
    
    # Usar geom_area para criar o gráfico de áreas empilhadas
    ggplot2::geom_area(alpha = 0.6, linewidth = 0.5, color = "black") +  # Define a transparência e as bordas
    
    ggplot2::ylab(expression(paste("Area (", km^2, ")"))) +
    ggplot2::xlab("Years") +
    
    # Escala de Y com pelo menos 8 divisões
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + 
    
    # Aumenta a espessura das linhas da grade
    ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
    
    ggplot2::theme(
      panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
      panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
      axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
      axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
      axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
      axis.text.y = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo Y
      legend.position = "right"
    )
  }
  
  if( !plot_type %in% c("profile", "areaplot")  ){ 
    p <- NULL
    message("Undefined plot type. Choose 'areaplot' or 'profile'! ")
  }
  
  return(
    list(
      "Years" = data$time_range[1]:data$time_range[2],
      "Occupied area" = areas,
      "aggregate_data" = df,
      "time_series" = p
    )
  )
}