#' Calculate Landscape Metrics for Classes Over Time
#'
#' This function computes a variety of landscape metrics for a set of raster data over a defined time period. 
#' It generates visualizations and a table of metrics including aggregation index, fragments number, class area, 
#' edge density, edge length, and average fragment area for selected classes.
#'
#' @param rasters_biomastats A list containing raster data and associated time range.
#' @param metrics A character vector specifying which metrics to calculate. 
#'   Available options are: \code{"keep.all"}, \code{"aggregation_index"}, \code{"fragments_number"}, 
#'   \code{"class_area"}, \code{"edge_density"}, \code{"edge_length"}, and \code{"frag_area"}.
#'   Default is \code{c("keep.all", "aggregation_index", "fragments_number", "class_area", "edge_density", "edge_length", "frag_area")}.
#' @param start Numeric value indicating the start year for analysis.
#' @param end Numeric value indicating the end year for analysis.
#' @param zone The zone of interest for calculating the metrics.
#' @param classes A vector of class codes to include in the analysis. If \code{NULL}, all classes are included by default.
#' @param hemisphere A character string specifying the hemisphere ('north' or 'south').
#' @param line_color Color of the lines in the generated plots. Default is \code{"blue"}.
#' @param point_shape Shape of the points in the plots. Default is 16.
#' @param point_size Size of the points in the plots. Default is 3.
#'
#' @return A list containing:
#'   \item{classes}{The classes considered for the analysis.}
#'   \item{ai_plot}{A ggplot object for the aggregation index over time.}
#'   \item{fragments_number}{A ggplot object for the number of fragments over time.}
#'   \item{area_plot}{A ggplot object for the class area over time.}
#'   \item{edge_density_plot}{A ggplot object for edge density over time.}
#'   \item{egde_lenght_plot}{A ggplot object for edge length over time.}
#'   \item{frag_avg_area}{A ggplot object for the average fragment area over time.}
#'   \item{metrics}{A character vector of metrics calculated.}
#'   \item{metrics_table}{A data frame containing the calculated metrics for each year.}
#'
#' @details This function iterates over a time range, calculating landscape metrics for each raster in the input list. 
#'   It checks for valid metric types and generates summary plots for selected metrics.
#'
#' @examples
#' # Example usage:
#' rasters_data <- list(raster = list_of_rasters, time_range = c(2000, 2020))
#' class_metrics(rasters_data, start = 2000, end = 2020, zone = "Zone1", hemisphere = "north")
#'
#' @import ggplot2
#' @importFrom raster raster
#' @importFrom dplyr filter
#' @importFrom scales pretty_breaks
#' @export

biomastats_metrics <- function(rasters_biomastats, metrics = c("keep.all", "aggregation_index", "fragments_number", 
                                                         "class_area", "edge_density", "edge_length", "frag_area"), 
                         start, end, zone, classes = NULL, hemisphere,
                         line_color = "blue", point_shape = 16, 
                         point_size = 3) {
  
  
  # definir as classes
  if( is.null(classes) ){
    selector_classes <- raster_index()
    classes <- selector_classes$codes
  }
  
  # Defina as opções válidas de métricas
  valid_metrics <- c("keep.all", "aggregation_index", "fragments_number", "class_area", "edge_density", "edge_length", "frag_area")
  
  # Verifique se a métrica fornecida é válida
  for(k in 1: length(metrics)){
  if (!metrics[k] %in% valid_metrics) {
    stop(paste("Invalid metric. Choose one of:", paste(valid_metrics, collapse = ", ")))
  }
  }
    
  t <- list()
  table_m <- data.frame( c(start:end) )
  colnames(table_m) <- "Year"
  
  for(i in 1:(end - (start-1))) {
    t[[i]] <- lulc_metrics(rasters_biomastats$raster[[i]], zone = zone, classe = classes, hemisphere = hemisphere)
  }  
  
  #result_metrics$average_area[2,6]
  
  p_frag_avg_area <- NULL
  if("frag_area" %in% metrics | "keep.all" %in% metrics ){  
    frag_area.vector <- NULL  
    for(i in 1:(end - (start-1))) {
      frag_area.vector <- c(frag_area.vector, 
                            as.numeric(t[[i]]$average_area[2,6])) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Avg_frag_area = frag_area.vector)
    table_m <- cbind(table_m, frag_area.vector)
    
    p_frag_avg_area <- ggplot2::ggplot(data, aes(x = Year, y = Avg_frag_area)) +
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "", x = "Year", y = "Frag. Avg. Area (ha)") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, 1.3*max(data$Avg_frag_area)), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  }    
  
  p_edge_length <- NULL
  if("edge_length" %in% metrics | "keep.all" %in% metrics ){  
    ed_length.vector <- NULL  
    for(i in 1:(end - (start-1))) {
      ed_length.vector <- c(ed_length.vector, 
                            as.numeric(t[[i]]$edge_length[,6])) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Edge_length = ed_length.vector)
    table_m <- cbind(table_m, ed_length.vector)
    
    p_edge_length <- ggplot2::ggplot(data, aes(x = Year, y = Edge_length))  +
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "", x = "Year", y = "Edge length (m)") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, 1.3*max(data$Edge_length)), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  }
  
  p_edge_density <- NULL
  if("edge_density" %in% metrics | "keep.all" %in% metrics ){  
    ed.vector <- NULL  
    for(i in 1:(end - (start-1))) {
      ed.vector <- c(ed.vector, as.numeric(t[[i]]$edge_density[6])) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Edge_density = ed.vector)
    table_m <- cbind(table_m, ed.vector)
    
    p_edge_density <- ggplot2::ggplot(data, aes(x = Year, y = Edge_density)) + 
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "", x = "Year", y = "Edge density (units)") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, 1.3*max(data$Edge_density)), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  } 
  
  
  p_area <- NULL
  if("class_area" %in% metrics | "keep.all" %in% metrics ){  
    area.vector <- NULL  
    for(i in 1:(end - (start-1))) {
      area.vector <- c(area.vector, as.numeric(t[[i]]$total_area_for_the_class[2,6])) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Class_area = area.vector)
    table_m <- cbind(table_m, area.vector)
    
    p_area <- ggplot2::ggplot(data, aes(x = Year, y = Class_area)) +
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "", x = "Year", y = "Class area (ha)") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, 1.3*max(data$Class_area)), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  } 
  

  p_ia <- NULL
  if("aggregation_index" %in% metrics | "keep.all" %in% metrics ){  
    ia.vector <- NULL  
    for(i in 1:(end - (start-1))) {
      ia.vector <- c(ia.vector, as.numeric(t[[i]]$index_of_aggregation[2, 6])) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Aggregation_Index = ia.vector)
    table_m <- cbind(table_m, ia.vector)
    
    p_ia <- ggplot2::ggplot(data, aes(x = Year, y = Aggregation_Index)) +
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "Plot of Aggregation Index", x = "Year", y = "Aggregation Index (%)") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, 100), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  } 
  
  p_number <- NULL
  if("fragments_number" %in% metrics | "keep.all" %in% metrics){  
    frag_number.vector <- NULL  
    
    for(i in 1:(end - (start-1))) {
      dimens <- t[[i]]$area_dos_fragmentos %>% dplyr::filter(class != 0) %>% dim
      frag_number.vector <- c(frag_number.vector, dimens[1]) 
    }
    
    years <- start:end
    data <- data.frame(Year = years, Number_of_fragments = frag_number.vector)
    table_m <- cbind(table_m, frag_number.vector)
    
    p_number <- ggplot2::ggplot(data, aes(x = Year, y = Number_of_fragments)) +
      geom_point(shape = point_shape, size = point_size, color = line_color) +
      labs(title = "", x = "Year", y = "Number of fragments") +
      ggplot2::geom_line(linewidth = 0.7) +  # Aumenta a espessura da linha para melhor visualização
      ggplot2::geom_point(size = 1.3) +  # Aumenta o tamanho dos pontos para melhor destaque
      
      # Escala de Y com pelo menos 8 divisões
      ggplot2::scale_y_continuous(limits = c(0, max(data$Number_of_fragments)), breaks = scales::pretty_breaks(n = 8)) + 
      
      # Aumenta a espessura das linhas da grade
      ggplot2::theme_minimal(base_size = 15) +  # Define o tamanho base dos textos (letras maiores)
      
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(color = "gray50", linewidth  = 0.3),  # Grade mais evidente
        panel.grid.minor = ggplot2::element_line(color = "gray100", linewidth = 0.3),  # Grade secundária mais sutil
        axis.title.x = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo X
        axis.title.y = ggplot2::element_text(size = 18),  # Tamanho maior para o rótulo do eixo Y
        axis.text.x = ggplot2::element_text(size = 14),   # Tamanho maior para os ticks do eixo X
        axis.text.y = ggplot2::element_text(size = 14)   # Tamanho maior para os ticks do eixo Y
      )
    
  } 
  
  
  return( list("classes" = classes, "ai_plot" = p_ia, "fragments_number" = p_number, "area_plot" = p_area,
               "edge_density_plot" = p_edge_density, "egde_lenght_plot" = p_edge_length,
               "frag_avg_area" = p_frag_avg_area, 
               "metrics" = metrics, "metrics_table" = table_m) )
}