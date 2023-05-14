#' Land Distribution for a Specified Year
#'
#' This function creates a plot (either pie chart or barplot) showing the distribution of different land classes in a specific year.
#'
#' @param data A list that includes 'aggregate_data', a dataframe with the calculated areas for each land class and year.
#' @param year A specific year to visualize the land distribution.
#' @param type The type of plot to generate, either "pie" or "barplot".
#' @return A plot showing the distribution of different land classes in a specific year.
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 coord_polar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 theme_void
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom dplyr %>%
#' @examples
#' \dontrun{
#' land_dist(data = result, year = 1985, type = "barplot")
#' }

land_dist <- function(data = NULL, year = NULL, type = "barplot" ){

  # Dictionary
  dic <- dict_build()

  #
  aux <- year

  df <- data$aggregate_data
  dat <- df %>% dplyr::filter(year == aux)

  names_classes <- NULL
  cores_classes <- NULL
  for(j in 1:length(dat$land_class)){
    pos <- dat$land_class[j] == dic$code
    names_classes[j] <- dic$class[pos]
    cores_classes[j] <- dic$color[pos]
  }

  dat$cores<- cores_classes
  dat$names<- names_classes
  dat$percentual <- round(dat$area / sum(dat$area) * 100, 2)

  head(dat,7)

  if(type!="pie"&type!="barplot"){
    print("Type not available yet!")
  }

  if(type == "pie"){
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = "", y = area, fill = names)) +
      ggplot2::geom_bar(width = 1, stat = "identity") +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::scale_fill_manual(values = dat$cores,
                        limits = dat$names,
                        name = "Land Uses") +
      ggplot2::theme_void() +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::ggtitle(paste("Year: ", as.character(year)))

  } # end-if

  if(type == "barplot"){

    dat_ordenado <- dat %>% dplyr::arrange(desc(area)) %>%
      dplyr::mutate(names = factor(names, levels = names[order(area, decreasing = TRUE)]))


    p <- ggplot2::ggplot(dat_ordenado, ggplot2::aes(x = names, y = area, fill = names)) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values = dat_ordenado$cores,
                        limits = dat_ordenado$names,
                        name = "Land Uses") +
      ggplot2::labs(x = NULL, y = expression(paste("Area in ", km^2))) +
      ggplot2::theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())+
      ggplot2::ggtitle(paste("Year: ", as.character(year)))

  } # end-if

  plot(p)

} # end-function
