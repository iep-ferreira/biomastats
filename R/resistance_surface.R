#' Superficie de resistencia a partir do uso do solo, com barreiras e custos
#'
#' Converte o mapa de uso e cobertura de um ano em uma superficie de custo de
#' deslocamento (friccao) para [cost_distance()] e para o Makurhini
#' (`distancefile(type = "least-cost")`, via [as_makurhini()]). Cada classe recebe
#' um custo por unidade de distancia; barreiras vetoriais (estradas, represas,
#' areas urbanas) elevam o custo das celulas que tocam ou as tornam
#' intransponiveis. Nao grava arquivos.
#' @param data Objeto de [load_rasters()] (raster, time_range e collection),
#'   um RasterLayer ou um SpatRaster de uma camada com codigos de classe.
#' @param year Ano a usar quando data vem de load_rasters; NULL aceita apenas
#'   uma serie de um ano.
#' @param resistance Custo por classe: data.frame com colunas code e cost, ou
#'   vetor numerico nomeado pelos codigos. Custos devem ser positivos; Inf ou NA
#'   tornam a classe uma barreira. O custo 1 equivale a distancia euclidiana.
#' @param barriers Objeto sf opcional de linhas ou poligonos. Uma coluna cost,
#'   quando presente, da o custo de cada feicao; senao vale barrier_cost.
#' @param barrier_cost Custo padrao das barreiras (Inf, intransponivel, por
#'   padrao). Uma celula tocada recebe o maior entre seu custo e o da barreira.
#' @param default Custo de codigos ausentes de resistance (NA, barreira, por
#'   padrao).
#' @param crs CRS projetado em metros para a superficie. NULL conserva um CRS
#'   projetado ou escolhe UTM local para mapas em graus (reamostragem pelo
#'   vizinho mais proximo dos codigos, antes da conversao em custos).
#' @param language Idioma dos rotulos: pt (padrao) ou en.
#' @param build_plot Construir o mapa ggplot (TRUE por padrao).
#' @return Lista biomastats_resistance com raster (RasterLayer em memoria, NA nas
#'   barreiras intransponiveis), classes (codigo, classe, custo, celulas e area em
#'   ha), barriers (sf usado, ou NULL), metadata e plot.
#' @details O custo de atravessar uma celula e proporcional ao custo da classe e a
#'   distancia percorrida nela, como em gdistance e no Makurhini. Barreiras sao
#'   rasterizadas com todas as celulas que tocam (touches = TRUE), o que evita
#'   frestas em linhas finas. O RasterLayer e serializavel com saveRDS e e aceito
#'   diretamente por Makurhini e gdistance.
#' @references Adriaensen, F. et al. (2003) Landscape and Urban Planning 64:
#'   233-247. Godinez-Gomez, O. & Correa Ayram, C. A. (2020) Makurhini,
#'   <https://github.com/connectscape/Makurhini>.
#' @md
#' @examples
#' \dontrun{
#' mapas <- load_rasters(...)
#' custos <- data.frame(code = c(3, 4, 12, 15, 21, 24, 33),
#'                      cost = c(1, 2, 3, 20, 10, Inf, 50))
#' r <- resistance_surface(mapas, year = 2024, resistance = custos,
#'   barriers = estradas, barrier_cost = 100)
#' r$plot
#' }
#' @export
resistance_surface <- function(data, year = NULL, resistance, barriers = NULL,
                               barrier_cost = Inf, default = NA_real_, crs = NULL,
                               language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  .connectivity_check_flag(build_plot, "build_plot")
  if (!is.numeric(barrier_cost) || length(barrier_cost) != 1L || is.na(barrier_cost) ||
      barrier_cost <= 0) {
    stop("barrier_cost deve ser um custo positivo ou Inf.")
  }
  if (length(default) != 1L || (!is.na(default) && (!is.numeric(default) || default <= 0))) {
    stop("default deve ser um custo positivo, Inf ou NA.")
  }
  costs <- .resistance_table(resistance)
  layer <- .resistance_layer(data, year)
  collection <- if (is.list(data) && !inherits(data, c("Raster", "SpatRaster"))) data$collection
  r <- if (inherits(layer, "SpatRaster")) layer else terra::rast(layer)
  if (terra::nlyr(r) != 1L) stop("data deve ter uma unica camada.")
  if (is.na(sf::st_crs(terra::crs(r)))) stop("Raster sem CRS.")
  target <- .resistance_crs(r, crs)
  if (!isTRUE(sf::st_crs(terra::crs(r)) == target)) {
    r <- terra::project(r, target$wkt, method = "near")
  }
  codes <- terra::values(r, mat = FALSE)
  if (any(!is.na(codes) & codes != trunc(codes))) stop("O mapa deve conter codigos inteiros.")
  cost <- costs$cost[match(codes, costs$code)]
  cost[!is.na(codes) & !codes %in% costs$code] <- default
  cost[!is.finite(cost)] <- NA_real_
  friction <- terra::rast(r)
  terra::values(friction) <- cost
  used <- NULL
  if (!is.null(barriers)) {
    if (!inherits(barriers, "sf") || is.na(sf::st_crs(barriers))) stop("barriers deve ser sf com CRS.")
    used <- sf::st_transform(barriers, target)
    used$cost <- if ("cost" %in% names(used)) as.numeric(used$cost) else barrier_cost
    if (any(is.na(used$cost) | used$cost <= 0)) stop("Custos das barreiras devem ser positivos ou Inf.")
    # Impassable features become NA; finite ones keep the larger cost.
    burn <- terra::rasterize(terra::vect(used), friction, field = "cost", fun = "max",
                             touches = TRUE)
    b <- terra::values(burn, mat = FALSE)
    hit <- !is.na(b)
    cost[hit & is.infinite(b)] <- NA_real_
    finite_hit <- hit & is.finite(b) & !is.na(cost)
    cost[finite_hit] <- pmax(cost[finite_hit], b[finite_hit])
    terra::values(friction) <- cost
  }
  names(friction) <- "resistance"
  out_raster <- raster::setValues(raster::raster(friction), cost)
  cell_ha <- prod(terra::res(friction)) / 10000
  present <- sort(unique(stats::na.omit(codes)))
  dictionary <- if (!is.null(collection)) .biomastats_dictionary(collection, present)
  classes <- data.frame(code = present,
    class = if (is.null(dictionary)) as.character(present)
      else .tr(language, dictionary$classe, dictionary$class),
    cost = vapply(present, function(code) {
      i <- match(code, costs$code)
      if (is.na(i)) default else costs$cost[i]
    }, numeric(1)),
    cells = as.integer(tabulate(match(codes, present), length(present))))
  classes$area_ha <- classes$cells * cell_ha
  metadata <- list(schema_version = "1.0",
    package_version = as.character(utils::packageVersion("biomastats")),
    collection = collection, year = year, crs_wkt = target$wkt,
    resolution = terra::res(friction), cell_area_ha = cell_ha, default_cost = default,
    barrier_cost = barrier_cost, barriers = if (is.null(used)) 0L else nrow(used),
    rule = paste("cost per unit distance; NA = impassable; barriers rasterized with",
      "touches = TRUE taking the larger cost"))
  plot <- if (build_plot) .resistance_plot(friction, language)
  structure(list(raster = out_raster, classes = classes, barriers = used,
    metadata = metadata, plot = plot), class = c("biomastats_resistance", "list"))
}

.resistance_table <- function(resistance) {
  if (is.data.frame(resistance)) {
    if (!all(c("code", "cost") %in% names(resistance))) {
      stop("resistance deve ter colunas code e cost.")
    }
    out <- data.frame(code = as.numeric(resistance$code), cost = as.numeric(resistance$cost))
  } else if (is.numeric(resistance) && !is.null(names(resistance))) {
    out <- data.frame(code = as.numeric(names(resistance)), cost = unname(resistance))
  } else {
    stop("resistance deve ser data.frame (code, cost) ou vetor nomeado por codigo.")
  }
  if (!nrow(out) || anyNA(out$code) || anyDuplicated(out$code)) {
    stop("Codigos de resistance devem ser unicos e numericos.")
  }
  if (any(!is.na(out$cost) & out$cost <= 0)) stop("Custos devem ser positivos, Inf ou NA.")
  out
}

.resistance_layer <- function(data, year) {
  if (inherits(data, c("RasterLayer", "SpatRaster"))) return(data)
  if (!is.list(data) || !all(c("raster", "time_range") %in% names(data))) {
    stop("data deve ser objeto de load_rasters, RasterLayer ou SpatRaster.")
  }
  tr <- data$time_range
  if (is.null(year)) {
    if (length(data$raster) != 1L) stop("Selecione year para uma serie com varios anos.")
    return(data$raster[[1]])
  }
  if (!is.numeric(year) || length(year) != 1L || year < tr[1] || year > tr[2]) {
    stop("year indisponivel em data.")
  }
  data$raster[[year - tr[1] + 1L]]
}

.resistance_crs <- function(r, crs) {
  if (!is.null(crs)) {
    target <- sf::st_crs(crs)
    if (is.na(target) || sf::st_is_longlat(target) || !.connectivity_metres(target)) {
      stop("crs deve ser projetado em metros.")
    }
    return(target)
  }
  e <- terra::ext(r)
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = e$xmin[[1]], ymin = e$ymin[[1]], xmax = e$xmax[[1]],
    ymax = e$ymax[[1]]), crs = sf::st_crs(terra::crs(r))))
  .connectivity_crs(sf::st_sf(geometry = box), NULL)
}

.resistance_plot <- function(friction, language) {
  df <- as.data.frame(friction, xy = TRUE, na.rm = FALSE)
  names(df)[3] <- "cost"
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = cost)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c(option = "magma", direction = -1, trans = "log10",
      na.value = "#000000", name = .tr(language, "Custo", "Cost")) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL, caption = .tr(language,
      "Preto: barreira intranspon\u00edvel", "Black: impassable barrier")) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables("cost")
