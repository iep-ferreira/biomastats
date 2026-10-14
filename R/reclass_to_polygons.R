#' Vetoriza os rasters reclassificados em poligonos sf
#'
#' @param obj Lista de `biomastats_metrics`, com `rec_map`, `start`, `end`
#'   e `classes`. Execute as metricas com `export.raster = TRUE`.
#' @param years Anos inteiros desejados; NULL seleciona todos os anos.
#' @param dissolve Dissolver pixels adjacentes de mesma classe (default TRUE).
#' @param only_class Retornar somente a classe de interesse (default TRUE).
#' @param crs CRS de saida, por padrao EPSG 4326. O CRS de entrada e respeitado.
#' @param save_shp Salvar um shapefile por ano (default TRUE).
#' @param output_dir Pasta de saida, por padrao "./source_codes/shp".
#' @param file_prefix Prefixo dos arquivos, por padrao "reclass_polygons".
#' @return Lista nomeada por ano ou, para um unico ano, um objeto sf com
#'   value (0/1), area_ha, year, fragment_id e geometry. O atributo shp_paths
#'   contem os caminhos salvos. Anos sem pixels selecionados retornam sf vazio.
#' @details Codigos preservados pela reclassificacao de uma unica classe sao
#'   convertidos para 1. NA e excluido. Fragmentos conectados somente por um
#'   vertice permanecem separados (conectividade por aresta), diferentemente
#'   das metricas de paisagem com oito vizinhos. A area e calculada com sf e
#'   convertida explicitamente para hectares, inclusive em CRS com pes.
#'   No shapefile, fragment_id e abreviado para frag_id (limite de 10 caracteres).
#' @export
reclass_to_polygons <- function(obj, years = NULL, dissolve = TRUE,
                                only_class = TRUE, crs = 4326,
                                save_shp = TRUE, output_dir = "./source_codes/shp",
                                file_prefix = "reclass_polygons") {
  if (!is.list(obj) || !all(c("rec_map", "start", "end", "classes") %in% names(obj))) {
    stop("obj deve ser a saida de biomastats_metrics.")
  }
  integer_values <- function(x) {
    is.numeric(x) && length(x) > 0L && all(is.finite(x) & x == trunc(x))
  }
  if (!integer_values(obj$start) || length(obj$start) != 1L ||
      !integer_values(obj$end) || length(obj$end) != 1L || obj$start > obj$end) {
    stop("start e end devem definir um intervalo de anos inteiros valido.")
  }
  available <- seq.int(obj$start, obj$end)
  if (!is.list(obj$rec_map) || length(obj$rec_map) != length(available)) {
    stop("rec_map deve conter um raster por ano; use export.raster = TRUE.")
  }
  if (!integer_values(obj$classes) || any(obj$classes <= 0)) {
    stop("classes deve conter codigos positivos inteiros.")
  }
  if (is.null(years)) years <- available
  if (!integer_values(years)) stop("years deve conter anos inteiros, sem NA.")
  years <- sort(unique(years))
  if (any(!years %in% available)) stop("Anos fora do intervalo disponivel.")
  for (flag in list(dissolve, only_class, save_shp)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("dissolve, only_class e save_shp devem ser logicos escalares.")
    }
  }
  target_crs <- sf::st_crs(crs)
  if (is.na(target_crs)) stop("crs deve especificar um CRS valido.")
  if (save_shp && (!is.character(output_dir) || length(output_dir) != 1L ||
      is.na(output_dir) || !nzchar(output_dir) || !is.character(file_prefix) ||
      length(file_prefix) != 1L || is.na(file_prefix) ||
      !grepl("^[[:alnum:]_-]+$", file_prefix))) {
    stop("output_dir e file_prefix devem ser validos; prefixo sem separadores.")
  }
  result <- lapply(years, function(yr) {
    r <- obj$rec_map[[yr - obj$start + 1L]]
    if (!inherits(r, "RasterLayer")) stop("rec_map deve conter objetos RasterLayer.")
    if (is.na(sf::st_crs(r))) stop("Raster de entrada sem CRS.")
    values <- raster::getValues(r)
    allowed <- if (length(obj$classes) == 1L) c(0, 1, obj$classes) else c(0, 1)
    if (any(!is.na(values) & !values %in% allowed)) {
      stop("rec_map contem valores incompatíveis com a reclassificacao.")
    }
    values[!is.na(values) & values != 0] <- 1
    if (only_class) values[!is.na(values) & values == 0] <- NA
    if (all(is.na(values))) {
      return(sf::st_sf(value = numeric(), area_ha = numeric(), year = numeric(),
                       fragment_id = integer(),
                       geometry = sf::st_sfc(crs = target_crs)))
    }
    r <- raster::setValues(r, values)
    p <- terra::as.polygons(terra::rast(r), aggregate = dissolve, na.rm = TRUE)
    p <- sf::st_as_sf(p)
    names(p)[1L] <- "value"
    p <- sf::st_cast(p, "POLYGON", warn = FALSE)
    p <- sf::st_transform(p, target_crs)
    p$area_ha <- as.numeric(units::set_units(sf::st_area(p), "ha"))
    p$year <- rep(yr, nrow(p))
    p$fragment_id <- seq_len(nrow(p))
    p[, c("value", "area_ha", "year", "fragment_id", "geometry")]
  })
  names(result) <- as.character(years)
  shp_paths <- character()
  if (save_shp) {
    if (!dir.exists(output_dir) && !dir.create(output_dir, recursive = TRUE)) {
      stop("Nao foi possivel criar output_dir.")
    }
    shp_paths <- vapply(names(result), function(yr) {
      path <- file.path(output_dir, paste0(file_prefix, "_", yr, ".shp"))
      exported <- result[[yr]]
      names(exported)[names(exported) == "fragment_id"] <- "frag_id"
      sf::st_write(exported, path, delete_layer = TRUE, quiet = TRUE,
                   layer_options = "SHPT=POLYGON")
      path
    }, character(1))
  }
  out <- if (length(result) == 1L) result[[1L]] else result
  attr(out, "shp_paths") <- shp_paths
  out
}
