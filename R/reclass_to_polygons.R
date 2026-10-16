#' Vetoriza os rasters reclassificados em poligonos sf
#'
#' @param obj Lista de `biomastats_metrics`, com `rec_map`, `start`, `end`
#'   e `classes`. Execute as metricas com `export.raster = TRUE`.
#' @param years Anos inteiros desejados; NULL seleciona todos os anos.
#' @param dissolve Dissolver pixels adjacentes de mesma classe (default TRUE).
#' @param only_class Retornar somente a classe de interesse (default TRUE).
#' @param crs CRS de saida. NULL (padrao) herda o CRS de cada raster;
#'   um CRS explicito solicita reprojecao.
#' @param save_shp Salvar um shapefile por ano (default TRUE).
#' @param output_dir Pasta de saida, por padrao "./source_codes/shp".
#' @param file_prefix Prefixo dos arquivos, por padrao "reclass_polygons".
#' @return Lista nomeada por ano ou, para um unico ano, um objeto sf com
#'   value (0/1), area_ha, year, fragment_id, code, classe, class, color,
#'   collection, classes (codigos selecionados) e geometry. O atributo shp_paths
#'   contem os caminhos salvos. Anos sem pixels selecionados retornam sf vazio.
#' @details Codigos preservados pela reclassificacao de uma unica classe sao
#'   convertidos para 1. NA e excluido. Fragmentos conectados somente por um
#'   vertice permanecem separados (conectividade por aresta), diferentemente
#'   das metricas de paisagem com oito vizinhos. A area e calculada com sf e
#'   convertida explicitamente para hectares, inclusive em CRS com pes.
#'   No shapefile, fragment_id e abreviado para frag_id (limite de 10 caracteres).
#'   collection e class_dictionary sao herdados do resultado das metricas.
#'   Os atributos class_dictionary e metadata preservam identidades, paleta,
#'   selecao temporal, CRS e resolucao de origem, inclusive apos saveRDS/readRDS.
#'   O atributo boundary herda o limite da area de estudo quando disponivel nas metricas.
#'   Para uma classe, code, classe, class e color identificam os poligonos de
#'   presenca. Para varias classes fundidas, esses campos sao NA: os codigos
#'   selecionados ficam em classes e no dicionario, sem inventar identidade
#'   por fragmento. O fundo tambem nao recebe identidade de classe selecionada.
#'   Objetos antigos sem colecao/dicionario continuam aceitos, com identidades
#'   desconhecidas (NA); nenhuma colecao e presumida. A exportacao SHP conserva
#'   as colunas, mas os atributos completos do objeto requerem serializacao RDS.
#' @export
reclass_to_polygons <- function(obj, years = NULL, dissolve = TRUE,
                                only_class = TRUE, crs = NULL,
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
  target_crs <- if (is.null(crs)) NULL else sf::st_crs(crs)
  if (!is.null(target_crs) && is.na(target_crs)) stop("crs deve especificar um CRS valido.")
  collection <- obj$collection
  if (!is.null(collection)) collection <- .biomastats_collection(collection)
  dictionary <- obj$class_dictionary
  if (is.null(dictionary) && !is.null(collection)) {
    dictionary <- .biomastats_dictionary(collection, obj$classes)
  }
  if (!is.null(dictionary)) {
    fields <- c("code", "classe", "class", "color")
    if (!is.data.frame(dictionary) || !all(fields %in% names(dictionary)) ||
        anyDuplicated(dictionary$code) || any(!obj$classes %in% dictionary$code)) {
      stop("class_dictionary deve identificar todas as classes selecionadas.")
    }
    dictionary <- dictionary[match(sort(unique(obj$classes)), dictionary$code), , drop = FALSE]
  }
  source <- setNames(lapply(years, function(yr) {
    r <- obj$rec_map[[yr - obj$start + 1L]]
    if (!inherits(r, "RasterLayer")) stop("rec_map deve conter objetos RasterLayer.")
    if (is.na(sf::st_crs(r))) stop("Raster de entrada sem CRS.")
    list(crs_wkt = sf::st_crs(r)$wkt, resolution = raster::res(r))
  }), as.character(years))
  metadata <- list(schema_version = "1.0", collection = collection,
    classes = obj$classes, years = years, source = source,
    dissolve = dissolve, only_class = only_class, connectivity = 4L,
    area_unit = "ha", upstream = obj$metadata)
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
    year_crs <- if (is.null(target_crs)) sf::st_crs(r) else target_crs
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
                       geometry = sf::st_sfc(crs = year_crs)))
    }
    r <- raster::setValues(r, values)
    p <- .vectorize_raster(r, dissolve)
    names(p)[1L] <- "value"
    # Homogenize mixed POLYGON/MULTIPOLYGON before splitting fragments.
    p <- sf::st_cast(p, "MULTIPOLYGON", warn = FALSE)
    p <- sf::st_cast(p, "POLYGON", warn = FALSE)
    p <- sf::st_transform(p, year_crs)
    p$area_ha <- as.numeric(units::set_units(sf::st_area(p), "ha"))
    p$year <- rep(yr, nrow(p))
    p$fragment_id <- seq_len(nrow(p))
    p[, c("value", "area_ha", "year", "fragment_id", "geometry")]
  })
  names(result) <- as.character(years)
  result <- lapply(names(result), function(yr) {
    p <- result[[yr]]
    p$code <- rep(NA_real_, nrow(p))
    p$classe <- p$class <- p$color <- rep(NA_character_, nrow(p))
    p$collection <- rep(if (is.null(collection)) NA_real_ else collection, nrow(p))
    p$classes <- rep(NA_character_, nrow(p))
    p$classes[p$value == 1] <- paste(sort(unique(obj$classes)), collapse = ",")
    if (length(unique(obj$classes)) == 1L && !is.null(dictionary)) {
      presence <- p$value == 1
      for (field in c("code", "classe", "class", "color")) p[[field]][presence] <- dictionary[[field]][1L]
    }
    p <- p[, c("value", "area_ha", "year", "fragment_id", "code", "classe",
               "class", "color", "collection", "classes", "geometry")]
    attr(p, "class_dictionary") <- dictionary
    attr(p, "boundary") <- obj$boundary
    year_metadata <- metadata
    year_metadata$years <- as.numeric(yr)
    year_metadata$source <- source[yr]
    year_metadata$output_crs_wkt <- sf::st_crs(p)$wkt
    attr(p, "metadata") <- year_metadata
    p
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
  if (length(result) > 1L) {
    attr(out, "class_dictionary") <- dictionary
    attr(out, "boundary") <- obj$boundary
    attr(out, "metadata") <- metadata
  }
  attr(out, "shp_paths") <- shp_paths
  out
}

# Shared vectorization routine: cells with equal values become polygons
# (dissolved per value when dissolve = TRUE), in the raster's own CRS.
.vectorize_raster <- function(r, dissolve = TRUE) {
  sf::st_as_sf(terra::as.polygons(.as_terra(r), aggregate = dissolve, na.rm = TRUE))
}
