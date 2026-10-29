#' Catalogo de mapas rasterizados para aquisicao seletiva por HTTP
#'
#' Lista as colecoes anuais de uso e cobertura do solo que
#' `load_rasters(method = "http")` le diretamente de GeoTIFFs publicos, apenas na
#' janela da area de estudo: MapBiomas Brasil (30 m e 10 m) e as iniciativas
#' MapBiomas de outros paises e regioes. Os arquivos sao divididos em blocos
#' (256 ou 512 pixels), entao so os blocos da area sao transferidos.
#' @param id Identificadores a retornar; NULL lista todos.
#' @param initiative Iniciativa (por exemplo "brasil", "chile"); NULL lista todas.
#' @return data.frame com id, initiative, product, resolution_m, first_year,
#'   last_year, url (modelo com `{year}`), dictionary (codigo de [dict_build()]
#'   compativel com a legenda, ou NA), license e notes.
#' @details Os modelos seguem a organizacao do bucket publico do MapBiomas no
#'   Google Cloud Storage, acessivel sem login. Mudancas nesse endereco podem
#'   exigir atualizar o catalogo; qualquer outro GeoTIFF anual pode ser lido com
#'   `load_rasters(method = "http", url = )`. As colecoes do Brasil usam a legenda
#'   do MapBiomas (`dict_build(10)`); as demais iniciativas tem legendas proprias,
#'   que podem ser informadas com [class_dictionary()]. Os dados do MapBiomas sao
#'   distribuidos sob CC-BY-SA, com citacao obrigatoria.
#' @md
#' @examples
#' raster_catalog(initiative = "brasil")
#' @export
raster_catalog <- function(id = NULL, initiative = NULL) {
  out <- .raster_registry()
  if (!is.null(id)) {
    missing <- setdiff(id, out$id)
    if (length(missing)) stop("Produto ausente do catalogo: ", paste(missing, collapse = ", "),
                              ". Veja raster_catalog().")
    out <- out[match(id, out$id), , drop = FALSE]
  }
  if (!is.null(initiative)) out <- out[out$initiative %in% initiative, , drop = FALSE]
  rownames(out) <- NULL
  out
}

.raster_registry <- function() {
  root <- "https://storage.googleapis.com/mapbiomas-public/initiatives/"
  rows <- list(
    c("mapbiomas_brasil_c11", "brasil", "MapBiomas Brasil, Colecao 11", 30, 1985, 2025,
      "brasil/collection11/lulc/coverage/brazil_coverage/brazil_coverage-col11_{year}.tif", 10),
    c("mapbiomas_brasil_c10", "brasil", "MapBiomas Brasil, Colecao 10", 30, 1985, 2024,
      "brasil/collection_10/lulc/coverage/brazil_coverage_{year}.tif", 10),
    c("mapbiomas_brasil_c9", "brasil", "MapBiomas Brasil, Colecao 9", 30, 1985, 2023,
      "brasil/collection_9/lclu/coverage/brasil_coverage_{year}.tif", 10),
    c("mapbiomas_brasil_10m_c4", "brasil", "MapBiomas Brasil 10 m, Colecao 4 beta (Sentinel-2)",
      10, 2017, 2025,
      "brasil/lulc_10m/collection4/coverage/brazil_coverage/brazil_coverage-col4_10m_{year}.tif", 10),
    c("mapbiomas_argentina_c3", "argentina", "MapBiomas Argentina, Colecao 3", 30, 1985, 2025,
      "argentina/lulc/collection_3/coverage/coverage-argentina_classification_{year}.tif", NA),
    c("mapbiomas_bolivia_c4", "bolivia", "MapBiomas Bolivia, Colecao 4", 30, 1985, 2025,
      "bolivia/lulc/collection_04/coverage/coverage-bolivia_classification_{year}.tif", NA),
    c("mapbiomas_chile_c1", "chile", "MapBiomas Chile, Colecao 1", 30, 2000, 2022,
      "chile/coverage/chile_coverage_{year}.tif", NA),
    c("mapbiomas_colombia_c3", "colombia", "MapBiomas Colombia, Colecao 3", 30, 1985, 2024,
      "colombia/lulc/collection_03/coverage/coverage-colombia_classification_{year}.tif", NA),
    c("mapbiomas_ecuador_c4", "ecuador", "MapBiomas Ecuador, Colecao 4", 30, 1985, 2025,
      "ecuador/lulc/collection_04/coverage/coverage-ecuador_classification_{year}.tif", NA),
    c("mapbiomas_paraguay_c1", "paraguay", "MapBiomas Paraguay, Colecao 1", 30, 1985, 2022,
      "paraguay/collection_1/coverage/paraguay_coverage_{year}.tif", NA),
    c("mapbiomas_peru_c4", "peru", "MapBiomas Peru, Colecao 4", 30, 1985, 2025,
      "peru/collection4/lulc/coverage/peru_coverage/peru_coverage-col4_{year}.tif", NA),
    c("mapbiomas_uruguay_c1", "uruguay", "MapBiomas Uruguay, Colecao 1", 30, 1985, 2022,
      "uruguay/collection_1/coverage/uruguay_coverage_{year}.tif", NA),
    c("mapbiomas_venezuela_c3", "venezuela", "MapBiomas Venezuela, Colecao 3", 30, 1985, 2024,
      "venezuela/lulc/collection_03/coverage/coverage-venezuela_classification_{year}.tif", NA),
    c("mapbiomas_indonesia_c5", "indonesia", "MapBiomas Indonesia, Colecao 5", 30, 1990, 2025,
      "indonesia/lulc/collection_5/coverage/coverage-indonesia_classification_{year}.tif", NA),
    c("mapbiomas_amazonia_c5", "amazonia", "MapBiomas Amazonia (RAISG), Colecao 5", 30, 1985, 2022,
      "amazonia/collection_5/coverage/amazonia_coverage_{year}.tif", NA),
    c("mapbiomas_chaco_c4", "chaco", "MapBiomas Chaco, Colecao 4", 30, 1985, 2022,
      "chaco/collection_4/lclu/coverage/chaco_coverage_{year}.tif", NA),
    c("mapbiomas_pampa_c5", "pampa", "MapBiomas Pampa Trinacional, Colecao 5", 30, 1985, 2024,
      "pampa/collection5/lulc/coverage/pampa_coverage/pampa_coverage-col5_{year}.tif", NA),
    c("mapbiomas_bosque_atlantico_c3", "bosque_atlantico", "MapBiomas Bosque Atlantico, Colecao 3",
      30, 1985, 2022, "bosque_atlantico/collection_3/coverage/bosque_coverage_{year}.tif", NA))
  m <- do.call(rbind, rows)
  data.frame(id = m[, 1], initiative = m[, 2], product = m[, 3],
    resolution_m = as.numeric(m[, 4]), first_year = as.integer(m[, 5]),
    last_year = as.integer(m[, 6]), url = paste0(root, m[, 7]),
    dictionary = suppressWarnings(as.numeric(m[, 8])), license = "CC-BY-SA 4.0",
    notes = ifelse(grepl("10m", m[, 7]), "beta; serie Sentinel-2 mais curta", ""),
    stringsAsFactors = FALSE)
}
