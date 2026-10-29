#' Assistente do dicionario de classes (codigos, nomes e cores)
#'
#' Monta e confere o dicionario de uma colecao de mapas no formato de
#' [dict_build()], para colecoes locais ([load_local_rasters()]) ou para
#' renomear e recolorir classes. Sem arquivo, devolve um modelo com os codigos
#' encontrados nos mapas, para completar e reler; com arquivo, padroniza as
#' colunas, completa cores e nomes ausentes e avisa sobre codigos sem entrada.
#' @param x Mapas (formato de [load_rasters()]), RasterLayer, SpatRaster ou lista
#'   de rasters, dos quais extrair os codigos presentes. Opcional.
#' @param file Dicionario: data.frame ou caminho de CSV (separador `,`, `;` ou
#'   tabulacao). Colunas reconhecidas, sem diferenca de maiusculas e acentos:
#'   codigo (code, codigo, value, valor, id), nome em portugues (classe, nome),
#'   nome em ingles (class, name, label), grupo (grupo), group (group) e cor
#'   (color, cor, colour, hex). So a coluna de codigo e obrigatoria; um unico
#'   nome vale para os dois idiomas.
#' @param codes Codigos adicionais a incluir.
#' @param collection Colecao conhecida por [dict_build()] usada como base dos
#'   nomes e cores quando file nao e informado (por exemplo 10, do MapBiomas).
#' @param palette Paleta de [grDevices::hcl.colors()] para cores ausentes.
#' @return data.frame no formato de [dict_build()] (grupo, group, classe, class,
#'   code, color), ordenado por codigo, com cores em hexadecimal. Pode ser passado
#'   a `load_local_rasters(dictionary = )` ou gravado com `write.csv()` para
#'   edicao.
#' @details Codigos dos mapas ausentes do arquivo entram com nome generico
#'   ("Classe 7") e cinza, com aviso; codigos do arquivo ausentes dos mapas sao
#'   mantidos. Codigos devem ser inteiros e unicos; cores devem ser reconhecidas
#'   por [grDevices::col2rgb()].
#' @md
#' @examples
#' \dontrun{
#' modelo <- class_dictionary(mapas)           # codigos presentes, para preencher
#' write.csv(modelo, "legenda.csv", row.names = FALSE)
#' dic <- class_dictionary(mapas, file = "legenda.csv")
#' mapas <- load_local_rasters(area, folder = "mapas", dictionary = dic)
#' }
#' @export
class_dictionary <- function(x = NULL, file = NULL, codes = NULL, collection = NULL,
                             palette = "Dynamic") {
  present <- sort(unique(c(.dictionary_codes(x), codes)))
  if (!is.null(present) && (any(!is.finite(present)) || any(present != trunc(present)))) {
    stop("Os codigos de classe devem ser inteiros.")
  }
  if (is.null(file)) {
    base <- .collection_dictionary(collection)
    if (!length(present)) {
      if (is.null(base)) stop("Informe x, codes, file ou uma collection conhecida.")
      present <- sort(as.numeric(base$code))
    }
    dic <- .biomastats_dictionary(collection, present, base)
    out <- data.frame(grupo = dic$grupo, group = dic$group, classe = dic$classe, class = dic$class,
                      code = dic$code, color = dic$color)
    missing_color <- !dic$known
    if (any(missing_color)) out$color[missing_color] <- grDevices::hcl.colors(sum(missing_color), palette)
    return(out)
  }
  table <- .dictionary_read(file)
  cols <- .dictionary_columns(names(table))
  if (is.na(cols[["code"]])) stop("O dicionario precisa de uma coluna de codigo (code, codigo, value).")
  code <- suppressWarnings(as.numeric(table[[cols[["code"]]]]))
  if (anyNA(code) || any(code != trunc(code))) stop("Codigos do dicionario devem ser inteiros.")
  if (anyDuplicated(code)) stop("Codigos repetidos no dicionario: ",
                                paste(unique(code[duplicated(code)]), collapse = ", "))
  # Column `field` with empty cells filled from `fallback`.
  pick <- function(field, fallback) {
    v <- if (is.na(cols[[field]])) fallback else trimws(as.character(table[[cols[[field]]]]))
    empty <- is.na(v) | !nzchar(v)
    v[empty] <- fallback[empty]
    v
  }
  # One name serves both languages; without names, generic ones.
  classe <- pick("classe", pick("class", paste("Classe", code)))
  class <- pick("class", pick("classe", paste("Class", code)))
  color <- if (is.na(cols[["color"]])) rep(NA_character_, length(code))
    else as.character(table[[cols[["color"]]]])
  empty <- is.na(color) | !nzchar(trimws(color))
  if (any(empty)) color[empty] <- grDevices::hcl.colors(sum(empty), palette)
  rgb <- tryCatch(grDevices::col2rgb(trimws(color)), error = function(e) NULL)
  if (is.null(rgb)) stop("Cores invalidas no dicionario; use nomes do R ou hexadecimal (#RRGGBB).")
  out <- data.frame(grupo = pick("grupo", rep(NA_character_, length(code))),
    group = pick("group", rep(NA_character_, length(code))),
    classe = classe, class = class, code = code,
    color = grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], maxColorValue = 255))
  extra <- setdiff(present, code)
  if (length(extra)) {
    warning("Codigos dos mapas sem entrada no dicionario: ", paste(extra, collapse = ", "),
            "; receberam nomes genericos.", call. = FALSE)
    out <- rbind(out, data.frame(grupo = NA_character_, group = NA_character_,
      classe = paste("Classe", extra), class = paste("Class", extra), code = extra,
      color = "#999999"))
  }
  out <- out[order(out$code), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Codes present in maps, rasters or lists of rasters.
.dictionary_codes <- function(x) {
  if (is.null(x)) return(NULL)
  layers <- if (inherits(x, c("RasterLayer", "SpatRaster"))) list(x)
    else if (is.list(x) && !is.null(x$raster)) x$raster
    else if (is.list(x)) x
    else stop("x deve ser mapas, raster ou lista de rasters.")
  sort(unique(unlist(lapply(layers, function(r) {
    v <- if (inherits(r, "SpatRaster")) terra::values(r, mat = FALSE) else raster::getValues(r)
    unique(v[!is.na(v)])
  }))))
}

.dictionary_read <- function(file) {
  if (is.data.frame(file)) return(file)
  if (!is.character(file) || length(file) != 1L || !file.exists(file)) {
    stop("file deve ser um data.frame ou o caminho de um CSV existente.")
  }
  first <- readLines(file, n = 1L, warn = FALSE, encoding = "UTF-8")
  sep <- if (grepl("\t", first)) "\t" else if (grepl(";", first)) ";" else ","
  utils::read.csv(file, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
                  fileEncoding = "UTF-8")
}

# Column of each field, matched by alias (case and accents ignored).
.dictionary_columns <- function(names) {
  key <- tolower(iconv(names, to = "ASCII//TRANSLIT"))
  key <- gsub("[^a-z_]", "", key)
  aliases <- list(code = c("code", "codigo", "value", "valor", "id", "cod"),
    classe = c("classe", "nome", "name_pt", "nome_pt"),
    class = c("class", "name", "label", "name_en", "nome_en"),
    grupo = c("grupo", "grupo_pt"), group = c("group", "group_en"),
    color = c("color", "cor", "colour", "hex", "cores"))
  vapply(aliases, function(a) {
    i <- match(a, key)
    i <- i[!is.na(i)]
    if (length(i)) names[i[1]] else NA_character_
  }, character(1))
}
