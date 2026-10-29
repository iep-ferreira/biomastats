#' Carrega mapas de uso do solo de uma pasta local no formato de load_rasters
#'
#' Le GeoTIFFs anuais de uma pasta de caminho conhecido, nomeados como
#' `<prefixo>_<ano>.tif` (ou `.tiff`), de qualquer colecao ou pais: avalia a
#' compatibilidade, harmoniza a grade, recorta e mascara pela area de estudo e
#' devolve o mesmo objeto de mapas de [load_rasters()], pronto para
#' [get_area()], [biomastats_metrics()], [fragment_metrics()], [land_change()],
#' [connectivity_context()] e as demais analises. Nada e gravado em disco.
#' @param shape_path Area de estudo: objeto sf/sfc poligonal ou arquivo vetorial
#'   (GeoPackage, shapefile ou GeoJSON), com ou sem extensao; com o mesmo nome,
#'   `.gpkg` tem preferencia sobre `.shp`. Um shapefile precisa do `.prj`. NULL
#'   procura um unico arquivo de area na propria pasta dos mapas.
#' @param folder Pasta com os GeoTIFFs.
#' @param prefix Prefixo dos arquivos (a parte antes de `_<ano>`). NULL aceita o
#'   unico prefixo da pasta; com varios, e preciso escolher.
#' @param years Anos consecutivos a carregar; NULL usa todos os disponiveis (que
#'   devem ser consecutivos, como em load_rasters).
#' @param collection Nome ou codigo da colecao. Um codigo conhecido por
#'   [dict_build()] (por exemplo 10, para a legenda do MapBiomas) dispensa
#'   dicionario; NULL usa o prefixo.
#' @param dictionary Dicionario opcional das classes: data.frame ou caminho de
#'   CSV, conferido por [class_dictionary()]. Sem ele, valem o de [dict_build()]
#'   (colecoes conhecidas) ou nomes genericos ("Classe 3").
#' @param nodata Codigos tratados como sem dado (NA); padrao 0, como no
#'   MapBiomas e no ESA WorldCover. NULL mantem todos.
#' @param crs,resolution Grade de referencia: CRS (codigo EPSG, WKT ou objeto
#'   crs) e resolucao, na unidade desse CRS. NULL usa a grade do primeiro ano.
#' @param context_m Buffer em metros ao redor da area de estudo (padrao 0), como
#'   em [load_rasters()]: com valor positivo, o recorte cobre o contexto e a area
#'   original fica em `focal`.
#' @return Lista no formato de [load_rasters()]: shape (sf em EPSG:4326; o
#'   contexto quando context_m > 0), time_range, raster (RasterLayer em memoria
#'   por ano, todos na mesma grade), collection, class_dictionary (ou NULL),
#'   metadata (arquivos, grade de referencia, harmonizacao e cobertura por ano),
#'   focal (com contexto) e source (origem, usada por [connectivity_context()]
#'   para recarregar uma area maior).
#' @details Compatibilidade: cada arquivo deve ter uma camada, CRS e codigos
#'   inteiros, e cobrir a area de estudo (ao menos 99%; um recorte parcial, como
#'   mapas baixados para outra area, e recusado). Harmonizacao: cada ano e
#'   recortado pela area (so a janela e lida) e, se sua grade (CRS, resolucao,
#'   origem ou extensao) diferir da de referencia, reprojetado ou reamostrado pelo
#'   vizinho mais proximo, que preserva os codigos de classe; a acao de cada ano
#'   consta em `metadata$harmonization`. A mascara usa o centro das celulas, como
#'   `raster::mask`. A mesma rotina atende `load_rasters(method = "http")`, cujas
#'   janelas gravadas em `export_folder_path` podem ser relidas aqui.
#' @md
#' @examples
#' \dontrun{
#' # Pasta com mapbiomas10m_2017.tif ... mapbiomas10m_2025.tif e a area
#' mapas <- load_local_rasters(folder = "~/Documents/mapbiomas_10m_col4", collection = 10)
#' get_area(mapas)$time_series
#' # Outra colecao, com dicionario proprio
#' dic <- class_dictionary(file = "legenda.csv")
#' mapas <- load_local_rasters("area.shp", folder = "mapas_chile", dictionary = dic)
#' }
#' @export
load_local_rasters <- function(shape_path = NULL, folder, prefix = NULL, years = NULL,
                               collection = NULL, dictionary = NULL, nodata = 0,
                               crs = NULL, resolution = NULL, context_m = 0) {
  if (!is.character(folder) || length(folder) != 1L || !dir.exists(folder)) {
    stop("folder deve ser uma pasta existente.")
  }
  .local_check_context(context_m)
  files <- .local_raster_files(folder, prefix)
  prefix <- files$prefix[1]
  if (is.null(years)) years <- files$year
  years <- .local_years(years)
  absent <- setdiff(years, files$year)
  if (length(absent)) stop("Anos sem arquivo ", prefix, "_<ano>.tif: ", paste(absent, collapse = ", "))
  area <- .read_study_area(if (is.null(shape_path)) folder else shape_path)
  focal <- NULL
  if (context_m > 0) {
    focal <- area
    area <- .context_buffer(area, context_m)
  }
  paths <- files$path[match(years, files$year)]
  core <- .load_raster_paths(paths, years, area, prefix, collection, dictionary, nodata,
                             crs, resolution, required = if (is.null(focal)) area else focal)
  folder <- normalizePath(folder, winslash = "/")
  core$maps$metadata <- c(list(source = "local GeoTIFF folder", folder = folder, prefix = prefix),
                          core$maps$metadata)
  .local_finish(core$maps, focal, list(loader = "load_local_rasters", folder = folder,
    prefix = prefix, collection = core$maps$collection, dictionary = core$maps$class_dictionary,
    nodata = nodata, crs = crs, resolution = resolution, context_m = context_m))
}

# Shared core of load_local_rasters() and load_rasters(method = "http"): reads
# one raster per year (local paths or GDAL virtual paths such as /vsicurl/),
# checks it, crops the window of the area, harmonizes it to the reference grid,
# masks it and returns a maps object (without focal and source).
.load_raster_paths <- function(paths, years, area, prefix, collection, dictionary, nodata,
                               crs, resolution, required = area) {
  if (!is.null(nodata) && (!is.numeric(nodata) || anyNA(nodata))) stop("nodata deve ser numerico ou NULL.")
  label <- function(p) sub("^/vsicurl/", "", p)
  template <- .local_template(terra::rast(paths[1]), area, crs, resolution, label(paths[1]),
                              required)
  zone_t <- terra::vect(sf::st_transform(area, sf::st_crs(terra::crs(template))))
  harmonization <- vector("list", length(years))
  rasters <- vector("list", length(years))
  for (i in seq_along(years)) {
    name <- basename(label(paths[i]))
    r <- terra::rast(paths[i])
    if (terra::nlyr(r) != 1L) stop(name, ": o arquivo deve ter uma unica camada.")
    if (!nzchar(terra::crs(r))) stop(name, ": raster sem CRS.")
    w <- terra::crop(r, .local_zone(r, area, label(paths[i]), required), snap = "out")
    same_crs <- isTRUE(sf::st_crs(terra::crs(w)) == sf::st_crs(terra::crs(template)))
    aligned <- same_crs && isTRUE(terra::compareGeom(w, template, crs = FALSE, ext = TRUE,
      rowcol = TRUE, res = TRUE, stopOnError = FALSE, messages = FALSE))
    action <- if (aligned) "none" else if (same_crs) "resampled (nearest)" else "reprojected (nearest)"
    if (!aligned) w <- terra::project(w, template, method = "near")
    w <- terra::mask(w, zone_t)
    v <- terra::values(w, mat = FALSE)
    if (!is.null(nodata)) v[v %in% nodata] <- NA
    if (any(!is.na(v) & (!is.finite(v) | v != trunc(v)))) {
      stop(name, ": as classes devem ser codigos inteiros.")
    }
    if (all(is.na(v))) stop(name, ": nenhum pixel valido na area de estudo.")
    terra::values(w) <- v
    rasters[[i]] <- .to_rasterlayer(w, paste0(prefix, "_", years[i]))
    harmonization[[i]] <- data.frame(year = years[i], file = name,
      source_crs = sf::st_crs(terra::crs(r))$input %||% NA_character_,
      source_resolution = paste(signif(terra::res(r), 6), collapse = " x "), action = action,
      valid_cells = sum(!is.na(v)), codes = paste(sort(unique(v[!is.na(v)])), collapse = ","))
  }
  if (is.null(collection)) collection <- prefix
  collection <- .biomastats_collection(collection)
  codes <- sort(unique(unlist(lapply(rasters, function(r) stats::na.omit(raster::getValues(r))))))
  if (!is.null(dictionary)) {
    dictionary <- class_dictionary(codes = codes, file = dictionary)
  } else if (is.null(.collection_dictionary(collection))) {
    message("Colecao '", collection, "' sem dicionario: as classes terao nomes genericos. ",
            "Use class_dictionary() para nomes e cores.")
  }
  list(maps = list(shape = sf::st_sf(geometry = sf::st_geometry(area)),
    time_range = range(years), raster = rasters, collection = collection,
    class_dictionary = dictionary,
    metadata = list(files = vapply(paths, function(p) basename(label(p)), ""),
      reference_crs = terra::crs(template), reference_resolution = terra::res(template),
      nodata = nodata, harmonization = do.call(rbind, harmonization),
      method = paste("window crop, nearest-neighbour harmonization to the reference grid,",
                     "mask by cell centre"))))
}

.local_finish <- function(maps, focal, source) {
  if (!is.null(focal)) maps$focal <- focal
  maps$source <- source
  maps
}

.local_check_context <- function(context_m) {
  if (!is.numeric(context_m) || length(context_m) != 1L || !is.finite(context_m) || context_m < 0) {
    stop("context_m deve ser uma distancia nao negativa em metros.")
  }
}

.local_years <- function(years) {
  if (!is.numeric(years) || !length(years) || anyNA(years) || any(years != trunc(years))) {
    stop("years deve conter anos inteiros.")
  }
  years <- sort(unique(years))
  if (length(years) > 1L && any(diff(years) != 1)) {
    stop("Os anos devem ser consecutivos, como em load_rasters(); selecione years.")
  }
  years
}

# GeoTIFFs named <prefix>_<year>.tif(f) of one prefix.
.local_raster_files <- function(folder, prefix = NULL) {
  f <- list.files(folder, pattern = "^(.+)_([0-9]{4})\\.tiff?$", ignore.case = TRUE,
                  full.names = TRUE)
  if (!length(f)) stop("Nenhum GeoTIFF <prefixo>_<ano>.tif em ", folder, ".")
  base <- basename(f)
  out <- data.frame(path = f, prefix = sub("_([0-9]{4})\\.tiff?$", "", base, ignore.case = TRUE),
                    year = as.integer(sub("^.*_([0-9]{4})\\.tiff?$", "\\1", base, ignore.case = TRUE)))
  if (is.null(prefix)) {
    found <- unique(out$prefix)
    if (length(found) > 1L) stop("Varios prefixos na pasta (", paste(found, collapse = ", "),
                                 "); escolha prefix.")
    prefix <- found
  }
  out <- out[out$prefix == prefix, , drop = FALSE]
  if (!nrow(out)) stop("Nenhum arquivo ", prefix, "_<ano>.tif em ", folder, ".")
  if (anyDuplicated(out$year)) stop("Ano repetido para ", prefix, " (.tif e .tiff?).")
  out[order(out$year), , drop = FALSE]
}

# Reference grid over the study area: the first year's grid, optionally in
# another CRS and/or resolution.
.local_template <- function(r, area, crs, resolution, path, required = area) {
  target <- if (is.null(crs)) sf::st_crs(terra::crs(r)) else sf::st_crs(crs)
  if (is.na(target)) stop("crs deve ser valido.")
  if (!is.null(resolution) && (!is.numeric(resolution) || !length(resolution) %in% 1:2 ||
                               any(!is.finite(resolution) | resolution <= 0))) {
    stop("resolution deve ser positiva (uma ou duas dimensoes).")
  }
  w <- terra::crop(r, .local_zone(r, area, path, required), snap = "out")
  if (!isTRUE(target == sf::st_crs(terra::crs(r)))) {
    w <- terra::project(w, target$wkt, method = "near")
  }
  if (!is.null(resolution)) {
    # Extent snapped outwards to whole cells of the requested size.
    res <- rep_len(resolution, 2L)
    e <- terra::ext(w)
    e <- terra::ext(floor(e$xmin[[1]] / res[1]) * res[1], ceiling(e$xmax[[1]] / res[1]) * res[1],
                    floor(e$ymin[[1]] / res[2]) * res[2], ceiling(e$ymax[[1]] / res[2]) * res[2])
    w <- terra::rast(e, resolution = res, crs = terra::crs(w))
  }
  terra::rast(w)
}

# Area in the CRS of a raster, after checking that the raster extent covers
# the required area (the study area, not its context buffer) by at least 99%:
# maps cut for another area are refused. The 1% margin absorbs border slivers
# of reprojected years; a context may run past the edge of the data.
.local_zone <- function(r, area, path, required = area) {
  crs <- sf::st_crs(terra::crs(r))
  zone_sf <- sf::st_transform(area, crs)
  # Raster extent with densified edges, so that in degrees they follow the
  # parallels and meridians rather than great circles.
  e <- as.vector(terra::ext(r))
  side <- function(a, b) seq(a, b, length.out = 65)
  ring <- rbind(cbind(side(e[1], e[2]), e[3]), cbind(e[2], side(e[3], e[4])),
                cbind(side(e[2], e[1]), e[4]), cbind(e[1], side(e[4], e[3])))
  box <- sf::st_sfc(sf::st_polygon(list(unique(ring)[c(seq_len(nrow(unique(ring))), 1), ])), crs = crs)
  g <- sf::st_union(sf::st_geometry(sf::st_transform(required, crs)))
  inside <- suppressMessages(suppressWarnings(sf::st_intersection(g, box)))
  covered <- if (length(inside)) as.numeric(sum(sf::st_area(inside)) / sf::st_area(g)) else 0
  if (covered < 0.99) {
    stop(basename(path), if (covered <= 0) ": o raster nao cobre a area de estudo." else
         sprintf(": o raster cobre apenas %.1f%% da area de estudo.", 100 * covered))
  }
  terra::vect(zone_sf)
}
