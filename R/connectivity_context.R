#' Sensibilidade da conectividade ao tamanho do contexto espacial
#'
#' Amplia automaticamente a area de estudo com faixas (buffers) de varios raios,
#' recarrega, reclassifica e vetoriza o territorio maior uma unica vez e mede
#' como os indices de conectividade e a importancia dos fragmentos da area
#' focal mudam com o tamanho do contexto. O contexto entra como trampolim (ver
#' `focal` em [land_connectivity()]); a matriz de distancias do maior contexto e
#' reaproveitada nos menores. E um fluxo opcional: sem ele, nada muda nas
#' demais funcoes.
#' @param x Mapas da area de estudo de [load_rasters()] (MapBiomas), de
#'   [load_local_rasters()] (GeoTIFFs locais de qualquer colecao), de
#'   [load_lulc()] (ESA WorldCover, em qualquer pais) ou de outra fonte no mesmo
#'   formato (o contexto e carregado aqui), ou poligonos de
#'   [reclass_to_polygons()] que ja cobrem o maior contexto, com `focal`
#'   informado ou herdado.
#' @param classes Codigos das classes de habitat (obrigatorio quando x sao
#'   mapas), como em [biomastats_metrics()].
#' @param context Raios do contexto em metros. 0 e a analise sem contexto (o
#'   habitat recortado pela area de estudo, como hoje). NULL usa
#'   `c(0, 1, 2, 4) * max(max_dist)`.
#' @param max_dist Limiares de distancia, como em [land_connectivity()].
#' @param years Anos a analisar; NULL usa o ano mais recente de x.
#' @param metric_patch Indices: IIC, PC e/ou AWF (padrao IIC e PC). A tabela de
#'   sensibilidade usa IIC e PC; a importancia, todos os pedidos.
#' @param focal Area focal sf/sfc; NULL usa `x$focal` ou `x$shape` (mapas) ou o
#'   atributo focal (poligonos).
#' @param source Origem do contexto: mapas maiores ja disponiveis (lista no
#'   formato de [load_rasters()], RasterLayer ou SpatRaster de um ano), dos
#'   quais ele e recortado sem novo download, ou uma funcao `function(area,
#'   years)` que devolve mapas nesse formato cobrindo `area` (sf do contexto em
#'   WGS84), para qualquer colecao. NULL usa `x$source`, registrado por
#'   [load_rasters()], [load_lulc()] ou [load_local_rasters()], com a mesma origem.
#' @param targets Fragmentos-alvo para a remocao conjunta: vetor de
#'   fragment_id do maior contexto (um conjunto), lista nomeada desses vetores
#'   (varios conjuntos) ou sf/sfc de geometrias (por exemplo, fragmentos da
#'   vetorizacao original ou uma area de interesse), agrupadas pela coluna target
#'   quando existir (senao, cada linha e um alvo). NULL desliga.
#' @param resistance Custos por classe (data.frame code/cost ou vetor nomeado,
#'   aplicados por [resistance_surface()] aos mapas do contexto) ou uma
#'   superficie pronta; NULL usa distancia entre bordas. A distancia de menor
#'   custo e calculada uma vez no maior contexto.
#' @param directions Vizinhanca da vetorizacao: 8 (padrao) ou 4.
#' @param k,p_ref,d_ref,parallel,prune Como em [land_connectivity()].
#' @param tolerance Variacao maxima da ECA (%) entre os dois maiores raios para
#'   considerar a curva estabilizada (padrao 1).
#' @param keep_results Guardar os objetos de land_connectivity de cada raio
#'   (TRUE por padrao).
#' @param language Idioma dos rotulos: pt (padrao) ou en.
#' @param build_plot Construir os ggplots (TRUE por padrao).
#' @return Lista biomastats_connectivity_context com sensitivity (por ano, raio,
#'   limiar e indice: fragmentos, fragmentos de contexto, valor do indice, ECA em
#'   ha, ProtConn = 100 ECA / A_focal, variacao da ECA em relacao ao maior raio e
#'   converged), patch_sensitivity (importancia e posicao de cada fragmento
#'   focal por raio), rank_stability (Spearman, sobreposicao dos 10 maiores e
#'   maior variacao, contra o maior raio), target_sensitivity (por alvo, raio,
#'   limiar e indice: fragmentos encontrados, area focal, importance conjunta,
#'   individual_sum, interaction, importance_change em p.p. contra o maior raio e
#'   converged; NULL sem targets), stepping_stones (fragmentos do contexto com
#'   papel de conector, por raio), polygons (fragmentos do maior contexto por
#'   ano), baseline (fragmentos recortados da area focal), focal, contexts (sf
#'   dos raios), connectivity (land_connectivity por ano e raio, ou NULL),
#'   metadata, sensitivity_plot, rank_plot, map_plot e target_plot.
#' @details Fluxo: (1) a area focal recebe o buffer do maior raio, calculado na
#'   UTM local; (2) os mapas desse contexto vem de source, de x quando ja o
#'   cobrem, ou de um novo [load_rasters()] ou [load_lulc()] com a mesma origem;
#'   colecoes sem dicionario em [dict_build()] funcionam com seus codigos
#'   numericos; (3) as classes
#'   sao reclassificadas como em [lulc_metrics()] e vetorizadas por
#'   [reclass_to_polygons()] (sem gravar arquivos); (4) [land_connectivity()]
#'   roda no maior contexto com `focal`, e sua matriz de distancias (ou a de
#'   [cost_distance()]) e recortada para os demais raios, que usam os fragmentos
#'   inteiros que tocam cada faixa. O raio 0 vetoriza o habitat recortado pela
#'   area focal e reproduz a analise sem contexto.
#'
#'   Com o contexto como trampolim e A_L fixo na area focal, os indices so
#'   crescem com o raio (mais trampolins encurtam caminhos), e a curva converge
#'   quando o contexto deixa de acrescentar caminhos relevantes. Para o PC, o
#'   alcance da influencia e da ordem de -log(eps) / k (p = eps); para o IIC,
#'   que nao decai com a distancia, a convergencia e avaliada pela curva.
#'   converged compara os dois maiores raios; se for FALSE, o efeito de borda
#'   ainda pesa e convem ampliar o maior raio.
#'
#'   Os fragmentos de raios menores sao os do maior contexto que tocam cada
#'   faixa (inteiros, sem corte), o que mantem as distancias exatas no
#'   reaproveitamento. Com distancia de menor custo, os caminhos podem passar
#'   fora da faixa, pela superficie do maior contexto. Na linha de base, partes de
#'   um mesmo fragmento separadas pelo recorte somam sua importancia no
#'   fragmento de origem. A matriz de distancias densa do maior contexto exige
#'   memoria quadratica no numero de fragmentos.
#'
#'   Em targets, cada alvo reune, em cada raio, os fragmentos cujo interior
#'   intersecta suas geometrias (alvos dados por id usam a geometria desses
#'   fragmentos no maior contexto; na linha de base, valem as partes recortadas).
#'   A importance e a queda do indice quando todos sao removidos de uma vez, com
#'   o contexto como trampolim. individual_sum soma as importancias individuais
#'   (dX), e interaction = importance - individual_sum: positiva quando os
#'   fragmentos se substituem (caminhos alternativos; cada um sozinho vale pouco)
#'   e negativa quando dividem a mesma contribuicao (pares entre eles contados
#'   duas vezes na soma). Alvos fora da area focal medem o papel de trampolim de
#'   fragmentos externos. converged compara os dois maiores raios com tolerance
#'   em pontos percentuais.
#' @references Saura, S. et al. (2017) Ecological Indicators 76: 144-158.
#' @md
#' @examples
#' \dontrun{
#' mapas <- load_rasters(area, start = 2024, end = 2024, method = "download",
#'                       export_folder_path = "mapbiomas-recortes")
#' contexto <- connectivity_context(mapas, classes = 3, max_dist = 300,
#'   context = c(0, 300, 600, 1200, 2400), metric_patch = c("IIC", "PC"))
#' contexto$sensitivity
#' contexto$sensitivity_plot
#' contexto$rank_stability
#' # Importancia de um conjunto de fragmentos frente ao contexto
#' alvo <- connectivity_context(mapas, classes = 3, max_dist = 300,
#'   targets = list(nucleo = c(12, 40), corredor = 7))
#' alvo$target_sensitivity
#' # Fora do Brasil, com ESA WorldCover (arvores = 10)
#' wc <- load_lulc(area_exterior, years = 2021)
#' connectivity_context(wc, classes = 10, max_dist = 300)
#' }
#' @export
connectivity_context <- function(x, classes = NULL, context = NULL, max_dist = 300, years = NULL,
                                 metric_patch = c("IIC", "PC"), focal = NULL, source = NULL,
                                 targets = NULL,
                                 resistance = NULL, directions = 8, k = NULL, p_ref = 0.5,
                                 d_ref = NULL, parallel = FALSE, prune = "auto", tolerance = 1,
                                 keep_results = TRUE, language = c("pt", "en"),
                                 build_plot = TRUE) {
  language <- match.arg(language)
  .connectivity_check_flag(build_plot, "build_plot")
  .connectivity_check_flag(keep_results, "keep_results")
  .connectivity_check_parallel(parallel)
  if (!is.numeric(max_dist) || !length(max_dist) || any(!is.finite(max_dist) | max_dist <= 0)) {
    stop("max_dist deve conter distancias positivas em metros.")
  }
  if (is.null(context)) context <- c(0, 1, 2, 4) * max(max_dist)
  if (!is.numeric(context) || length(unique(context)) < 2L || any(!is.finite(context) | context < 0)) {
    stop("context deve ter ao menos dois raios nao negativos, em metros.")
  }
  context <- sort(unique(context))
  if (!is.numeric(tolerance) || length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) {
    stop("tolerance deve ser uma porcentagem nao negativa.")
  }
  if (!directions %in% c(4, 8)) stop("directions deve ser 4 ou 8.")
  invisible(.calibrate_k(max(max_dist), k, p_ref, d_ref))
  inputs <- .context_inputs(x, classes, years, focal, source, max(context), directions)
  focal <- inputs$focal
  # A number of processes opens one plan for every year and radius.
  if (is.numeric(parallel)) {
    old_plan <- future::plan(future::multisession, workers = parallel)
    on.exit(future::plan(old_plan), add = TRUE)
    parallel <- TRUE
  }
  common <- list(max_dist = max_dist, metric_patch = metric_patch, k = k, p_ref = p_ref,
                 d_ref = d_ref, parallel = parallel, prune = prune, language = language,
                 build_plot = FALSE)
  per_year <- lapply(names(inputs$polygons), function(yr) {
    .context_year(inputs$polygons[[yr]], inputs$baseline[[yr]], focal, context, common,
                  .context_resistance(resistance, inputs$maps, as.numeric(yr)),
                  .context_targets(targets, inputs$polygons[[yr]]))
  })
  names(per_year) <- names(inputs$polygons)
  bind <- function(field) {
    out <- do.call(rbind, lapply(names(per_year), function(yr) {
      z <- per_year[[yr]][[field]]
      if (is.null(z) || !nrow(z)) return(NULL)
      data.frame(year = as.numeric(yr), z)
    }))
    if (!is.null(out)) rownames(out) <- NULL
    out
  }
  sensitivity <- .context_converged(bind("sensitivity"), tolerance)
  patches <- bind("patches")
  stability <- .context_stability(patches)
  stepping <- bind("stepping")
  target_rows <- bind("targets")
  target_sensitivity <- if (!is.null(target_rows)) .context_target_change(target_rows, tolerance)
  contexts <- do.call(rbind, lapply(context, function(r) {
    sf::st_sf(radius_m = r, geometry = sf::st_geometry(.context_buffer(focal, r)))
  }))
  plots <- list(sensitivity_plot = NULL, rank_plot = NULL, map_plot = NULL, target_plot = NULL)
  if (build_plot) {
    if (!is.null(target_sensitivity)) {
      plots$target_plot <- .context_target_plot(target_sensitivity, max_dist, language)
    }
    plots$sensitivity_plot <- .context_sensitivity_plot(sensitivity, max_dist, language)
    plots$rank_plot <- .context_rank_plot(stability, language)
    last <- per_year[[length(per_year)]]
    plots$map_plot <- .context_map_plot(last$results[[length(last$results)]], focal, contexts,
                                        language)
  }
  metadata <- list(schema_version = "1.0",
    package_version = as.character(utils::packageVersion("biomastats")),
    years = as.numeric(names(per_year)), context_m = context, max_dist_m = sort(unique(max_dist)),
    metric_patch = metric_patch, classes = inputs$classes, maps_source = inputs$origin,
    focal_area_m2 = per_year[[1]]$focal_area, distance = if (is.null(resistance))
      "edge-to-edge, computed once at the largest context" else
      "least-cost edge-to-edge, computed once at the largest context",
    rule = paste("focal habitat as attribute, context patches as stepping stones",
      "(attribute 0); whole patches touching each buffer; radius 0 = habitat clipped",
      "to the focal area"),
    convergence = paste0("|ECA(largest) - ECA(second largest)| / ECA(largest) <= ",
      tolerance, "%; targets: importance difference <= ", tolerance, " p.p."),
    targets = if (!is.null(targets)) paste("joint removal of the patches whose interior",
      "meets each target geometry; interaction = joint - sum of single importances"))
  structure(c(list(sensitivity = sensitivity, patch_sensitivity = patches,
    rank_stability = stability, target_sensitivity = target_sensitivity,
    stepping_stones = stepping, polygons = inputs$polygons,
    baseline = inputs$baseline, focal = focal, contexts = contexts,
    connectivity = if (keep_results) lapply(per_year, `[[`, "results"),
    metadata = metadata), plots), class = c("biomastats_connectivity_context", "list"))
}

# Polygons of the largest context and of the clipped focal area, per year.
.context_inputs <- function(x, classes, years, focal, source, radius, directions) {
  polygons_input <- inherits(x, "sf") ||
    (is.list(x) && length(x) && all(vapply(x, inherits, logical(1), what = "sf")))
  if (polygons_input) {
    if (is.null(focal)) focal <- attr(if (inherits(x, "sf")) x else x[[1]], "focal")
    if (is.null(focal)) stop("Informe focal para poligonos sem o atributo focal.")
    years_x <- if (inherits(x, "sf")) unique(stats::na.omit(x$year)) else as.numeric(names(x))
    if (is.null(years)) years <- max(years_x)
    polygons <- lapply(years, function(y) .connectivity_select_year(x, y))
    names(polygons) <- as.character(years)
    focal <- .read_study_area(focal)
    baseline <- lapply(polygons, .context_clip, focal = focal)
    return(list(polygons = polygons, baseline = baseline, focal = focal, maps = NULL,
                classes = attr(polygons[[1]], "metadata")$classes, origin = "polygons"))
  }
  if (!is.list(x) || !all(c("raster", "time_range") %in% names(x))) {
    stop("x deve ser objeto de load_rasters ou poligonos de reclass_to_polygons.")
  }
  if (is.null(classes) || !is.numeric(classes) || anyNA(classes) || any(classes <= 0) ||
      any(classes != trunc(classes))) {
    stop("Informe classes: os codigos inteiros positivos do habitat.")
  }
  if (is.null(focal)) focal <- if (!is.null(x$focal)) x$focal else x$shape
  if (is.null(focal)) stop("Informe focal: os mapas nao trazem a area de estudo (shape).")
  focal <- .read_study_area(focal)
  tr <- x$time_range
  if (is.null(years)) years <- tr[2]
  if (!is.numeric(years) || any(years < tr[1] | years > tr[2])) stop("years fora do intervalo dos mapas.")
  years <- sort(unique(years))
  zone <- .context_buffer(focal, radius)
  maps <- .context_maps(x, source, focal, zone, radius, years)
  known <- .context_dictionary(maps$collection, classes, maps$class_dictionary)
  collection <- known$collection
  dictionary <- known$dictionary
  vectorize <- function(r, y, boundary, focal_attr) {
    obj <- list(rec_map = list(r), start = y, end = y, classes = classes,
                collection = collection, class_dictionary = dictionary,
                boundary = boundary, focal = focal_attr)
    reclass_to_polygons(obj, years = y, directions = directions, save_shp = FALSE)
  }
  polygons <- baseline <- list()
  for (y in years) {
    r <- .context_reclass(maps$raster[[y - maps$time_range[1] + 1L]], classes)
    polygons[[as.character(y)]] <- vectorize(r, y, zone, focal)
    inner <- raster::mask(r, sf::st_transform(focal, sf::st_crs(r)))
    baseline[[as.character(y)]] <- vectorize(inner, y, focal, NULL)
  }
  list(polygons = polygons, baseline = baseline, focal = focal, maps = maps, classes = classes,
       origin = maps$origin)
}

# Maps covering the context zone: from `source`, from x when it already covers
# the zone, or reloaded with the origin recorded by load_rasters().
.context_maps <- function(x, source, focal, zone, radius, years) {
  crop <- function(m, origin) {
    m$raster <- lapply(m$raster, function(r) {
      z <- sf::st_transform(zone, sf::st_crs(r))
      raster::mask(raster::crop(r, z), z)
    })
    m$shape <- zone
    m$origin <- origin
    m
  }
  if (is.function(source)) {
    maps <- source(zone, years)
    if (!is.list(maps) || !all(c("raster", "time_range") %in% names(maps))) {
      stop("A funcao em source deve devolver mapas no formato de load_rasters().")
    }
    if (is.null(maps$collection)) maps$collection <- x$collection
    return(crop(maps, "source function"))
  }
  if (!is.null(source)) {
    if (inherits(source, c("RasterLayer", "SpatRaster"))) {
      if (length(years) != 1L) stop("Um raster em source cobre um unico ano.")
      layer <- if (inherits(source, "SpatRaster")) raster::raster(source) else source
      if (inherits(source, "SpatRaster")) {
        layer <- raster::setValues(layer, terra::values(source, mat = FALSE))
      }
      source <- list(raster = list(layer), time_range = c(years, years),
                     collection = x$collection)
    }
    if (!is.list(source) || !all(c("raster", "time_range") %in% names(source)) ||
        any(years < source$time_range[1] | years > source$time_range[2])) {
      stop("source deve ser mapas (formato de load_rasters) que cubram years.")
    }
    if (is.null(source$collection)) source$collection <- x$collection
    return(crop(source, "source"))
  }
  covered <- !is.null(x$source$context_m) && x$source$context_m >= radius
  if (covered) return(crop(x, "maps"))
  s <- x$source
  loader <- if (!is.null(s$loader)) s$loader else if (!is.null(s$method)) "load_rasters"
  if (is.null(loader)) {
    stop("Os mapas nao registram a origem; recarregue com load_rasters() ou load_lulc(), ",
         "ou informe source.")
  }
  maps <- switch(loader,
    load_rasters = load_rasters(focal, start = min(years), end = max(years), method = s$method,
      export_folder_path = s$export_folder_path, import_folder_path = s$import_folder_path,
      type = s$type, collection = s$collection, context_m = radius, product = s$product,
      url = s$url, dictionary = s$dictionary, nodata = if (is.null(s$nodata)) 0 else s$nodata,
      timeout = s$timeout),
    load_lulc = load_lulc(focal, source = s$product, years = years, timeout = s$timeout,
      context_m = radius),
    load_local_rasters = suppressMessages(load_local_rasters(focal, folder = s$folder, prefix = s$prefix,
      years = years, collection = s$collection, dictionary = s$dictionary, nodata = s$nodata,
      crs = s$crs, resolution = s$resolution, context_m = radius)),
    stop("Carregador desconhecido em source: ", loader))
  if (is.null(maps)) stop("Nao foi possivel carregar os mapas do contexto (", loader, ").")
  crop(maps, if (loader == "load_rasters") paste0("load_rasters (", s$method, ")") else loader)
}

# Collection and dictionary entries of `classes`: from the maps' own
# dictionary, from dict_build(), or generic names for codes without one.
.context_dictionary <- function(collection, classes, dictionary = NULL) {
  collection <- tryCatch(.biomastats_collection(collection), error = function(e) NULL)
  if (is.null(collection) && is.null(dictionary)) return(list(collection = NULL, dictionary = NULL))
  list(collection = collection, dictionary = .biomastats_dictionary(collection, classes, dictionary))
}

# Same reclassification as lulc_metrics(): one class keeps its code, several
# become 1; other valid cells are 0.
.context_reclass <- function(r, classes) {
  values <- raster::getValues(r)
  keep <- !is.na(values) & values %in% classes
  values[!is.na(values) & !keep] <- 0
  values[keep] <- if (length(classes) == 1L) classes else 1
  raster::setValues(r, values)
}

# Polygons clipped to the focal area, keeping the original fragment_id.
.context_clip <- function(p, focal) {
  if ("value" %in% names(p)) p <- p[!is.na(p$value) & p$value == 1, , drop = FALSE]
  zone <- sf::st_union(sf::st_geometry(sf::st_transform(focal, sf::st_crs(p))))
  sf::st_agr(p) <- "constant"
  clipped <- suppressWarnings(sf::st_intersection(p, zone))
  # Touching patches leave points or lines; collections keep their polygon parts.
  if (any(sf::st_geometry_type(clipped) == "GEOMETRYCOLLECTION")) {
    clipped <- suppressWarnings(sf::st_collection_extract(clipped, "POLYGON"))
  }
  clipped <- clipped[sf::st_dimension(clipped) == 2L, , drop = FALSE]
  clipped <- sf::st_cast(clipped, "MULTIPOLYGON", warn = FALSE)
  # One feature per original patch, even when the clip splits it.
  ids <- unique(clipped$fragment_id)
  geometry <- do.call(c, lapply(ids, function(id) {
    sf::st_union(sf::st_geometry(clipped)[clipped$fragment_id == id])
  }))
  clipped <- sf::st_sf(value = 1, fragment_id = ids,
                       geometry = sf::st_cast(sf::st_sfc(geometry, crs = sf::st_crs(p)),
                                              "MULTIPOLYGON"))
  attr(clipped, "boundary") <- sf::st_sf(geometry = zone)
  attr(clipped, "focal") <- NULL
  clipped
}

# Surface for the least-cost distance of one year (NULL for edge distance).
.context_resistance <- function(resistance, maps, year) {
  if (is.null(resistance) || inherits(resistance, c("biomastats_resistance", "RasterLayer",
                                                    "SpatRaster"))) return(resistance)
  if (is.null(maps)) stop("Custos por classe exigem mapas; informe uma superficie pronta.")
  resistance_surface(maps, year = year, resistance = resistance, build_plot = FALSE)
}

# One year: the largest context first (its distances are reused), then the
# smaller radii and the clipped baseline.
.context_year <- function(polygons, baseline, focal, context, common, resistance,
                          targets = NULL) {
  p <- polygons
  if ("value" %in% names(p)) p <- p[!is.na(p$value) & p$value == 1, , drop = FALSE]
  if (!nrow(p)) stop("Nenhum fragmento de habitat no contexto.")
  zone_crs <- sf::st_crs(p)
  radii <- rev(context[context > 0])
  D <- if (!is.null(resistance)) cost_distance(p, resistance)
  results <- list()
  for (r in radii) {
    zone <- .context_buffer(sf::st_transform(focal, zone_crs), r)
    sel <- lengths(sf::st_intersects(p, zone)) > 0L
    sub <- p[sel, , drop = FALSE]
    ids <- as.character(sub$fragment_id)
    distance <- if (!is.null(D)) structure(D[ids, ids, drop = FALSE], unit = attr(D, "unit"),
                                          method = paste(attr(D, "method"), "(reused)"))
    res <- do.call(land_connectivity, c(list(sub, boundary = zone, focal = focal,
                                             distance = distance), common))
    if (is.null(D)) {
      D <- structure(res$distance, unit = "m", method = "edge-to-edge (sf::st_distance), reused")
    }
    results[[as.character(r)]] <- res
  }
  if (0 %in% context) {
    b <- baseline
    if ("value" %in% names(b)) b <- b[!is.na(b$value) & b$value == 1, , drop = FALSE]
    results[["0"]] <- do.call(land_connectivity, c(list(b, boundary = attr(b, "boundary")),
                                                   common))
  }
  results <- results[as.character(context)]
  largest <- results[[length(results)]]
  stepping <- do.call(rbind, lapply(seq_along(results), function(i) {
    s <- .context_stepping(results[[i]])
    if (!is.null(s)) data.frame(radius_m = context[i], s)
  }))
  list(results = results, focal_area = largest$metadata$landscape_area_m2,
       sensitivity = .context_indices(results, context),
       patches = .context_patches(results, context, p), stepping = stepping,
       targets = if (!is.null(targets)) .context_target_rows(results, context, targets))
}

# Target geometries (one sf row per target, in the CRS of the polygons).
.context_targets <- function(targets, polygons) {
  if (is.null(targets)) return(NULL)
  p <- polygons
  if ("value" %in% names(p)) p <- p[!is.na(p$value) & p$value == 1, , drop = FALSE]
  if (inherits(targets, c("sf", "sfc"))) {
    t <- if (inherits(targets, "sfc")) sf::st_sf(geometry = targets) else targets
    if (is.na(sf::st_crs(t))) stop("targets precisa de CRS.")
    t <- sf::st_transform(t, sf::st_crs(p))
    group <- if ("target" %in% names(t)) as.character(t$target) else as.character(seq_len(nrow(t)))
    geoms <- lapply(unique(group), function(g) sf::st_union(sf::st_geometry(t)[group == g]))
    return(sf::st_sf(target = unique(group), geometry = do.call(c, geoms)))
  }
  sets <- if (is.list(targets)) targets else list(target = targets)
  if (is.null(names(sets)) || any(!nzchar(names(sets)))) names(sets) <- seq_along(sets)
  if (!all(vapply(sets, function(s) is.numeric(s) && length(s) > 0, logical(1)))) {
    stop("targets deve ser fragment_id, lista nomeada de fragment_id ou sf.")
  }
  missing <- setdiff(unlist(sets), p$fragment_id)
  if (length(missing)) stop("targets com fragment_id ausentes do maior contexto: ",
                            paste(utils::head(missing, 5), collapse = ", "))
  geoms <- lapply(sets, function(s) sf::st_union(sf::st_geometry(p)[p$fragment_id %in% s]))
  sf::st_sf(target = names(sets), geometry = do.call(c, geoms))
}

# Joint and summed single importance of each target, at every radius,
# threshold and index.
.context_target_rows <- function(results, context, targets) {
  do.call(rbind, lapply(seq_along(results), function(i) {
    res <- results[[i]]
    a <- res$analysis_polygons
    weights <- if ("focal_area_m2" %in% names(a)) a$focal_area_m2 else a$area_m2
    tg <- sf::st_transform(targets, sf::st_crs(a))
    members <- sf::st_relate(tg, a, pattern = "2********")
    D <- unname(res$distance)
    pm <- res$patch_metrics
    do.call(rbind, lapply(seq_len(nrow(res$metrics)), function(j) {
      md <- res$metrics$max_dist_m[j]
      rows <- pm$max_dist_m == md
      do.call(rbind, lapply(res$metadata$metric_patch, function(metric) {
        k <- if (metric == "IIC") NULL else res$metrics$k[j]
        single <- pm[[paste0("d", metric)]][rows]
        do.call(rbind, lapply(seq_len(nrow(tg)), function(t) {
          set <- members[[t]]
          joint <- .set_importance(D, weights, md, metric, k, set)
          total <- sum(single[set])
          data.frame(target = tg$target[t], radius_m = context[i], max_dist_m = md,
            metric = metric, patches = length(set), focal_area_ha = sum(weights[set]) / 10000,
            importance = joint, individual_sum = total, interaction = joint - total)
        }))
      }))
    }))
  }))
}

.context_target_change <- function(t, tolerance) {
  key <- paste(t$year, t$target, t$max_dist_m, t$metric)
  t$importance_change <- NA_real_
  t$converged <- NA
  for (g in unique(key)) {
    i <- which(key == g)
    i <- i[order(t$radius_m[i])]
    ref <- t$importance[i[length(i)]]
    t$importance_change[i] <- t$importance[i] - ref
    t$converged[i] <- is.finite(ref) && abs(ref - t$importance[i[length(i) - 1L]]) <= tolerance
  }
  t <- t[order(t$year, t$target, t$metric, t$max_dist_m, t$radius_m), , drop = FALSE]
  rownames(t) <- NULL
  t
}

.context_target_plot <- function(t, max_dist, language) {
  t$panel <- paste0(t$metric, " | ", t$max_dist_m, " m")
  if (length(unique(t$year)) > 1L) t$panel <- paste(t$panel, t$year)
  ggplot2::ggplot(t, ggplot2::aes(x = radius_m, y = importance, colour = target)) +
    ggplot2::geom_vline(xintercept = sort(unique(max_dist)), linetype = "dashed",
                        colour = "#9a9a9a") +
    ggplot2::geom_line(ggplot2::aes(y = individual_sum), linetype = "dotted", linewidth = 0.6) +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_viridis_d(end = 0.85, name = .tr(language, "Alvo", "Target")) +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::labs(x = .tr(language, "Raio do contexto (m)", "Context radius (m)"),
      y = .tr(language, "Import\u00e2ncia conjunta (%)", "Joint importance (%)"),
      caption = .tr(language, "Pontilhado: soma das import\u00e2ncias individuais",
                    "Dotted: sum of single importances")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

.context_indices <- function(results, context) {
  do.call(rbind, lapply(seq_along(results), function(i) {
    res <- results[[i]]
    m <- res$metrics
    role <- res$analysis_polygons$focal_role
    do.call(rbind, lapply(intersect(c("IIC", "PC"), res$metadata$metric_patch), function(metric) {
      eca <- m[[paste0("ECA_", metric)]]
      data.frame(radius_m = context[i], max_dist_m = m$max_dist_m, metric = metric,
        patches = nrow(res$analysis_polygons),
        context_patches = if (is.null(role)) 0L else sum(role == "context"),
        index = m[[metric]], ECA_ha = eca / 10000,
        ProtConn = 100 * eca / res$metadata$landscape_area_m2)
    }))
  }))
}

# Importance of the focal patches (ids of the largest context) at every radius.
# Baseline pieces are mapped to the context patch that contains them.
.context_patches <- function(results, context, polygons) {
  largest <- results[[length(results)]]
  metrics <- intersect(c("IIC", "PC", "AWF"), largest$metadata$metric_patch)
  rows <- lapply(seq_along(results), function(i) {
    pm <- results[[i]]$patch_metrics
    if (context[i] == 0) {
      parent <- .context_parent(pm, polygons)
      pm$fragment_id <- parent
    }
    keep <- if ("focal_role" %in% names(pm)) pm$focal_role != "context" else rep(TRUE, nrow(pm))
    pm <- sf::st_drop_geometry(pm)[keep, , drop = FALSE]
    do.call(rbind, lapply(metrics, function(metric) {
      v <- pm[[paste0("d", metric)]]
      agg <- stats::aggregate(list(importance = v), list(fragment_id = pm$fragment_id,
                              max_dist_m = pm$max_dist_m), sum)
      data.frame(radius_m = context[i], metric = metric, agg)
    }))
  })
  out <- do.call(rbind, rows)
  roles <- sf::st_drop_geometry(largest$patch_metrics)
  roles <- unique(roles[, c("fragment_id", "focal_role")])
  out$focal_role <- roles$focal_role[match(out$fragment_id, roles$fragment_id)]
  key <- paste(out$radius_m, out$max_dist_m, out$metric)
  out$rank <- stats::ave(-out$importance, key, FUN = function(v) rank(v, ties.method = "min"))
  out <- out[order(out$metric, out$max_dist_m, out$radius_m, out$rank), , drop = FALSE]
  rownames(out) <- NULL
  out[, c("radius_m", "max_dist_m", "metric", "fragment_id", "focal_role", "importance", "rank")]
}

.context_parent <- function(pm, polygons) {
  pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(pm)))
  pts <- sf::st_transform(pts, sf::st_crs(polygons))
  hit <- sf::st_intersects(pts, polygons)
  polygons$fragment_id[vapply(hit, function(h) if (length(h)) h[1] else NA_integer_, integer(1))]
}

.context_stepping <- function(res) {
  pm <- sf::st_drop_geometry(res$patch_metrics)
  if (!"focal_role" %in% names(pm)) return(NULL)
  metrics <- intersect(c("IIC", "PC"), res$metadata$metric_patch)
  out <- do.call(rbind, lapply(metrics, function(metric) {
    v <- pm[[paste0("d", metric, "_connector")]]
    take <- pm$focal_role == "context" & is.finite(v) & v > 0
    if (!any(take)) return(NULL)
    data.frame(max_dist_m = pm$max_dist_m[take], metric = metric,
               fragment_id = pm$fragment_id[take], area_ha = pm$area_ha[take],
               connector = v[take])
  }))
  if (is.null(out)) return(NULL)
  out[order(out$metric, out$max_dist_m, -out$connector), , drop = FALSE]
}

.context_converged <- function(s, tolerance) {
  key <- paste(s$year, s$max_dist_m, s$metric)
  s$ECA_change <- NA_real_
  s$converged <- NA
  for (g in unique(key)) {
    i <- which(key == g)
    i <- i[order(s$radius_m[i])]
    ref <- s$ECA_ha[i[length(i)]]
    s$ECA_change[i] <- 100 * (s$ECA_ha[i] - ref) / ref
    second <- s$ECA_ha[i[length(i) - 1L]]
    s$converged[i] <- is.finite(ref) && ref > 0 && abs(ref - second) / ref * 100 <= tolerance
  }
  s[order(s$year, s$metric, s$max_dist_m, s$radius_m), , drop = FALSE]
}

# Rank agreement of the focal patches with the largest radius.
.context_stability <- function(patches) {
  key <- paste(patches$year, patches$max_dist_m, patches$metric)
  out <- do.call(rbind, lapply(split(patches, factor(key, unique(key))), function(g) {
    ref_r <- max(g$radius_m)
    ref <- g[g$radius_m == ref_r, ]
    do.call(rbind, lapply(sort(unique(g$radius_m)), function(r) {
      cur <- g[g$radius_m == r, ]
      ids <- intersect(ref$fragment_id, cur$fragment_id)
      a <- ref$importance[match(ids, ref$fragment_id)]
      b <- cur$importance[match(ids, cur$fragment_id)]
      ok <- is.finite(a) & is.finite(b)
      top <- min(10L, length(ids))
      top_ref <- utils::head(ids[order(-a)], top)
      top_cur <- utils::head(ids[order(-b)], top)
      data.frame(year = g$year[1], max_dist_m = g$max_dist_m[1], metric = g$metric[1],
        radius_m = r, patches = length(ids),
        spearman = if (sum(ok) > 2 && stats::sd(a[ok]) > 0 && stats::sd(b[ok]) > 0)
          stats::cor(a[ok], b[ok], method = "spearman") else NA_real_,
        top10_overlap = if (top) 100 * length(intersect(top_ref, top_cur)) / top else NA_real_,
        max_change = if (any(ok)) max(abs(b[ok] - a[ok])) else NA_real_)
    }))
  }))
  rownames(out) <- NULL
  out
}

.context_sensitivity_plot <- function(s, max_dist, language) {
  s$threshold <- factor(paste0(s$max_dist_m, " m"), levels = paste0(sort(unique(s$max_dist_m)), " m"))
  s$panel <- if (length(unique(s$year)) > 1L) paste(s$metric, s$year) else s$metric
  ggplot2::ggplot(s, ggplot2::aes(x = radius_m, y = ECA_change + 100, colour = threshold)) +
    ggplot2::geom_vline(xintercept = sort(unique(max_dist)), linetype = "dashed",
                        colour = "#9a9a9a") +
    ggplot2::geom_hline(yintercept = 100, colour = "#555555") +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_viridis_d(end = 0.85,
      name = .tr(language, "Dist\u00e2ncia m\u00e1xima", "Maximum distance")) +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::labs(x = .tr(language, "Raio do contexto (m)", "Context radius (m)"),
      y = .tr(language, "ECA (% do maior contexto)", "ECA (% of the largest context)")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

.context_rank_plot <- function(stability, language) {
  long <- rbind(
    data.frame(stability[, c("year", "max_dist_m", "metric", "radius_m")], measure = "Spearman",
               value = stability$spearman),
    data.frame(stability[, c("year", "max_dist_m", "metric", "radius_m")],
               measure = .tr(language, "10 maiores (fra\u00e7\u00e3o)", "Top 10 (fraction)"),
               value = stability$top10_overlap / 100))
  long$panel <- paste0(long$metric, " | ", long$max_dist_m, " m")
  ggplot2::ggplot(long[is.finite(long$value), ], ggplot2::aes(x = radius_m, y = value,
                                                              colour = measure)) +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = c("#245b78", "#d08c2d"), name = NULL) +
    ggplot2::coord_cartesian(ylim = c(min(0, long$value, na.rm = TRUE), 1)) +
    ggplot2::facet_wrap(~panel) +
    ggplot2::labs(x = .tr(language, "Raio do contexto (m)", "Context radius (m)"),
      y = .tr(language, "Concord\u00e2ncia com o maior contexto",
              "Agreement with the largest context")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

.context_map_plot <- function(res, focal, contexts, language) {
  p <- res$patch_metrics
  p <- p[p$max_dist_m == min(p$max_dist_m), , drop = FALSE]
  labels <- c(focal = .tr(language, "Focal", "Focal"),
              edge = .tr(language, "Cruza o limite", "Crosses the boundary"),
              context = .tr(language, "Contexto", "Context"))
  p$role <- factor(labels[p$focal_role], levels = labels)
  crs <- sf::st_crs(p)
  rings <- sf::st_transform(contexts[contexts$radius_m > 0, ], crs)
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = rings, fill = NA, colour = "#7a7a7a", linetype = "dashed",
                     linewidth = 0.3) +
    ggplot2::geom_sf(data = p, ggplot2::aes(fill = role), colour = "#555555", linewidth = 0.1) +
    ggplot2::geom_sf(data = sf::st_transform(focal, crs), fill = NA, colour = "#202020",
                     linewidth = 0.7) +
    ggplot2::scale_fill_manual(values = stats::setNames(c("#125c40", "#6aa37f", "#c9c9c9"), labels),
                               name = NULL, drop = FALSE) +
    ggplot2::coord_sf(datum = NA) + ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables(c("radius_m", "ECA_change", "threshold", "measure", "role", "target",
                         "individual_sum"))
