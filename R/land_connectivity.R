#' Conectividade de fragmentos vetorizados e interoperabilidade com lconnect
#'
#' Integra diretamente os poligonos sf de [reclass_to_polygons()] ao fluxo de
#' lconnect, usando as rotinas otimizadas do biomastats. Nao procura arquivos,
#' nao grava shapefiles e nao recalcula distancias nas remocoes de fragmentos.
#' @param polygons Objeto sf de reclass_to_polygons, inclusive apos readRDS,
#'   ou sua lista temporal nomeada. Coluna value, quando presente, seleciona
#'   somente presenca (1). Fragmentos devem ser POLYGON validos e sem sobreposicao
#'   de interiores. fragment_id e preservado; frag_id de SHP e reconhecido.
#' @param max_dist Uma ou varias distancias positivas em metros (default 300).
#'   A matriz de distancias e calculada uma vez e reutilizada em todos os cenarios.
#' @param year Ano a analisar. Herdado para sf de um unico ano ou lista de um
#'   unico elemento. Uma serie com varios anos exige escolha explicita.
#' @param boundary Limite sf/sfc opcional para denominadores de paisagem.
#'   NULL herda o atributo boundary dos poligonos; sem ele usa o fecho convexo
#'   dos fragmentos, como upload_land. A origem do limite consta nos metadados.
#' @param crs CRS projetado em metros para analise. NULL conserva um CRS ja
#'   projetado em metros. Caso contrario escolhe UTM local para extensoes de ate
#'   seis graus de longitude entre 80S e 84N; regioes maiores exigem CRS explicito.
#'   A geometria original e preservada em source_polygons.
#' @param metric_patch Indices de priorizacao, IIC e/ou AWF; padrao ambos.
#' @param language Idioma dos rotulos: pt (padrao) ou en; dicionario preserva ambos.
#' @param build_plot Construir ggplots (TRUE por padrao).
#' @return Lista biomastats_connectivity com source_polygons, analysis_polygons,
#'   boundary, distance (matriz em metros), metrics (data.frame por limiar),
#'   patch_metrics (sf por fragmento/limiar), edge_metrics (data.frame),
#'   scenarios (lista nomeada por limiar, cada item com land de classe lconnect,
#'   graph igraph com todos os fragmentos e patch_importance com objetos pimp),
#'   class_dictionary, metadata, plot (grafo espacial ggplot), cluster_plot,
#'   importance_plot e range_plot. Todos sao reutilizaveis apos saveRDS/readRDS.
#' @details Distancias sao minimas entre bordas de poligonos, nao entre centroides.
#'   Os indices NC, LNK, SLC, MSC, CCP, LCP, CPL, ECS, AWF e IIC reproduzem as
#'   convencoes de lconnect 0.1.2 com rotinas nativas otimizadas. Areas SLC, MSC
#'   e ECS sao m2; AWF e m4; CPL e numero de passos; IIC, CCP e LCP sao adimensionais.
#'
#'   O grafo de exploracao usa distancia < max_dist, incluindo contatos a distancia
#'   zero, e preserva isolados. Para compatibilidade, NC e indices de componentes
#'   usam distancia <= max_dist (single linkage de lconnect), enquanto IIC e CPL
#'   usam 0 < distancia < max_dist e excluem isolados. Assim, em empates ou contatos
#'   de distancia zero, NC pode diferir de graph_components e componentes do grafo.
#'   patch_metrics distingue cluster_lconnect, component e iic_included.
#'   Estas diferencas sao explicitas; nao se deve interpretar NC como contagem
#'   dos componentes do grafo estrito nos casos de fronteira do limiar.
#'
#'   IIC mantem o denominador area_l fixo na remocao de fragmentos. AWF nao corta
#'   pares pelo limiar; max_dist controla seu decaimento exponencial. Em indices
#'   inicialmente zero, a priorizacao e indefinida (NaN), com status
#'   "undefined_baseline" no objeto pimp. Um fragmento nao tem ranking relativo e
#'   recebe o mesmo status. CPL sem pares conectados e NA. Nenhum indice prova
#'   conectividade funcional.
#'
#'   A normalizacao utiliza o limite herdado ou informado; sem limite, o fecho
#'   convexo e apenas uma aproximacao da paisagem. Reprojecao local e exclusiva
#'   desta analise metrica e registra CRS de origem/analise. IDs, ano, selecao,
#'   paleta e proveniencia sao herdados. Classes fundidas nao recebem identidades
#'   individuais inventadas. Nao dissolve nem repara fragmentos implicitamente.
#'
#'   plot usa cores das classes herdadas e simbolos para isolados. As linhas
#'   ligam pontos interiores apenas para visualizacao; nao sao corredores nem
#'   representam a linha da menor distancia entre bordas. cluster_plot usa cores
#'   categoricas de componentes (nao classes de uso). importance_plot usa escala
#'   continua da reducao percentual. Todos aceitam labs, temas e escalas ggplot2;
#'   use ggsave/st_write/write.csv/write_graph/saveRDS nos retornos para escrita.
#'   O pacote lconnect e opcional: os objetos land/pimp sao construidos em memoria
#'   com seu contrato e podem ser consumidos pelas suas funcoes quando instalado.
#'   A matriz de distancias e os calculos de caminhos requerem memoria quadratica.
#' @references [lconnect 0.1.2](https://cran.r-project.org/package=lconnect).
#' @md
#' @examples
#' \dontrun{
#' poligonos <- reclass_to_polygons(metricas, save_shp = FALSE)
#' conectividade <- land_connectivity(poligonos, max_dist = c(30, 100, 300))
#' conectividade$plot + ggplot2::labs(title = "Conectividade dos fragmentos")
#' conectividade$metrics
#' lconnect::con_metric(conectividade$scenarios[[1]]$land, c("NC", "LNK", "IIC"))
#' }
#' @export
land_connectivity <- function(polygons, max_dist = 300, year = NULL, boundary = NULL,
                              crs = NULL, metric_patch = c("IIC", "AWF"),
                              language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  .connectivity_check_flag(build_plot, "build_plot")
  if (!is.numeric(max_dist) || !length(max_dist) || any(!is.finite(max_dist) | max_dist <= 0)) {
    stop("max_dist deve conter distancias positivas em metros.")
  }
  max_dist <- sort(unique(max_dist))
  if (!is.character(metric_patch) || !length(metric_patch) || anyNA(metric_patch) ||
      any(!metric_patch %in% c("IIC", "AWF"))) stop("metric_patch deve conter IIC e/ou AWF.")
  metric_patch <- unique(metric_patch)
  polygons <- .connectivity_select_year(polygons, year)
  inherited <- attr(polygons, "metadata")
  dictionary <- attr(polygons, "class_dictionary")
  inherited_boundary <- attr(polygons, "boundary")
  original_crs <- sf::st_crs(polygons)
  if (is.na(original_crs)) stop("Poligonos sem CRS.")
  if ("value" %in% names(polygons)) {
    polygons <- polygons[!is.na(polygons$value) & polygons$value == 1, , drop = FALSE]
  }
  if (!nrow(polygons)) stop("Nenhum fragmento de habitat disponivel.")
  if (any(sf::st_is_empty(polygons)) || any(!sf::st_is_valid(polygons)) ||
      any(as.character(sf::st_geometry_type(polygons)) != "POLYGON")) {
    stop("Fragmentos devem ser POLYGON validos e nao vazios; use reclass_to_polygons.")
  }
  if (!"fragment_id" %in% names(polygons)) {
    polygons$fragment_id <- if ("frag_id" %in% names(polygons)) polygons$frag_id else seq_len(nrow(polygons))
  }
  if (anyNA(polygons$fragment_id) || anyDuplicated(polygons$fragment_id)) stop("fragment_id deve ser unico e sem NA.")
  collection <- if (!is.null(inherited$collection)) inherited$collection else
    if ("collection" %in% names(polygons)) unique(stats::na.omit(polygons$collection)) else NULL
  if (!length(collection)) collection <- NULL
  if (!is.null(collection)) collection <- .biomastats_collection(collection)
  classes <- inherited$classes
  if (is.null(classes) && "classes" %in% names(polygons)) {
    encoded <- unique(as.character(stats::na.omit(polygons$classes)))
    if (length(encoded) && all(grepl("^[0-9]+(,[0-9]+)*$", encoded))) {
      classes <- sort(unique(as.numeric(unlist(strsplit(encoded, ",", fixed = TRUE)))))
      if (any(!is.finite(classes))) stop("Codigos de classes invalidos nos poligonos.")
    }
  }
  if (is.null(classes) && "code" %in% names(polygons)) classes <- sort(unique(stats::na.omit(polygons$code)))
  if (is.null(dictionary) && !is.null(collection) && length(classes)) {
    dictionary <- .biomastats_dictionary(collection, classes)
  }
  analysis_crs <- .connectivity_crs(polygons, crs)
  p <- sf::st_transform(polygons, analysis_crs)
  # Positive-area interior intersections would double count habitat.
  if (any(lengths(sf::st_relate(p, pattern = "T********")) > 1L)) {
    stop("Fragmentos com interiores sobrepostos; a paisagem deve estar fragmentada.")
  }
  areas <- as.numeric(units::set_units(sf::st_area(p), "m^2"))
  if (any(!is.finite(areas) | areas <= 0)) stop("Areas dos fragmentos devem ser positivas.")
  source <- polygons
  if ("area_ha" %in% names(p)) p$source_area_ha <- p$area_ha
  p$area_m2 <- areas
  p$area_ha <- areas / 10000
  if (is.null(boundary)) boundary <- inherited_boundary
  boundary_method <- if (!is.null(boundary)) {
    if (!is.null(inherited_boundary) && identical(boundary, inherited_boundary)) "inherited" else "supplied"
  } else "habitat convex hull"
  if (is.null(boundary)) boundary <- sf::st_convex_hull(sf::st_union(sf::st_geometry(p)))
  if (!inherits(boundary, c("sf", "sfc")) || is.na(sf::st_crs(boundary))) stop("boundary deve ser sf/sfc com CRS.")
  bound <- sf::st_union(sf::st_geometry(sf::st_transform(boundary, analysis_crs)))
  if (any(sf::st_is_empty(bound)) || any(!sf::st_is_valid(bound)) ||
      !all(as.character(sf::st_geometry_type(bound)) %in% c("POLYGON", "MULTIPOLYGON"))) stop("boundary deve ser poligonal e valido.")
  area_l <- as.numeric(units::set_units(sf::st_area(bound), "m^2"))
  if (!is.finite(area_l) || area_l <= 0 || area_l + max(1, area_l) * 1e-8 < sum(areas)) {
    stop("Area do limite deve ser positiva e nao inferior a area de habitat.")
  }
  distance <- matrix(as.numeric(units::set_units(sf::st_distance(p), "m")), nrow(p), nrow(p))
  dimnames(distance) <- list(as.character(p$fragment_id), as.character(p$fragment_id))
  scenarios <- lapply(max_dist, function(md) {
    adjacency <- distance < md
    diag(adjacency) <- FALSE
    graph <- igraph::graph_from_adjacency_matrix(adjacency * 1, mode = "undirected", diag = FALSE)
    comp <- igraph::components(graph)$membership
    inclusive <- distance <= md
    diag(inclusive) <- FALSE
    clusters <- igraph::components(igraph::graph_from_adjacency_matrix(inclusive * 1,
      mode = "undirected", diag = FALSE))$membership
    pp <- p
    pp$component <- as.integer(comp)
    pp$cluster_lconnect <- as.integer(clusters)
    pp$clusters <- as.integer(clusters)
    pp$degree <- as.numeric(igraph::degree(graph))
    pp$iic_included <- rowSums(distance > 0 & distance < md) > 0
    pp$max_dist_m <- md
    # Keep the native landscape schema so plot.lconnect draws one cluster map.
    # IDs remain available explicitly without becoming extra native map panels.
    land <- structure(list(landscape = pp[, "clusters", drop = FALSE],
      fragment_id = pp$fragment_id, max_dist = md, clusters = as.integer(clusters),
      distance = stats::as.dist(distance), boundary = bound,
      area_l = units::set_units(area_l, "m^2")), class = "lconnect")
    importance <- lapply(metric_patch, function(metric) {
      values <- .patch_imp_engine(distance, areas, md, area_l, metric)
      out <- pp
      out$attributes <- values
      status <- if (all(is.nan(values))) "undefined_baseline" else "ok"
      structure(list(landscape = out, prioritization = values), class = "pimp",
        metric = metric, status = status)
    })
    names(importance) <- metric_patch
    for (metric in metric_patch) pp[[paste0("d", metric)]] <- importance[[metric]]$prioritization
    graph <- igraph::set_vertex_attr(graph, "name", value = as.character(pp$fragment_id))
    for (field in setdiff(names(sf::st_drop_geometry(pp)), "geometry")) {
      graph <- igraph::set_vertex_attr(graph, field, value = pp[[field]])
    }
    edge_ids <- which(upper.tri(distance) & adjacency, arr.ind = TRUE)
    edges <- data.frame(from = as.character(pp$fragment_id[edge_ids[, 1]]),
      to = as.character(pp$fragment_id[edge_ids[, 2]]), distance_m = distance[edge_ids],
      max_dist_m = rep(md, nrow(edge_ids)))
    ge <- igraph::as_edgelist(graph, names = TRUE)
    graph <- igraph::set_edge_attr(graph, "distance_m", value = if (nrow(ge)) distance[
      cbind(match(ge[, 1], rownames(distance)), match(ge[, 2], colnames(distance)))] else numeric())
    graph <- igraph::set_graph_attr(graph, "max_dist_m", md)
    graph <- igraph::set_graph_attr(graph, "analysis_crs_wkt", analysis_crs$wkt)
    graph <- igraph::set_graph_attr(graph, "edge_rule", "distance < max_dist; isolates retained")
    metrics <- as.data.frame(as.list(.lconnect_metrics_fast(distance, areas, md, area_l)))
    metrics$max_dist_m <- md
    metrics$graph_components <- igraph::components(graph)$no
    metrics$isolates <- sum(pp$degree == 0)
    list(land = land, graph = graph, patch_importance = importance,
      patches = pp, edges = edges, metrics = metrics)
  })
  names(scenarios) <- as.character(max_dist)
  patches <- do.call(rbind, lapply(scenarios, `[[`, "patches"))
  edges <- do.call(rbind, lapply(scenarios, `[[`, "edges"))
  metrics <- do.call(rbind, lapply(scenarios, `[[`, "metrics"))
  rownames(metrics) <- rownames(edges) <- rownames(patches) <- NULL
  plots <- if (build_plot) .connectivity_plots(patches, edges, dictionary, language, metric_patch)
    else list(plot = NULL, cluster_plot = NULL, importance_plot = NULL, range_plot = NULL)
  plots$range_plot <- if (build_plot) .connectivity_range_plot(metrics, language) else NULL
  metadata <- list(schema_version = "1.0", package_version = as.character(utils::packageVersion("biomastats")),
    collection = collection, classes = classes, year = if ("year" %in% names(p)) unique(p$year) else year,
    max_dist_m = max_dist, metric_patch = metric_patch, source_crs_wkt = original_crs$wkt,
    analysis_crs_wkt = analysis_crs$wkt, reprojected = !isTRUE(original_crs == analysis_crs),
    boundary_method = boundary_method, landscape_area_m2 = area_l, habitat_area_m2 = sum(areas),
    area_unit = "m2", distance_unit = "m", awf_unit = "m4", compatibility = "lconnect 0.1.2",
    graph_rule = "distance < max_dist; zero-distance links and isolated vertices retained",
    components_rule = "lconnect NC and area components: distance <= max_dist",
    iic_rule = "0 < distance < max_dist; isolated vertices omitted; fixed landscape denominator",
    awf_rule = "exponential all-pairs kernel, diagonal zero; half-probability at max_dist/2",
    zero_baseline = "priority NaN; pimp status undefined_baseline; CPL NA without connected pairs",
    upstream = inherited)
  structure(c(list(source_polygons = source, analysis_polygons = p, boundary = bound,
    distance = distance, metrics = metrics, patch_metrics = patches, edge_metrics = edges,
    scenarios = scenarios, class_dictionary = dictionary, metadata = metadata), plots),
    class = c("biomastats_connectivity", "list"))
}

.connectivity_select_year <- function(polygons, year) {
  if (!is.null(year) && (!is.numeric(year) || length(year) != 1L || !is.finite(year) || year != trunc(year))) {
    stop("year deve ser um ano inteiro.")
  }
  if (!inherits(polygons, "sf")) {
    if (!is.list(polygons) || !length(polygons) ||
        !all(vapply(polygons, inherits, logical(1), what = "sf"))) stop("polygons deve ser sf ou lista temporal de sf.")
    if (is.null(year)) {
      if (length(polygons) != 1L) stop("Selecione year para uma lista com varios anos.")
      polygons <- polygons[[1]]
    } else {
      if (is.null(names(polygons)) || !as.character(year) %in% names(polygons)) stop("year indisponivel na lista.")
      polygons <- polygons[[as.character(year)]]
    }
  }
  if ("year" %in% names(polygons) && nrow(polygons)) {
    available <- unique(stats::na.omit(polygons$year))
    if (is.null(year) && length(available) > 1L) stop("Selecione year para poligonos de varios anos.")
    if (!is.null(year)) {
      if (!year %in% available) stop("year indisponivel nos poligonos.")
      polygons <- polygons[!is.na(polygons$year) & polygons$year == year, , drop = FALSE]
    }
  }
  polygons
}

.connectivity_crs <- function(polygons, crs) {
  target <- if (is.null(crs)) sf::st_crs(polygons) else sf::st_crs(crs)
  if (is.na(target)) stop("crs deve ser valido.")
  if (!sf::st_is_longlat(target) && .connectivity_metres(target)) return(target)
  if (!is.null(crs)) stop("crs de analise deve ser projetado em metros.")
  bb <- sf::st_bbox(sf::st_transform(polygons, 4326))
  lon <- mean(bb[c("xmin", "xmax")]); lat <- mean(bb[c("ymin", "ymax")])
  if (bb["xmax"] - bb["xmin"] > 6 || bb["ymin"] < -80 || bb["ymax"] > 84) {
    stop("UTM automatica exige paisagem local; informe crs projetado em metros.")
  }
  zone <- max(1, min(60, floor((lon + 180) / 6) + 1))
  sf::st_crs(if (lat >= 0) 32600 + zone else 32700 + zone)
}

.connectivity_plots <- function(patches, edges, dictionary, language, metric_patch) {
  tr <- function(pt, en) if (language == "pt") pt else en
  p <- patches
  # Geometry may represent a merged selection: do not invent a single class.
  p$class_id <- if ("code" %in% names(p)) as.character(p$code) else NA_character_
  p$class_id[is.na(p$class_id)] <- "habitat"
  codes <- unique(p$class_id)
  colors <- stats::setNames(rep("#999999", length(codes)), codes)
  labels <- stats::setNames(rep(tr("Habitat selecionado", "Selected habitat"), length(codes)), codes)
  if (!is.null(dictionary)) {
    ids <- match(codes, as.character(dictionary$code))
    known <- !is.na(ids)
    colors[known] <- dictionary$color[ids[known]]
    labels[known] <- paste(codes[known], if (language == "pt") dictionary$classe[ids[known]] else dictionary$class[ids[known]])
  }
  labels <- vapply(labels, function(x) paste(strwrap(x, 32), collapse = "\n"), character(1))
  p$scenario <- paste0(p$max_dist_m, " m")
  p$scenario <- factor(p$scenario, levels = paste0(sort(unique(p$max_dist_m)), " m"))
  points <- suppressWarnings(sf::st_point_on_surface(p))
  coords <- sf::st_coordinates(points)
  p$x <- coords[, 1]; p$y <- coords[, 2]
  p$status <- ifelse(p$degree == 0, tr("Isolado", "Isolated"), tr("Conectado", "Connected"))
  nodes <- sf::st_drop_geometry(p)
  e <- edges
  key <- paste(p$max_dist_m, p$fragment_id, sep = ":")
  a <- match(paste(e$max_dist_m, e$from, sep = ":"), key)
  b <- match(paste(e$max_dist_m, e$to, sep = ":"), key)
  e$x <- p$x[a]; e$y <- p$y[a]; e$xend <- p$x[b]; e$yend <- p$y[b]
  e$scenario <- factor(paste0(e$max_dist_m, " m"), levels = levels(p$scenario))
  graph <- ggplot2::ggplot() + ggplot2::geom_sf(data = p,
    ggplot2::aes(fill = class_id), colour = "#555555", linewidth = 0.2) +
    ggplot2::geom_segment(data = e, ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
      colour = "#5a6470", linewidth = 0.4, alpha = 0.7) +
    ggplot2::geom_point(data = nodes, ggplot2::aes(x = x, y = y, size = area_ha, shape = status),
      colour = "#202020", fill = "white") +
    ggplot2::scale_fill_manual(values = colors, breaks = names(colors), labels = labels,
      name = tr("Habitat (c\u00f3digo)", "Habitat (code)")) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(21, 4),
      c(tr("Conectado", "Connected"), tr("Isolado", "Isolated"))), name = tr("Situa\u00e7\u00e3o", "Status")) +
    ggplot2::scale_size_continuous(range = c(1.5, 6), name = tr("\u00c1rea (ha)", "Area (ha)")) +
    ggplot2::facet_wrap(~scenario) + ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 12)
  p$component_id <- factor(p$component)
  clusters <- ggplot2::ggplot(p) + ggplot2::geom_sf(ggplot2::aes(fill = component_id),
    colour = "#555555", linewidth = 0.2) + ggplot2::facet_wrap(~scenario) +
    ggplot2::labs(fill = tr("Componente do grafo", "Graph component"), x = NULL, y = NULL) +
    ggplot2::coord_sf(datum = NA) + ggplot2::theme_minimal(base_size = 12)
  imp <- lapply(metric_patch, function(metric) {
    out <- p
    out$metric <- paste0("d", metric, " (%)")
    out$importance <- out[[paste0("d", metric)]]
    out
  })
  imp <- do.call(rbind, imp)
  importance <- ggplot2::ggplot(imp) + ggplot2::geom_sf(ggplot2::aes(fill = importance),
    colour = "#555555", linewidth = 0.2) + ggplot2::scale_fill_gradient(low = "#f3f6f1", high = "#125c40",
      name = tr("Redu\u00e7\u00e3o (%)", "Reduction (%)")) + ggplot2::facet_grid(metric ~ scenario) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::coord_sf(datum = NA) + ggplot2::theme_minimal(base_size = 12)
  list(plot = graph, cluster_plot = clusters, importance_plot = importance)
}

.connectivity_range_plot <- function(metrics, language) {
  fields <- c("NC", "LNK", "IIC", "AWF", "graph_components", "isolates")
  rows <- lapply(fields, function(field) data.frame(max_dist_m = metrics$max_dist_m,
    metric = field, value = metrics[[field]]))
  out <- do.call(rbind, rows)
  plot <- ggplot2::ggplot(out, ggplot2::aes(x = max_dist_m, y = value)) +
    ggplot2::geom_point(colour = "#245b78", size = 2) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(x = if (language == "pt") "Dist\u00e2ncia de conex\u00e3o (m)" else "Connection distance (m)",
      y = if (language == "pt") "Valor do \u00edndice" else "Index value") + ggplot2::theme_minimal(base_size = 12)
  if (nrow(metrics) > 1L) plot <- plot + ggplot2::geom_line(colour = "#245b78")
  plot
}

utils::globalVariables(c("area_ha", "class_id", "status", "scenario", "component_id",
                        "importance", "max_dist_m", "metric", "value"))
