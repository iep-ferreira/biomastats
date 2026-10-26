#' Conectividade de fragmentos vetorizados e interoperabilidade com lconnect
#'
#' Integra diretamente os poligonos sf de [reclass_to_polygons()] ao fluxo de
#' lconnect, usando as rotinas otimizadas do biomastats. Nao procura arquivos,
#' nao grava shapefiles e nao recalcula distancias nas remocoes de fragmentos.
#' @param polygons Objeto sf de reclass_to_polygons, inclusive apos readRDS,
#'   ou sua lista temporal nomeada. Coluna value, quando presente, seleciona
#'   somente presenca (1). Fragmentos devem ser POLYGON/MULTIPOLYGON validos e sem sobreposicao
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
#' @param metric_patch Indices de priorizacao: IIC, PC e/ou AWF; padrao IIC e
#'   AWF. O PC e o mais custoso (poda n^3 e caminhos ponderados).
#' @param iic_rule "definition" (padrao) ou "lconnect"; ver [patch_imp_fast()].
#' @param k,p_ref,d_ref Calibracao de k para PC e AWF, como em
#'   [patch_imp_fast()]. Sem k nem d_ref, cada limiar usa p = 0,5 a max_dist / 2.
#' @param parallel FALSE (padrao), TRUE (plano future do usuario) ou numero de
#'   processos; ver [patch_imp_fast()]. Com um numero, os processos sao abertos
#'   uma unica vez para todos os limiares e indices e fechados ao final.
#' @param prune Poda das arestas do PC: "auto" (padrao), TRUE ou FALSE; ver
#'   [patch_imp_fast()].
#' @param distance Distancias externas entre fragmentos; NULL (padrao) usa a
#'   distancia minima entre bordas, em metros. Aceita matriz quadrada (com
#'   nomes iguais a fragment_id ou na ordem dos fragmentos), objeto dist ou
#'   tabela com colunas From, To e Distance, como as de
#'   `Makurhini::distancefile()`, e o resultado de [cost_distance()]. NA e Inf
#'   significam ausencia de ligacao (barreira). max_dist, d_ref e as colunas
#'   terminadas em _m passam a usar a unidade dessas distancias.
#' @param link_importance Calcular a importancia de cada ligacao para IIC e PC
#'   (FALSE por padrao); ver link_metrics em Value.
#' @param language Idioma dos rotulos: pt (padrao) ou en; dicionario preserva ambos.
#' @param build_plot Construir ggplots (TRUE por padrao).
#' @return Lista biomastats_connectivity com source_polygons, analysis_polygons,
#'   boundary, distance (matriz em metros ou na unidade externa), metrics
#'   (data.frame por limiar, com ECA, fracoes da paisagem e indicadores no
#'   formato do ProtConn),
#'   patch_metrics (sf por fragmento/limiar, com dIIC, dPC, dAWF e suas fracoes
#'   dX_intra, dX_flux e dX_connector), fraction_metrics (data.frame longo das
#'   fracoes por fragmento, limiar e indice), edge_metrics (data.frame),
#'   link_metrics (importancia das ligacoes, ou NULL),
#'   scenarios (lista nomeada por limiar, cada item com land de classe lconnect,
#'   graph igraph com todos os fragmentos e patch_importance com objetos pimp),
#'   class_dictionary, metadata, plot (grafo espacial ggplot), cluster_plot,
#'   importance_plot, fractions_plot, connector_plot, range_plot e link_plot
#'   (NULL sem link_importance). Todos sao reutilizaveis apos saveRDS/readRDS.
#' @details Distancias sao minimas entre bordas de poligonos, nao entre centroides.
#'   NC, LNK, SLC, MSC, CCP, LCP, CPL, ECS e AWF reproduzem as convencoes de
#'   lconnect 0.1.2 com rotinas nativas otimizadas. IIC segue a definicao de
#'   Pascual-Hortal & Saura (2006), que inclui os fragmentos isolados; o valor do
#'   lconnect, que os omite, fica em IIC_lconnect. PC (Saura & Pascual-Hortal 2007)
#'   usa a probabilidade maxima de caminho e so e calculado quando pedido em
#'   metric_patch (NA caso contrario). Areas SLC, MSC e ECS sao m2; AWF e m4; CPL e
#'   numero de passos; IIC, PC, CCP e LCP sao adimensionais.
#'
#'   O grafo de exploracao, o IIC e o CPL usam distancia < max_dist, incluindo
#'   contatos a distancia zero, e preservam isolados. Para compatibilidade, NC e
#'   indices de componentes usam distancia <= max_dist (single linkage de
#'   lconnect). O ideal e dissolver contatos antes, com
#'   reclass_to_polygons(directions = 8). Em empates no limiar, NC pode diferir
#'   de graph_components e componentes do grafo. patch_metrics distingue
#'   cluster_lconnect, component e iic_included (todos os fragmentos na regra
#'   "definition"; somente os com ligacao a d > 0 na regra "lconnect").
#'
#'   A importancia e decomposta segundo Saura & Rubio (2010) em intra (area do
#'   proprio fragmento), fluxo (ligacao com os demais) e conector (papel de
#'   trampolim nos melhores caminhos entre outros fragmentos). No AWF tudo e
#'   fluxo. IIC e PC mantem o denominador area_l fixo na remocao de fragmentos.
#'   AWF nao corta pares pelo limiar; max_dist controla seu decaimento
#'   exponencial. Em indices inicialmente zero, a priorizacao e indefinida (NaN),
#'   com status "undefined_baseline" no objeto pimp. Um fragmento nao tem ranking
#'   relativo e recebe o mesmo status. CPL sem pares conectados e NA. Nenhum
#'   indice prova conectividade funcional.
#'
#'   A area conectada equivalente (Saura et al. 2011) e ECA_IIC = A_L sqrt(IIC)
#'   e ECA_PC = A_L sqrt(PC), em m2; ECAnorm_IIC e ECAnorm_PC a expressam em %
#'   da area de habitat. As fracoes da paisagem seguem a saida overall do
#'   Makurhini, em % do numerador: X_intra (soma de a_i^2), X_direct (pares
#'   ligados diretamente: um passo no IIC, probabilidade direta p_ij no PC) e
#'   X_step (ganho por trampolins). Com o PC, metrics traz tambem os indicadores
#'   do ProtConn (Saura et al. 2017) tomando o habitat como area protegida e o
#'   limite como regiao: Prot = 100 habitat / A_L, ProtConn = 100 ECA_PC / A_L,
#'   ProtUnconn = Prot - ProtConn e RelConn = 100 ProtConn / Prot. Para areas
#'   protegidas reais e fracoes Within, Contig, Trans e Unprot, use
#'   [as_makurhini()] com target = "ProtConn".
#'
#'   link_metrics traz, por limiar e indice (IIC, PC), a reducao percentual do
#'   numerador quando apenas aquela ligacao e removida. So uma ligacao que e
#'   ela propria o melhor caminho entre suas pontas pode importar; as demais
#'   (inclusive as podadas do PC) valem zero e nao sao listadas. Ligacoes com
#'   caminho alternativo de mesmo custo tambem valem zero. link_plot mapeia
#'   essas importancias.
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
#'   continua da reducao percentual. fractions_plot empilha intra, fluxo e
#'   conector dos 15 fragmentos mais importantes por indice e limiar;
#'   connector_plot mapeia a fracao de conector (IIC e PC).
#'   Todos aceitam labs, temas e escalas ggplot2;
#'   use ggsave/st_write/write.csv/write_graph/saveRDS nos retornos para escrita.
#'   O pacote lconnect e opcional: os objetos land/pimp sao construidos em memoria
#'   com seu contrato e podem ser consumidos pelas suas funcoes quando instalado.
#'   A matriz de distancias e os calculos de caminhos requerem memoria quadratica.
#' @references Pascual-Hortal, L. & Saura, S. (2006) Landscape Ecology 21:
#'   959-967. Saura, S. & Pascual-Hortal, L. (2007) Landscape and Urban
#'   Planning 83: 91-103. Saura, S. & Rubio, L. (2010) Ecography 33: 523-537.
#'   Saura, S. et al. (2011) Ecological Indicators 11: 407-416. Saura, S. et
#'   al. (2017) Ecological Indicators 76: 144-158. Godinez-Gomez, O. &
#'   Correa Ayram, C. A. (2020) Makurhini, <https://github.com/connectscape/Makurhini>.
#'   [lconnect 0.1.2](https://cran.r-project.org/package=lconnect).
#' @md
#' @examples
#' \dontrun{
#' poligonos <- reclass_to_polygons(metricas, save_shp = FALSE)
#' conectividade <- land_connectivity(poligonos, max_dist = c(30, 100, 300),
#'   metric_patch = c("IIC", "PC", "AWF"))
#' conectividade$plot + ggplot2::labs(title = "Conectividade dos fragmentos")
#' conectividade$metrics
#' conectividade$fractions_plot
#' lconnect::con_metric(conectividade$scenarios[[1]]$land, c("NC", "LNK", "IIC"))
#' }
#' @export
land_connectivity <- function(polygons, max_dist = 300, year = NULL, boundary = NULL,
                              crs = NULL, metric_patch = c("IIC", "AWF"),
                              iic_rule = c("definition", "lconnect"),
                              k = NULL, p_ref = 0.5, d_ref = NULL, parallel = FALSE,
                              prune = "auto", distance = NULL, link_importance = FALSE,
                              language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  iic_rule <- match.arg(iic_rule)
  .connectivity_check_flag(build_plot, "build_plot")
  .connectivity_check_flag(link_importance, "link_importance")
  external <- distance
  .connectivity_check_parallel(parallel)
  invisible(.resolve_prune(prune, "IIC", matrix(0)))  # validates prune early
  if (!is.numeric(max_dist) || !length(max_dist) || any(!is.finite(max_dist) | max_dist <= 0)) {
    stop("max_dist deve conter distancias positivas em metros.")
  }
  max_dist <- sort(unique(max_dist))
  if (!is.character(metric_patch) || !length(metric_patch) || anyNA(metric_patch) ||
      any(!metric_patch %in% c("IIC", "PC", "AWF"))) {
    stop("metric_patch deve conter IIC, PC e/ou AWF.")
  }
  metric_patch <- unique(metric_patch)
  # Validate the k calibration before any geometric work.
  invisible(.calibrate_k(max_dist[1], k, p_ref, d_ref))
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
      any(!as.character(sf::st_geometry_type(polygons)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("Fragmentos devem ser POLYGON/MULTIPOLYGON validos e nao vazios; use reclass_to_polygons.")
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
  if (is.null(external)) {
    distance <- matrix(as.numeric(units::set_units(sf::st_distance(p), "m")), nrow(p), nrow(p))
    dimnames(distance) <- list(as.character(p$fragment_id), as.character(p$fragment_id))
    distance_source <- "edge-to-edge (sf::st_distance)"
    distance_unit <- "m"
  } else {
    distance <- .connectivity_distance(external, p$fragment_id)
    distance_source <- if (!is.null(attr(external, "method"))) attr(external, "method") else "external"
    distance_unit <- if (!is.null(attr(distance, "unit"))) attr(distance, "unit") else "external"
    attr(distance, "unit") <- NULL
  }
  # A number of processes opens one temporary plan for every threshold and
  # index (opening costs about a second); the user's plan is restored.
  if (is.numeric(parallel)) {
    old_plan <- future::plan(future::multisession, workers = parallel)
    on.exit(future::plan(old_plan), add = TRUE)
  }
  workers <- if (isFALSE(parallel)) 1L else future::nbrOfWorkers()
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
    pp$iic_included <- if (iic_rule == "definition") rep(TRUE, nrow(pp))
      else rowSums(distance > 0 & distance < md) > 0
    pp$max_dist_m <- md
    k_md <- .calibrate_k(md, k, p_ref, d_ref)
    # Keep the native landscape schema so plot.lconnect draws one cluster map.
    # IDs remain available explicitly without becoming extra native map panels.
    land <- structure(list(landscape = pp[, "clusters", drop = FALSE],
      fragment_id = pp$fragment_id, max_dist = md, clusters = as.integer(clusters),
      distance = stats::as.dist(distance), boundary = bound,
      area_l = units::set_units(area_l, "m^2")), class = "lconnect")
    importance <- lapply(metric_patch, function(metric) {
      res <- .patch_imp_engine(unname(distance), areas, md, area_l, metric, !isFALSE(parallel),
        "decomposition", iic_rule, if (metric == "IIC") NULL else k_md, prune)
      .as_pimp(list(landscape = pp), res, metric, iic_rule)
    })
    names(importance) <- metric_patch
    for (metric in metric_patch) {
      imp <- importance[[metric]]
      pp[[paste0("d", metric)]] <- imp$prioritization
      for (part in c("intra", "flux", "connector")) {
        value <- if (is.null(imp$fractions)) NA_real_ else imp$fractions[[part]]
        pp[[paste0("d", metric, "_", part)]] <- value
      }
    }
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
    pc <- if ("PC" %in% metric_patch) attr(importance$PC, "index") else NA_real_
    pc_landscape <- if ("PC" %in% metric_patch) attr(importance$PC, "landscape_fractions")
    metrics <- as.data.frame(as.list(.connectivity_indices(unname(distance), areas, md, area_l,
      k = k_md, pc = pc, pc_landscape = pc_landscape)))
    links <- if (link_importance) {
      do.call(rbind, lapply(intersect(metric_patch, c("IIC", "PC")), function(metric) {
        D <- unname(distance)
        out <- .link_importance(D, areas, md, metric, if (metric == "IIC") NULL else k_md,
          !isFALSE(parallel), .resolve_prune(prune, metric, D))
        data.frame(from = as.character(pp$fragment_id[out$from]),
          to = as.character(pp$fragment_id[out$to]), distance_m = D[cbind(out$from, out$to)],
          max_dist_m = rep(md, nrow(out)), metric = rep(metric, nrow(out)),
          importance = out$importance)
      }))
    }
    metrics$max_dist_m <- md
    metrics$k <- k_md
    metrics$pc_pruned <- if ("PC" %in% metric_patch) attr(importance$PC, "pruned") else NA
    metrics$pc_edges <- if ("PC" %in% metric_patch) attr(importance$PC, "edges") else NA_integer_
    metrics$graph_components <- igraph::components(graph)$no
    metrics$isolates <- sum(pp$degree == 0)
    list(land = land, graph = graph, patch_importance = importance,
      patches = pp, edges = edges, metrics = metrics, links = links)
  })
  names(scenarios) <- as.character(max_dist)
  patches <- do.call(rbind, lapply(scenarios, `[[`, "patches"))
  edges <- do.call(rbind, lapply(scenarios, `[[`, "edges"))
  metrics <- do.call(rbind, lapply(scenarios, `[[`, "metrics"))
  links <- if (link_importance) do.call(rbind, lapply(scenarios, `[[`, "links"))
  for (i in seq_along(scenarios)) scenarios[[i]]$links <- NULL
  rownames(metrics) <- rownames(edges) <- rownames(patches) <- NULL
  if (!is.null(links)) rownames(links) <- NULL
  fractions <- .connectivity_fractions(patches, metric_patch)
  plots <- if (build_plot) .connectivity_plots(patches, edges, dictionary, language, metric_patch)
    else list(plot = NULL, cluster_plot = NULL, importance_plot = NULL, connector_plot = NULL)
  plots$fractions_plot <- if (build_plot) .connectivity_fractions_plot(fractions, language)
  plots$range_plot <- if (build_plot) .connectivity_range_plot(metrics, language)
  plots$link_plot <- if (build_plot && !is.null(links) && nrow(links)) {
    .connectivity_link_plot(patches, links, language)
  }
  iic_definition <- paste("Pascual-Hortal & Saura 2006: all patches incl. isolates and i = j;",
    "distance < max_dist, zero-distance contacts linked; fixed landscape denominator")
  iic_lconnect <- paste("lconnect 0.1.2: 0 < distance < max_dist; isolated vertices omitted;",
    "fixed landscape denominator")
  # Schema 1.3: external distances, ECA, landscape fractions, ProtConn
  # indicators and link importance (1.2: optional PC pruning and parallel
  # blocks; 1.1: IIC by definition, PC, fractions and k calibration; 1.0:
  # lconnect IIC).
  metadata <- list(schema_version = "1.3",
    package_version = as.character(utils::packageVersion("biomastats")),
    collection = collection, classes = classes, year = if ("year" %in% names(p)) unique(p$year) else year,
    max_dist_m = max_dist, metric_patch = metric_patch, source_crs_wkt = original_crs$wkt,
    analysis_crs_wkt = analysis_crs$wkt, reprojected = !isTRUE(original_crs == analysis_crs),
    boundary_method = boundary_method, landscape_area_m2 = area_l, habitat_area_m2 = sum(areas),
    area_unit = "m2", distance_unit = distance_unit, distance_source = distance_source,
    awf_unit = "m4", eca_unit = "m2",
    landscape_fractions = paste("Saura et al. 2011 / Makurhini overall: intra, direct",
      "(one step; PC direct p_ij) and step (stepping stones), % of the numerator"),
    protconn_rule = paste("Saura et al. 2017 with habitat as protected area and boundary",
      "as region: ProtConn = 100 ECA_PC / A_L"),
    link_importance = link_importance,
    compatibility = "lconnect 0.1.2: NC, LNK, SLC, MSC, CCP, LCP, CPL, ECS, AWF and IIC_lconnect",
    graph_rule = "distance < max_dist; zero-distance links and isolated vertices retained",
    components_rule = "lconnect NC and area components: distance <= max_dist",
    iic_rule = if (iic_rule == "definition") iic_definition else iic_lconnect,
    pc_rule = paste("Saura & Pascual-Hortal 2007: maximum-product path probability",
      "exp(-k d*), all patches incl. i = j"),
    awf_rule = "exponential all-pairs kernel, diagonal zero",
    k_calibration = if (!is.null(k)) list(k = k) else list(p_ref = p_ref,
      d_ref = if (is.null(d_ref)) "max_dist / 2" else d_ref),
    importance_method = "Saura & Rubio 2010 decomposition: intra + flux + connector",
    pc_prune = if (is.logical(prune)) prune else "auto: >= 50 patches and >= 25% finite links",
    parallel_workers = workers,
    zero_baseline = "priority NaN; pimp status undefined_baseline; CPL NA without connected pairs",
    upstream = inherited)
  structure(c(list(source_polygons = source, analysis_polygons = p, boundary = bound,
    distance = distance, metrics = metrics, patch_metrics = patches,
    fraction_metrics = fractions, edge_metrics = edges, link_metrics = links,
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
  p <- patches
  # Geometry may represent a merged selection: do not invent a single class.
  p$class_id <- if ("code" %in% names(p)) as.character(p$code) else NA_character_
  p$class_id[is.na(p$class_id)] <- "habitat"
  codes <- unique(p$class_id)
  colors <- stats::setNames(rep("#999999", length(codes)), codes)
  labels <- stats::setNames(rep(.tr(language, "Habitat selecionado", "Selected habitat"),
                                length(codes)), codes)
  if (!is.null(dictionary)) {
    ids <- match(codes, as.character(dictionary$code))
    known <- !is.na(ids)
    colors[known] <- dictionary$color[ids[known]]
    labels[known] <- paste(codes[known],
      .tr(language, dictionary$classe[ids[known]], dictionary$class[ids[known]]))
  }
  labels <- vapply(labels, function(x) paste(strwrap(x, 32), collapse = "\n"), character(1))
  p$scenario <- paste0(p$max_dist_m, " m")
  p$scenario <- factor(p$scenario, levels = paste0(sort(unique(p$max_dist_m)), " m"))
  points <- suppressWarnings(sf::st_point_on_surface(p))
  coords <- sf::st_coordinates(points)
  p$x <- coords[, 1]; p$y <- coords[, 2]
  connected <- .tr(language, "Conectado", "Connected")
  isolated <- .tr(language, "Isolado", "Isolated")
  p$status <- ifelse(p$degree == 0, isolated, connected)
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
      name = .tr(language, "Habitat (c\u00f3digo)", "Habitat (code)")) +
    ggplot2::scale_shape_manual(values = stats::setNames(c(21, 4), c(connected, isolated)),
      name = .tr(language, "Situa\u00e7\u00e3o", "Status")) +
    ggplot2::scale_size_continuous(range = c(1.5, 6),
      name = .tr(language, "\u00c1rea (ha)", "Area (ha)")) +
    ggplot2::facet_wrap(~scenario) + ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 12)
  p$component_id <- factor(p$component)
  clusters <- ggplot2::ggplot(p) + ggplot2::geom_sf(ggplot2::aes(fill = component_id),
    colour = "#555555", linewidth = 0.2) + ggplot2::facet_wrap(~scenario) +
    ggplot2::labs(fill = .tr(language, "Componente do grafo", "Graph component"),
      x = NULL, y = NULL) +
    ggplot2::coord_sf(datum = NA) + ggplot2::theme_minimal(base_size = 12)
  importance <- .connectivity_map(p, metric_patch, "",
    .tr(language, "Redu\u00e7\u00e3o (%)", "Reduction (%)"), "#f3f6f1", "#125c40")
  path_metrics <- intersect(metric_patch, c("IIC", "PC"))
  connector <- if (length(path_metrics)) {
    .connectivity_map(p, path_metrics, "_connector", .tr(language, "Conector (%)", "Connector (%)"),
                      "#f6f3ee", "#9c5a12", .tr(language, " conector", " connector"))
  } else NULL
  list(plot = graph, cluster_plot = clusters, importance_plot = importance,
       connector_plot = connector)
}

# Map of one patch_metrics column per index ("dIIC", "dIIC_connector", ...),
# faceted by index and threshold.
.connectivity_map <- function(p, metrics, suffix, legend, low, high, label = "") {
  data <- do.call(rbind, lapply(metrics, function(metric) {
    out <- p
    out$metric <- paste0("d", metric, label, " (%)")
    out$importance <- out[[paste0("d", metric, suffix)]]
    out
  }))
  ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = importance), colour = "#555555", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = low, high = high, name = legend) +
    ggplot2::facet_grid(metric ~ scenario) + ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::coord_sf(datum = NA) + ggplot2::theme_minimal(base_size = 12)
}

# Saura & Rubio (2010) fractions in long format: one row per patch,
# threshold, index and fraction.
.connectivity_fractions <- function(patches, metric_patch) {
  p <- sf::st_drop_geometry(patches)
  parts <- c("intra", "flux", "connector")
  rows <- lapply(metric_patch, function(metric) {
    do.call(rbind, lapply(parts, function(part) data.frame(
      fragment_id = p$fragment_id, max_dist_m = p$max_dist_m, metric = metric, fraction = part,
      value = p[[paste0("d", metric, "_", part)]], importance = p[[paste0("d", metric)]])))
  })
  out <- do.call(rbind, rows)
  out$fraction <- factor(out$fraction, levels = parts)
  rownames(out) <- NULL
  out
}

# Stacked fractions of the `top` most important patches per index and threshold.
.connectivity_fractions_plot <- function(fractions, language, top = 15L) {
  f <- fractions[is.finite(fractions$value) & is.finite(fractions$importance), , drop = FALSE]
  if (!nrow(f)) return(NULL)
  keep <- unlist(lapply(split(seq_len(nrow(f)), paste(f$metric, f$max_dist_m)), function(i) {
    ranked <- unique(f$fragment_id[i][order(-f$importance[i])])
    i[f$fragment_id[i] %in% utils::head(ranked, top)]
  }))
  f <- f[keep, , drop = FALSE]
  thresholds <- paste0(sort(unique(f$max_dist_m)), " m")
  f$scenario <- factor(paste0(f$max_dist_m, " m"), levels = thresholds)
  f$panel <- paste0("d", f$metric)
  # One bar per panel and patch, ordered by importance inside each panel.
  f$key <- paste(f$panel, f$scenario, f$fragment_id, sep = "|")
  order_key <- unique(f$key[order(f$panel, f$scenario, f$importance)])
  f$key <- factor(f$key, levels = order_key)
  ggplot2::ggplot(f, ggplot2::aes(x = key, y = value, fill = fraction)) +
    ggplot2::geom_col(width = 0.8) + ggplot2::coord_flip() +
    ggplot2::scale_x_discrete(labels = stats::setNames(sub(".*\\|", "", order_key), order_key)) +
    ggplot2::scale_fill_manual(
      values = c(intra = "#2e74b5", flux = "#5b8c2d", connector = "#d08c2d"),
      labels = c(intra = "intra", flux = .tr(language, "fluxo", "flux"),
                 connector = .tr(language, "conector", "connector")),
      name = .tr(language, "Fra\u00e7\u00e3o", "Fraction")) +
    ggplot2::facet_wrap(panel ~ scenario, scales = "free") +
    ggplot2::labs(x = .tr(language, "Fragmento", "Patch"),
                  y = .tr(language, "Redu\u00e7\u00e3o do \u00edndice (%)", "Index reduction (%)")) +
    ggplot2::theme_minimal(base_size = 11)
}

.connectivity_range_plot <- function(metrics, language) {
  fields <- c("NC", "LNK", "IIC", if (any(is.finite(metrics$PC))) "PC", "AWF",
              "graph_components", "isolates")
  rows <- lapply(fields, function(field) data.frame(max_dist_m = metrics$max_dist_m,
    metric = field, value = metrics[[field]]))
  out <- do.call(rbind, rows)
  plot <- ggplot2::ggplot(out, ggplot2::aes(x = max_dist_m, y = value)) +
    ggplot2::geom_point(colour = "#245b78", size = 2) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(x = .tr(language, "Dist\u00e2ncia de conex\u00e3o (m)", "Connection distance (m)"),
      y = .tr(language, "Valor do \u00edndice", "Index value")) +
    ggplot2::theme_minimal(base_size = 12)
  if (nrow(metrics) > 1L) plot <- plot + ggplot2::geom_line(colour = "#245b78")
  plot
}

# Landscape indices for one threshold, reusing the distance matrix. NC and
# component indices follow lconnect single linkage (d <= max_dist); IIC
# follows the original definition and IIC_lconnect reproduces lconnect 0.1.2.
# PC and its landscape fractions come from the patch-importance step (NA when
# not requested). ECA = A_L sqrt(index); ProtConn indicators use ECA_PC.
.connectivity_indices <- function(D, areas, max_dist, area_l, k, pc = NA_real_,
                                  pc_landscape = NULL) {
  inclusive <- D <= max_dist
  diag(inclusive) <- FALSE
  clusters <- igraph::components(igraph::graph_from_adjacency_matrix(
    inclusive * 1, mode = "undirected", diag = FALSE))$membership
  component_areas <- as.numeric(rowsum(areas, clusters))
  paths <- .path_lengths(.path_graph(D, "IIC", max_dist), "IIC")
  connected <- paths[upper.tri(paths) & is.finite(paths)]
  M <- 1 / (1 + paths)
  numerator <- sum(areas * as.numeric(M %*% areas))
  iic_landscape <- .landscape_fractions(D, paths, M, areas, "IIC", NULL, numerator)
  if (is.null(pc_landscape)) pc_landscape <- c(intra = NA_real_, direct = NA_real_, step = NA_real_)
  habitat <- sum(areas)
  eca_iic <- sqrt(numerator)
  eca_pc <- area_l * sqrt(pc)
  c(NC = length(component_areas), LNK = sum(D[upper.tri(D)] < max_dist),
    SLC = max(component_areas), MSC = habitat / length(component_areas),
    CCP = sum((component_areas / habitat)^2), LCP = sum((component_areas / area_l)^2),
    CPL = if (length(connected)) mean(connected) else NA_real_,
    ECS = sum(component_areas^2) / habitat, AWF = .awf_value(D, areas, k),
    IIC = numerator / area_l^2,
    IIC_lconnect = .iic_lconnect(D, areas, max_dist, area_l), PC = pc,
    ECA_IIC = eca_iic, ECA_PC = eca_pc,
    ECAnorm_IIC = 100 * eca_iic / habitat, ECAnorm_PC = 100 * eca_pc / habitat,
    IIC_intra = iic_landscape[["intra"]], IIC_direct = iic_landscape[["direct"]],
    IIC_step = iic_landscape[["step"]], PC_intra = pc_landscape[["intra"]],
    PC_direct = pc_landscape[["direct"]], PC_step = pc_landscape[["step"]],
    Prot = 100 * habitat / area_l, ProtConn = 100 * eca_pc / area_l,
    ProtUnconn = 100 * (habitat - eca_pc) / area_l, RelConn = 100 * eca_pc / habitat)
}

# Map of link importance: patches in grey and links coloured by the drop of
# the index when each one alone is removed, faceted by index and threshold.
.connectivity_link_plot <- function(patches, links, language) {
  points <- suppressWarnings(sf::st_point_on_surface(patches))
  coords <- sf::st_coordinates(points)
  key <- paste(patches$max_dist_m, patches$fragment_id, sep = ":")
  a <- match(paste(links$max_dist_m, links$from, sep = ":"), key)
  b <- match(paste(links$max_dist_m, links$to, sep = ":"), key)
  e <- links
  e$x <- coords[a, 1]; e$y <- coords[a, 2]; e$xend <- coords[b, 1]; e$yend <- coords[b, 2]
  thresholds <- paste0(sort(unique(patches$max_dist_m)), " m")
  e$scenario <- factor(paste0(e$max_dist_m, " m"), levels = thresholds)
  e$panel <- paste0("d", e$metric, "_link")
  e <- e[order(e$importance), , drop = FALSE]
  p <- patches
  p$scenario <- factor(paste0(p$max_dist_m, " m"), levels = thresholds)
  p <- do.call(rbind, lapply(unique(e$panel), function(panel) { p$panel <- panel; p }))
  ggplot2::ggplot() +
    ggplot2::geom_sf(data = p, fill = "#d9d9d9", colour = "#8c8c8c", linewidth = 0.15) +
    ggplot2::geom_segment(data = e, ggplot2::aes(x = x, y = y, xend = xend, yend = yend,
      colour = importance, linewidth = importance), lineend = "round") +
    ggplot2::scale_colour_gradient(low = "#c6dbef", high = "#08306b",
      name = .tr(language, "Import\u00e2ncia da liga\u00e7\u00e3o (%)", "Link importance (%)")) +
    ggplot2::scale_linewidth_continuous(range = c(0.2, 1.6), guide = "none") +
    ggplot2::facet_grid(panel ~ scenario) + ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(x = NULL, y = NULL) + ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables(c("area_ha", "class_id", "status", "scenario", "component_id",
                        "importance", "max_dist_m", "metric", "value", "key", "fraction", "panel"))
