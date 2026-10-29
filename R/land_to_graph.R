#' Adjacencias entre classes, fronteiras e redes de paisagem
#'
#' Conta contatos entre pixels de um mapa anual e constroi uma rede ponderada
#' de classes. Usa o objeto de mapas do fluxo de [load_rasters()] e nao exige
#' reclassificacao nem vetorizacao previa.
#'
#' @param data Lista com `raster` (RasterLayer por ano), `time_range`
#'   (inicio e fim inclusivos) e `collection` (dicionario MapBiomas).
#' @param year Ano inteiro dentro de `data$time_range`. Se NULL, usa o unico
#'   ano disponivel; objetos com varios anos exigem selecao explicita.
#' @param connectivity Vizinhanca 4 (arestas) ou 8 (arestas e vertices).
#' @param include_self Incluir contatos da mesma classe na diagonal da matriz
#'   de contagens e lacos no grafo ponderado por contagens. Default FALSE.
#' @param build_plot Criar graficos ggplot2 e, se ggiraph estiver instalado,
#'   um widget interativo. FALSE calcula somente matrizes e metricas.
#' @param weight Peso da rede: `"adjacencies"` (contagens) ou `"boundary"`
#'   (comprimento aproximado de fronteiras em metros).
#' @param language Idioma dos rotulos: `"pt"` ou `"en"`. As tabelas preservam
#'   os dois idiomas do dict_build, independentemente desta opcao.
#'
#' @return Lista com `adjacency` (matriz simetrica de contagens), `boundary_m`
#'   (matriz simetrica de fronteiras em metros, diagonal zero), `graph`
#'   (igraph com nomes iguais aos codigos das classes), `plot` (ggplot),
#'   `graph_plot` (widget ggiraph ou NULL), `heatmap` (ggplot das contagens),
#'   `graph_metrics`, `node_metrics` e `edge_metrics` (data.frames).
#'   As matrizes usam codigos; node_metrics fornece nomes e cores.
#'   edge_metrics inclui contatos, comprimento, peso, fracao do peso total
#'   e intermediacao da aresta. Contatos de peso zero nao entram no grafo.
#'   O resultado tem classe `biomastats_land_graph`, e inclui `class_dictionary`
#'   (code, classe, class, grupo, group, color, known), `metadata` (parametros,
#'   CRS, resolucao, regras e versao), `edge_plot` (ranking de fronteiras em
#'   ggplot2). Os campos class/classe preservam ingles/portugues; label segue
#'   language. Todos os resultados sao retornados em memoria para reuso,
#'   personalizacao e escrita pelas ferramentas padrao do R.
#'
#' @details Cada par de pixels e contado uma vez. A matriz e espelhada para
#'   consulta; somar toda a matriz duplica contatos entre classes. NA e
#'   excluido, codigo zero e tratado como classe, e classes isoladas sao
#'   preservadas. O limite externo do raster e buracos NA nao sao fronteiras
#'   entre classes. Contatos diagonais entram nas contagens com connectivity=8,
#'   mas contribuem zero metros. Contatos da mesma classe tambem contribuem
#'   zero para boundary_m. O comprimento representa fronteiras da grade, nao
#'   o comprimento exato de feicoes do terreno. Em CRS projetado, lados dos
#'   pixels sao convertidos das unidades do CRS para metros. Em CRS geografico,
#'   distancias geodesicas dos lados sao estimadas por faixa de latitude com sf.
#'
#'   Grau, forca (strength), componentes, densidade, transitividade e
#'   centralidades sao calculados na rede sem lacos. Caminhos usam o inverso
#'   do peso como custo: mais contato significa menor custo. A centralidade
#'   harmonica normalizada trata isolados com zero; intermediacao nao e
#'   normalizada. neighbor_entropy e a entropia de Shannon (nats) dos pesos
#'   incidentes entre classes, zero para isolados. homogeneity e a proporcao
#'   de contatos incidentes da mesma classe, contando duas pontas por contato
#'   interno; e NA para pixels sem vizinhos validos. Estas duas medidas sao
#'   descritivas e nao demonstram conectividade funcional/ecologica.
#'   total_adjacencies inclui ou exclui contatos internos segundo include_self;
#'   interclass_fraction sempre usa todos os pares validos como denominador.
#'   Na rede por fronteiras, contatos apenas diagonais nao criam arestas.
#'
#'   Os graficos usam layout circular deterministico, cores do dict_build,
#'   tamanho dos nos pela forca e espessura das arestas pelo peso. A versao
#'   ggiraph oferece informacoes ao passar o mouse, selecao e zoom. Sem ggiraph,
#'   retorna os graficos estaticos e avisa que o widget nao foi construido.
#'   As metricas e matrizes sao identicas com build_plot=FALSE.
#'   Colecao, codigos presentes, CRS e resolucao sao obtidos de data. Nomes
#'   e cores vem exclusivamente de dict_build(data$collection). As legendas
#'   mostram codigo e nome, com amostras de cor de tamanho fixo.
#'   Nos usam codigos e tamanho pela forca. Titulos, temas e escalas podem
#'   ser editados nos objetos ggplot retornados, sem plot_options.
#'   Use ggplot2::ggsave para salvar os graficos editados, saveRDS/readRDS
#'   para preservar a analise, utils::write.csv para tabelas,
#'   igraph::write_graph para a rede e htmlwidgets::saveWidget para o widget.
#'   O raster anual e lido em memoria; recorte mapas extensos antes da analise.
#'
#' @references
#' [igraph: harmonic centrality](https://r.igraph.org/reference/harmonic_centrality.html)
#' e [betweenness](https://r.igraph.org/reference/betweenness.html).
#' [sf: geometric measurements](https://r-spatial.github.io/sf/reference/geos_measures.html).
#' @md
#' @examples
#' \dontrun{
#' rede <- land_to_graph(mapas, year = 1985)
#' rede$graph_plot
#' rede$heatmap
#' rede$node_metrics
#' rede$plot <- rede$plot + ggplot2::labs(title = "Rede de classes")
#' ggplot2::ggsave("rede.png", plot = rede$plot)
#' saveRDS(rede, "analise.rds")
#' fronteiras <- land_to_graph(mapas, year = 1985, weight = "boundary")
#' fronteiras$edge_metrics
#' }
#' @export
land_to_graph <- function(data, year = NULL, connectivity = 4, include_self = FALSE,
                          build_plot = TRUE, weight = c("adjacencies", "boundary"),
                          language = c("pt", "en")) {
  weight <- match.arg(weight)
  language <- match.arg(language)
  integer_vector <- function(x) is.numeric(x) && length(x) > 0L &&
    all(is.finite(x) & x == trunc(x))
  if (!is.list(data) || !all(c("raster", "time_range", "collection") %in% names(data))) {
    stop("data deve conter raster, time_range e collection.")
  }
  collection <- .biomastats_collection(data$collection)
  tr <- data$time_range
  if (!integer_vector(tr) || length(tr) != 2L || tr[1] > tr[2] ||
      !is.list(data$raster) || length(data$raster) != diff(tr) + 1L) {
    stop("time_range e raster devem definir um mapa por ano.")
  }
  if (is.null(year) && tr[1] == tr[2]) year <- tr[1]
  if (!integer_vector(year) || length(year) != 1L || year < tr[1] || year > tr[2]) {
    stop("year deve ser um ano inteiro dentro de time_range.")
  }
  if (!is.numeric(connectivity) || length(connectivity) != 1L ||
      is.na(connectivity) || !connectivity %in% c(4, 8)) stop("connectivity deve ser 4 ou 8.")
  for (flag in list(include_self, build_plot)) {
    if (!is.logical(flag) || length(flag) != 1L || is.na(flag)) {
      stop("include_self e build_plot devem ser logicos escalares.")
    }
  }
  r <- data$raster[[year - tr[1] + 1L]]
  if (!inherits(r, "RasterLayer")) stop("raster deve conter objetos RasterLayer.")
  crs <- sf::st_crs(r)
  if (is.na(crs)) stop("O raster deve ter CRS para calcular fronteiras em metros.")
  m <- raster::as.matrix(r)
  codes <- sort(unique(as.vector(m[!is.na(m)])))
  if (!integer_vector(codes)) stop("O mapa deve conter codigos inteiros e pixels validos.")
  dictionary <- .biomastats_dictionary(collection, codes, data$class_dictionary)
  labels <- if (language == "pt") dictionary$classe else dictionary$class
  colors <- dictionary$color
  nr <- nrow(m)
  nc <- ncol(m)
  k <- length(codes)
  counts <- lengths <- matrix(0, k, k)

  # Side lengths by raster row, respecting latitude and projected CRS units.
  dx <- raster::res(r)[1]
  dy <- raster::res(r)[2]
  if (isTRUE(sf::st_is_longlat(crs))) {
    y <- raster::ymax(r) - (seq_len(nr) - 1) * dy
    x <- raster::xmin(r)
    points <- function(x, y) sf::st_as_sf(data.frame(x = x, y = y),
                                        coords = c("x", "y"), crs = crs)
    vertical <- as.numeric(units::set_units(sf::st_distance(
      points(x, y), points(x, y - dy), by_element = TRUE), "m"))
    horizontal <- as.numeric(units::set_units(sf::st_distance(
      points(x, y - dy), points(x + dx, y - dy), by_element = TRUE), "m"))
  } else {
    if (is.null(crs$ud_unit)) stop("Unidade linear do CRS nao reconhecida.")
    sizes <- as.numeric(units::set_units(units::set_units(c(dx, dy), crs$ud_unit,
                                                        mode = "standard"), "m"))
    vertical <- rep(sizes[2], nr)
    horizontal <- rep(sizes[1], nr)
  }
  accumulate <- function(a, b, side) {
    a <- as.vector(a)
    b <- as.vector(b)
    valid <- !is.na(a) & !is.na(b)
    if (!any(valid)) return(invisible(NULL))
    index <- match(a[valid], codes) + (match(b[valid], codes) - 1L) * k
    counts[] <<- counts + matrix(tabulate(index, nbins = k * k), k, k)
    totals <- rowsum(side[valid], index, reorder = FALSE)
    lengths[as.integer(rownames(totals))] <<-
      lengths[as.integer(rownames(totals))] + totals[, 1]
    invisible(NULL)
  }
  if (nc > 1L) accumulate(m[, -nc, drop = FALSE], m[, -1, drop = FALSE],
                           rep(vertical, nc - 1L))
  if (nr > 1L) accumulate(m[-nr, , drop = FALSE], m[-1, , drop = FALSE],
                           rep(horizontal[seq_len(nr - 1L)], nc))
  if (connectivity == 8 && nr > 1L && nc > 1L) {
    side <- rep(0, (nr - 1L) * (nc - 1L))
    accumulate(m[-nr, -nc, drop = FALSE], m[-1, -1, drop = FALSE], side)
    accumulate(m[-nr, -1, drop = FALSE], m[-1, -nc, drop = FALSE], side)
  }
  internal <- diag(counts)
  adjacency <- counts + t(counts)
  diag(adjacency) <- internal
  boundary_m <- lengths + t(lengths)
  diag(boundary_m) <- 0
  dimnames(adjacency) <- dimnames(boundary_m) <- list(as.character(codes), as.character(codes))
  external <- adjacency
  diag(external) <- 0
  total_pairs <- sum(adjacency[upper.tri(adjacency, diag = TRUE)])
  if (!include_self) diag(adjacency) <- 0
  selected <- if (weight == "boundary") boundary_m else adjacency
  g <- igraph::graph_from_adjacency_matrix(selected, mode = "undirected", weighted = TRUE,
                                           diag = include_self)
  g <- igraph::set_vertex_attr(g, "code", value = codes)
  g <- igraph::set_vertex_attr(g, "class", value = dictionary$class)
  g <- igraph::set_vertex_attr(g, "classe", value = dictionary$classe)
  g <- igraph::set_vertex_attr(g, "label", value = labels)
  g <- igraph::set_vertex_attr(g, "group", value = dictionary$group)
  g <- igraph::set_vertex_attr(g, "grupo", value = dictionary$grupo)
  g <- igraph::set_vertex_attr(g, "color", value = colors)
  simple <- igraph::simplify(g, remove.multiple = FALSE, remove.loops = TRUE)
  weights <- igraph::edge_attr(simple, "weight")
  degree <- igraph::degree(simple)
  strength <- igraph::strength(simple, weights = weights)
  components <- igraph::components(simple)
  cost <- 1 / weights
  betweenness <- harmonic <- rep(0, k)
  if (length(weights)) {
    betweenness <- igraph::betweenness(simple, directed = FALSE, weights = cost)
    harmonic <- igraph::harmonic_centrality(simple, weights = cost, normalized = k > 1L)
  }
  mixing <- if (weight == "boundary") boundary_m else external
  entropy <- apply(mixing, 1, function(x) {
    if (!sum(x)) return(0)
    p <- x[x > 0] / sum(x)
    -sum(p * log(p))
  })
  incident <- rowSums(external) + 2 * internal
  node_metrics <- data.frame(code = codes, class = dictionary$class,
    classe = dictionary$classe, label = labels, group = dictionary$group,
    grupo = dictionary$grupo, color = colors,
    pixels = as.numeric(table(factor(as.vector(m), levels = codes))),
    degree = as.numeric(degree), strength = as.numeric(strength),
    betweenness = as.numeric(betweenness), harmonic = as.numeric(harmonic),
    component = as.integer(components$membership), neighbor_entropy = entropy,
    within_adjacencies = internal,
    homogeneity = ifelse(incident > 0, 2 * internal / incident, NA_real_),
    boundary_m = rowSums(boundary_m), row.names = NULL)
  pairs <- which(upper.tri(adjacency, diag = TRUE) & adjacency > 0, arr.ind = TRUE)
  edge_metrics <- data.frame(from = codes[pairs[, 1]], to = codes[pairs[, 2]],
    from_class = dictionary$class[pairs[, 1]], to_class = dictionary$class[pairs[, 2]],
    from_classe = dictionary$classe[pairs[, 1]], to_classe = dictionary$classe[pairs[, 2]],
    from_label = labels[pairs[, 1]], to_label = labels[pairs[, 2]],
    adjacencies = adjacency[pairs], boundary_m = boundary_m[pairs], weight = selected[pairs])
  edge_metrics$share <- if (sum(edge_metrics$weight) > 0) {
    edge_metrics$weight / sum(edge_metrics$weight)
  } else rep(0, nrow(edge_metrics))
  edge_metrics$betweenness <- rep(0, nrow(edge_metrics))
  if (length(weights)) {
    se <- igraph::as_data_frame(simple, what = "edges")
    key <- function(a, b) paste(pmin(as.numeric(a), as.numeric(b)),
                               pmax(as.numeric(a), as.numeric(b)), sep = ":")
    idx <- match(key(se$from, se$to), key(edge_metrics$from, edge_metrics$to))
    edge_metrics$betweenness[idx] <- igraph::edge_betweenness(simple, directed = FALSE,
                                                           weights = cost)
  }
  transitivity <- igraph::transitivity(simple, type = "global", isolates = "zero")
  graph_metrics <- data.frame(year = year, connectivity = connectivity, weight = weight,
    classes_count = k, edges_count = igraph::ecount(simple),
    connected_components = components$no,
    density = if (k > 1L) igraph::edge_density(simple, loops = FALSE) else 0,
    transitivity = if (is.finite(transitivity)) transitivity else 0,
    mean_degree = mean(degree), mean_strength = mean(strength),
    total_adjacencies = sum(adjacency[upper.tri(adjacency, diag = TRUE)]),
    total_boundary_m = sum(boundary_m[upper.tri(boundary_m)]),
    interclass_fraction = if (total_pairs > 0) sum(external[upper.tri(external)]) / total_pairs else NA_real_)
  # Keep the igraph representation analytically equivalent to the tables.
  for (field in setdiff(names(node_metrics), c("code", "class", "classe", "label", "group", "grupo", "color"))) {
    g <- igraph::set_vertex_attr(g, field, value = node_metrics[[field]])
  }
  ge <- igraph::as_data_frame(g, what = "edges")
  key <- function(a, b) paste(pmin(as.numeric(a), as.numeric(b)),
                             pmax(as.numeric(a), as.numeric(b)), sep = ":")
  edge_index <- match(key(ge$from, ge$to), key(edge_metrics$from, edge_metrics$to))
  for (field in setdiff(names(edge_metrics), c("from", "to"))) {
    g <- igraph::set_edge_attr(g, field, value = edge_metrics[[field]][edge_index])
  }
  plots <- list(plot = NULL, graph_plot = NULL, heatmap = NULL, edge_plot = NULL)
  if (build_plot) plots <- .land_graph_plots(g, node_metrics, edge_metrics, adjacency,
                                            weight, language)
  metadata <- list(schema_version = "1.0", package_version = as.character(utils::packageVersion("biomastats")),
    year = year, collection = collection, connectivity = connectivity,
    include_self = include_self, weight = weight, language = language, crs_wkt = crs$wkt,
    nrows = nr, ncols = nc, resolution_x = dx, resolution_y = dy,
    xmin = raster::xmin(r), xmax = raster::xmax(r), ymin = raster::ymin(r), ymax = raster::ymax(r),
    boundary_unit = "m", pair_counting = "once; symmetric matrices",
    nodata = "NA excluded; zero is a class", diagonal_boundary_m = 0,
    centrality_cost = "inverse weight; loops excluded")
  for (field in names(metadata)) g <- igraph::set_graph_attr(g, field, value = metadata[[field]])
  out <- structure(c(list(adjacency = adjacency, boundary_m = boundary_m, graph = g), plots,
    list(graph_metrics = graph_metrics, node_metrics = node_metrics, edge_metrics = edge_metrics,
         class_dictionary = dictionary, metadata = metadata)),
    class = c("biomastats_land_graph", "list"))
  out
}

# Shared ggplot grammar for static graphics and the optional HTML widget.
.land_graph_plots <- function(g, nodes, edges, adjacency, weight, language) {
  tr <- function(pt, en) if (language == "pt") pt else en
  xy <- igraph::layout_in_circle(g)
  nodes$display <- as.character(nodes$code)
  nodes$size_value <- nodes$strength
  legend_labels <- stats::setNames(vapply(paste(nodes$code, nodes$label),
    function(label) paste(strwrap(label, width = 32), collapse = "\n"), character(1)),
    as.character(nodes$code))
  class_colours <- stats::setNames(nodes$color, as.character(nodes$code))
  nodes$x <- xy[, 1]
  nodes$y <- xy[, 2]
  nodes$tooltip <- paste0(nodes$label, " (", nodes$code, ")\nPixels: ", nodes$pixels,
                          tr("\nGrau: ", "\nDegree: "), nodes$degree,
                          tr("\nForca: ", "\nStrength: "), signif(nodes$strength, 5))
  all_edges <- edges
  nodes$id <- as.character(nodes$code)
  edges <- edges[edges$weight > 0, , drop = FALSE]
  i <- match(edges$from, nodes$code)
  j <- match(edges$to, nodes$code)
  edges$x <- nodes$x[i]
  edges$y <- nodes$y[i]
  edges$xend <- nodes$x[j]
  edges$yend <- nodes$y[j]
  edges$tooltip <- if (nrow(edges)) paste0(edges$from_label, " / ", edges$to_label,
    tr("\nContatos: ", "\nContacts: "), edges$adjacencies,
    tr("\nFronteira (m): ", "\nBoundary (m): "), signif(edges$boundary_m, 5)) else character()
  edges$id <- paste(edges$from, edges$to, sep = ":")
  nonloops <- edges[edges$from != edges$to, , drop = FALSE]
  loops <- edges[edges$from == edges$to, , drop = FALSE]
  loop_paths <- lapply(seq_len(nrow(loops)), function(i) {
    theta <- seq(0, 2 * pi, length.out = 40)
    out <- loops[rep(i, length(theta)), , drop = FALSE]
    out$x <- out$x + 0.12 + 0.12 * cos(theta)
    out$y <- out$y + 0.12 + 0.12 * sin(theta)
    out
  })
  loops <- if (length(loop_paths)) do.call(rbind, loop_paths) else loops
  interactive <- requireNamespace("ggiraph", quietly = TRUE)
  make_plot <- function(widget = FALSE) {
    p <- ggplot2::ggplot()
    if (nrow(nonloops)) {
      mapping <- ggplot2::aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight)
      if (widget) {
        mapping$tooltip <- ggplot2::aes(tooltip = tooltip)$tooltip
        mapping$data_id <- ggplot2::aes(data_id = id)$data_id
      }
      layer <- if (widget) ggiraph::geom_segment_interactive else ggplot2::geom_segment
      p <- p + layer(data = nonloops, mapping = mapping, colour = "#687787", alpha = 0.65)
    }
    if (nrow(loops)) {
      mapping <- ggplot2::aes(x = x, y = y, group = id, linewidth = weight)
      if (widget) {
        mapping$tooltip <- ggplot2::aes(tooltip = tooltip)$tooltip
        mapping$data_id <- ggplot2::aes(data_id = id)$data_id
      }
      layer <- if (widget) ggiraph::geom_path_interactive else ggplot2::geom_path
      p <- p + layer(data = loops, mapping = mapping, colour = "#687787")
    }
    mapping <- ggplot2::aes(x = x, y = y, colour = id, size = size_value)
    if (widget) {
      mapping$tooltip <- ggplot2::aes(tooltip = tooltip)$tooltip
      mapping$data_id <- ggplot2::aes(data_id = id)$data_id
    }
    layer <- if (widget) ggiraph::geom_point_interactive else ggplot2::geom_point
    p + layer(data = nodes, mapping = mapping) +
      ggplot2::geom_text(data = nodes, ggplot2::aes(x = 1.22 * x, y = 1.22 * y,
                        label = display), size = 3, check_overlap = TRUE) +
      ggplot2::scale_colour_manual(values = stats::setNames(nodes$color, nodes$id),
        limits = nodes$id, breaks = nodes$id,
        labels = legend_labels,
        name = tr("Classe (c\u00f3digo)", "Class (code)"), drop = FALSE) +
      ggplot2::scale_size_continuous(range = c(5, 14), breaks = scales::breaks_pretty(n = 3)) +
      ggplot2::guides(colour = ggplot2::guide_legend(order = 1,
        override.aes = list(size = 5, shape = 15, alpha = 1))) +
      # Legend name on the scale: graphs without edges then raise no unknown-label warning.
      ggplot2::scale_linewidth_continuous(name = tr("Peso", "Weight"), range = c(0.4, 4),
                                          breaks = scales::breaks_pretty(n = 3)) +
      ggplot2::coord_equal(xlim = c(-1.6, 1.6), ylim = c(-1.6, 1.6)) +
      ggplot2::labs(title = NULL,
        subtitle = if (weight == "boundary") tr("Peso: fronteira aproximada (m)", "Weight: approximate boundary (m)") else tr("Peso: contatos entre pixels", "Weight: pixel contacts"),
        size = tr("Forca", "Strength")) +
      ggplot2::theme_void(base_size = 12)
  }
  widget <- NULL
  if (interactive) {
    widget <- ggiraph::girafe(ggobj = make_plot(TRUE), width_svg = 8, height_svg = 6,
      options = list(ggiraph::opts_zoom(min = 0.5, max = 5), ggiraph::opts_hover_inv(css = "opacity:0.25;")))
  } else warning("Instale ggiraph para obter graph_plot interativo.", call. = FALSE)
  tiles <- expand.grid(from = colnames(adjacency), to = rownames(adjacency), stringsAsFactors = FALSE)
  tiles$count <- as.vector(adjacency)
  axis_labels <- stats::setNames(nodes$display, as.character(nodes$code))
  heatmap <- ggplot2::ggplot(tiles, ggplot2::aes(x = from, y = to, fill = count, colour = from)) +
    ggplot2::geom_tile(linewidth = 0.15) +
    ggplot2::scale_colour_manual(values = stats::setNames(nodes$color, as.character(nodes$code)),
      limits = as.character(nodes$code),
      breaks = as.character(nodes$code),
      labels = legend_labels,
      name = tr("Classe (c\u00f3digo)", "Class (code)"), drop = FALSE) +
    ggplot2::guides(colour = ggplot2::guide_legend(order = 1,
      override.aes = list(fill = unname(class_colours), colour = "#555555", linewidth = 0.4))) +
    ggplot2::scale_x_discrete(limits = as.character(nodes$code), labels = axis_labels) +
    ggplot2::scale_y_discrete(limits = as.character(nodes$code), labels = axis_labels) +
    ggplot2::scale_fill_gradient(low = "#f3f6f1", high = "#16543b", name = tr("Contatos", "Contacts")) +
    ggplot2::coord_equal() + ggplot2::labs(title = NULL,
      x = tr("Classe", "Class"), y = tr("Classe", "Class")) + ggplot2::theme_minimal(base_size = 12)
  all_edges$interface <- paste(all_edges$from_label, all_edges$to_label, sep = " / ")
  all_edges$from_id <- as.character(all_edges$from)
  edge_plot <- ggplot2::ggplot(all_edges, ggplot2::aes(
    x = stats::reorder(interface, boundary_m), y = boundary_m / 1000, fill = from_id)) +
    ggplot2::geom_col(show.legend = TRUE) +
    ggplot2::scale_fill_manual(values = stats::setNames(nodes$color, as.character(nodes$code)),
      limits = as.character(nodes$code),
      breaks = as.character(nodes$code),
      labels = legend_labels,
      name = tr("Classe de origem (c\u00f3digo)", "Source class (code)"), drop = FALSE) + ggplot2::coord_flip() +
    ggplot2::labs(title = NULL, x = tr("Par de classes", "Class pair"), y = tr("Fronteira (km)", "Boundary (km)")) +
    ggplot2::theme_minimal(base_size = 12)
  list(plot = make_plot(), graph_plot = widget, heatmap = heatmap, edge_plot = edge_plot)
}

utils::globalVariables(c("x", "y", "xend", "yend", "weight", "tooltip", "id",
                         "color", "strength", "code", "from", "to", "count",
                          "size_value", "display", "interface", "boundary_m", "from_id"))

