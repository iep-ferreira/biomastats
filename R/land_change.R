#' Mudancas de uso e cobertura em dois ou tres momentos
#'
#' Cruza os mesmos pixels em dois ou tres anos do objeto de [load_rasters()],
#' retornando trajetorias, transicoes, metricas, rede temporal e graficos
#' bipartite/tripartite prontos para reuso e personalizacao com ggplot2.
#'
#' @param data Objeto de load_rasters com raster, time_range e collection.
#' @param years Dois ou tres anos distintos. A ordem e cronologica. NULL usa
#'   todos os anos somente se o objeto tiver exatamente dois ou tres anos.
#' @param language Idioma dos rotulos, "pt" (padrao) ou "en". O dicionario
#'   e as tabelas conservam nomes em ambos os idiomas.
#' @param build_plot Construir graficos ggplot2 (padrao TRUE). FALSE conserva
#'   todos os resultados numericos, rede e dados de desenho, sem os graficos.
#'
#' @return Lista de classe biomastats_land_change contendo:
#' \itemize{
#'   \item years, class_dictionary e metadata: anos, dicionario e proveniencia.
#'   \item trajectories: codigos code_ANO, pixels, area_ha, share e trajectory_id.
#'   \item transitions: pares observados com anos, codigos, nomes pt/en, cores,
#'     pixels, area_ha, share e source_fraction (fracao da area de origem).
#'   \item matrices: por intervalo, matrizes pixels, area_ha e probability;
#'     linhas sao origem, colunas destino, ambas com chaves de codigo.
#'   \item class_metrics: area inicial/final, persistencia, ganhos, perdas,
#'     saldo, mudanca bruta, troca e saldo anual medio por classe e intervalo.
#'   \item interval_metrics: area comum, persistencia, mudanca total, fracao
#'     alterada e componentes de quantidade e alocacao por intervalo.
#'   \item trajectory_metrics: areas, pixels e fracoes por tipo de trajetoria.
#'   \item coverage: cobertura observada e exclusoes da mascara comum por ano.
#'   \item node_metrics e graph: classes por ano e igraph temporal dirigido.
#'   \item plot_data: nodes e flows usados no desenho dos graficos.
#'   \item plot: ggplot de faixas bipartite ou tripartite; change_plot: ggplot
#'     dos saldos de area por classe, facetado por intervalo.
#' }
#' @details Os mapas devem ter a mesma grade, extensao, resolucao e CRS.
#'   Nenhuma reprojecao, reamostragem ou imputacao e realizada. Somente pixels
#'   validos em todos os anos selecionados entram na analise. NA e excluido;
#'   zero e uma classe. Um codigo deve ter o mesmo significado entre anos.
#'   Codigo desconhecido fica identificado no dicionario com known=FALSE e
#'   cor neutra. Colecao, classes, CRS e resolucao sao herdados de data.
#'
#'   A area de cada pixel vem de terra::cellSize, como em [get_area()]: e a
#'   area geodesica da celula em CRS geografico; em CRS projetado, o terra
#'   transforma as celulas para longitude/latitude e corrige a distorcao da
#'   projecao. Assim, initial_ha e final_ha de cada classe coincidem com
#'   100 * area_raw de get_area. area_method e as convencoes constam em metadata.
#'
#'   As comparacoes incluem intervalos consecutivos e, para tres anos, tambem
#'   o primeiro versus o ultimo. Todas usam a mesma mascara comum. A rede e o
#'   grafico de faixas usam apenas os intervalos consecutivos e as trajetorias
#'   observadas completas; nao multiplicam matrizes para inferir trajetorias.
#'
#'   Persistencia e a diagonal da matriz de areas M. Ganho e colSums(M)-diag(M),
#'   perda e rowSums(M)-diag(M), saldo e ganho-perda, mudanca bruta por classe e
#'   ganho+perda e troca por classe e 2*min(ganho,perda). A mudanca total da
#'   paisagem e total-sum(diag(M)); somar mudancas brutas por classe a duplica.
#'   quantity_ha = sum(abs(saldos))/2 e allocation_ha = mudanca-quantity_ha.
#'   Sao decomposicoes descritivas da comparacao temporal, nao estimativas de
#'   erro de classificacao. net_ha_per_year e apenas saldo/duracao do intervalo.
#'   probability e a proporcao empirica de area de destino em cada origem;
#'   linhas sem area de origem recebem NA. Nao e um modelo preditivo de Markov.
#'
#'   Com tres anos, trajetorias A-A-A sao stable, A-A-B/A-B-B sao changed_once,
#'   A-B-A sao returned e A-B-C sao changed_twice. Retornos sao observados apenas
#'   nas datas selecionadas; nao revelam eventos entre elas. Para dois anos,
#'   as categorias sao stable e changed_once.
#'
#'   No grafico, as faixas conservam a cor da classe do primeiro ano; blocos
#'   usam a cor da classe em cada ano. Alturas e espessuras representam area;
#'   espacamentos sao visuais. Eixos temporais sao igualmente espacados.
#'   Legendas mostram amostras de cor, codigo e nome, sem hexadecimais impressos.
#'   Use labs(), theme() e escalas ggplot2 nos retornos e ggsave() para salvar.
#'   saveRDS/readRDS preservam o resultado completo sem os rasters originais.
#'   Os mapas selecionados sao lidos em memoria; recorte mapas extensos antes.
#' @references
#' Pontius e Millones (2011), decomposicao de diferencas entre mapas:
#' \doi{10.1080/01431161.2011.552923}.
#' @md
#' @examples
#' \dontrun{
#' mudancas <- land_change(mapas, years = c(1985, 2000, 2021))
#' mudancas$plot + ggplot2::labs(title = "Trajetorias de uso da terra")
#' mudancas$class_metrics
#' mudancas$matrices[[1]]$area_ha
#' saveRDS(mudancas, "mudancas.rds")
#' }
#' @export
land_change <- function(data, years = NULL, language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  integers <- function(x) is.numeric(x) && length(x) > 0L &&
    all(is.finite(x) & x == trunc(x))
  if (!is.list(data) || !all(c("raster", "time_range", "collection") %in% names(data))) {
    stop("data deve ser o objeto de load_rasters: raster, time_range e collection.")
  }
  tr <- data$time_range
  if (!integers(tr) || length(tr) != 2L || tr[1] > tr[2] ||
      !is.list(data$raster) || length(data$raster) != diff(tr) + 1L) {
    stop("time_range e raster devem definir um mapa por ano.")
  }
  collection <- .biomastats_collection(data$collection)
  if (is.null(years) && diff(tr) %in% c(1, 2)) years <- seq.int(tr[1], tr[2])
  if (!integers(years) || !length(years) %in% c(2L, 3L) || anyDuplicated(years) ||
      any(years < tr[1] | years > tr[2])) stop("years deve selecionar dois ou tres anos distintos disponiveis.")
  years <- sort(years)
  if (!is.logical(build_plot) || length(build_plot) != 1L || is.na(build_plot)) {
    stop("build_plot deve ser logico escalar.")
  }
  maps <- data$raster[years - tr[1] + 1L]
  if (!all(vapply(maps, inherits, logical(1), what = "RasterLayer"))) {
    stop("raster deve conter objetos RasterLayer.")
  }
  template <- maps[[1]]
  crs <- sf::st_crs(template)
  if (is.na(crs)) stop("Raster sem CRS; necessario para calcular area.")
  # Tolerance of 1e-6 cell: years that raster wrote to disk during load_rasters()
  # differ from in-memory years by ~1e-14 degrees in the extent, on the same grid.
  aligned <- vapply(maps, function(r) !is.na(sf::st_crs(r)) &&
    raster::compareRaster(template, r, extent = TRUE, rowcol = TRUE, crs = TRUE,
      res = TRUE, orig = TRUE, rotation = TRUE, tolerance = 1e-6,
      stopiffalse = FALSE, showwarning = FALSE), logical(1))
  if (!all(aligned) || raster::rotated(template)) {
    stop("Rasters devem ter a mesma grade, extensao, resolucao e CRS, sem rotacao.")
  }
  values <- do.call(cbind, lapply(maps, raster::getValues))
  if (any(!is.na(values) & (!is.finite(values) | values != trunc(values)))) {
    stop("Os mapas devem conter codigos inteiros ou NA.")
  }
  common <- rowSums(is.na(values)) == 0L
  if (!any(common)) stop("Nenhum pixel valido comum aos anos selecionados.")
  area <- .land_change_cell_area(template)
  coverage <- data.frame(year = years, total_pixels = nrow(values),
    valid_pixels = colSums(!is.na(values)), common_pixels = sum(common),
    excluded_valid_pixels = colSums(!is.na(values)) - sum(common),
    valid_area_ha = colSums((!is.na(values)) * area), common_area_ha = sum(area[common]))
  selected <- values[common, , drop = FALSE]
  codes <- sort(unique(as.vector(selected)))
  dictionary <- .biomastats_dictionary(collection, codes)
  key <- apply(selected, 1, paste, collapse = ":")
  keys <- unique(key)
  first <- match(keys, key)
  trajectories <- as.data.frame(selected[first, , drop = FALSE])
  names(trajectories) <- paste0("code_", years)
  trajectories$pixels <- tabulate(match(key, keys), nbins = length(keys))
  trajectories$area_ha <- as.numeric(rowsum(area[common], match(key, keys), reorder = FALSE))
  order_args <- unname(trajectories[paste0("code_", years)])
  trajectories <- trajectories[do.call(order, order_args), , drop = FALSE]
  rownames(trajectories) <- NULL
  trajectories$share <- trajectories$area_ha / sum(trajectories$area_ha)
  trajectories$trajectory_id <- seq_len(nrow(trajectories))
  total <- sum(trajectories$area_ha)
  stage <- as.matrix(trajectories[paste0("code_", years)])
  changes <- rowSums(stage[, -1, drop = FALSE] != stage[, -ncol(stage), drop = FALSE])
  category <- ifelse(changes == 0, "stable", "changed_once")
  types <- c("stable", "changed_once")
  if (length(years) == 3L) {
    category[changes == 2] <- ifelse(stage[changes == 2, 1] == stage[changes == 2, 3],
                                      "returned", "changed_twice")
    types <- c(types, "returned", "changed_twice")
  }
  trajectory_metrics <- data.frame(type = types,
    pixels = vapply(types, function(type) sum(trajectories$pixels[category == type]), numeric(1)),
    area_ha = vapply(types, function(type) sum(trajectories$area_ha[category == type]), numeric(1)))
  trajectory_metrics$share <- trajectory_metrics$area_ha / total
  trajectories$type <- category
  pairs <- rbind(c(1L, 2L), if (length(years) == 3L) rbind(c(2L, 3L), c(1L, 3L)))
  comparisons <- lapply(seq_len(nrow(pairs)), function(i) {
    a <- pairs[i, 1]; b <- pairs[i, 2]
    .land_change_interval(trajectories, years[a], years[b], dictionary, total, language)
  })
  names(comparisons) <- paste(years[pairs[, 1]], years[pairs[, 2]], sep = "_")
  bind <- function(field) { out <- do.call(rbind, lapply(comparisons, `[[`, field)); rownames(out) <- NULL; out }
  transitions <- bind("transitions")
  class_metrics <- bind("class_metrics")
  interval_metrics <- bind("interval_metrics")
  node_metrics <- do.call(rbind, lapply(seq_along(years), function(j) {
    out <- dictionary
    out$year <- years[j]
    out$partition <- j
    out$name <- paste(years[j], codes, sep = ":")
    out$label <- if (language == "pt") out$classe else out$class
    out$area_ha <- vapply(codes, function(code) sum(trajectories$area_ha[stage[, j] == code]), numeric(1))
    out$pixels <- vapply(codes, function(code) sum(trajectories$pixels[stage[, j] == code]), numeric(1))
    out
  }))
  rownames(node_metrics) <- NULL
  adjacent <- transitions$end_year - transitions$start_year > 0 &
    paste(transitions$start_year, transitions$end_year, sep = "_") %in%
    paste(years[-length(years)], years[-1], sep = "_")
  edges <- transitions[adjacent, , drop = FALSE]
  edges <- data.frame(from = paste(edges$start_year, edges$from, sep = ":"),
    to = paste(edges$end_year, edges$to, sep = ":"),
    edges[, c("start_year", "end_year", "pixels", "area_ha", "source_fraction")])
  edges$weight <- edges$area_ha
  graph <- igraph::graph_from_data_frame(edges, directed = TRUE,
    vertices = node_metrics[, c("name", setdiff(names(node_metrics), "name"))])
  if (length(years) == 2L) {
    graph <- igraph::set_vertex_attr(graph, "type", value = node_metrics$partition == 2L)
  }
  plot_data <- .land_change_plot_data(trajectories, node_metrics, years, total)
  plots <- if (build_plot) .land_change_plots(plot_data, class_metrics, dictionary, years, language)
    else list(plot = NULL, change_plot = NULL)
  metadata <- list(schema_version = "1.0", package_version = as.character(utils::packageVersion("biomastats")),
    collection = collection, years = years, language = language, crs_wkt = crs$wkt,
    resolution = raster::res(template), extent = as.vector(raster::extent(template)),
    nrows = raster::nrow(template), ncols = raster::ncol(template), area_unit = "ha",
    area_method = "terra::cellSize(unit = \"ha\", mask = FALSE), as in get_area()",
    mask = "complete cases across all selected years; NA excluded; zero is a class",
    source = "observed trajectories, no temporal interpolation or resampling",
    flow_colour = "first-year class", node_colour = "class at each year",
    comparison = "adjacent intervals and first-last; shared complete-case mask")
  for (field in c("collection", "crs_wkt", "area_unit", "mask")) {
    graph <- igraph::set_graph_attr(graph, field, metadata[[field]])
  }
  graph <- igraph::set_graph_attr(graph, "years", paste(years, collapse = ","))
  structure(c(list(years = years, class_dictionary = dictionary, metadata = metadata,
    trajectories = trajectories, transitions = transitions,
    matrices = lapply(comparisons, `[[`, "matrices"), class_metrics = class_metrics,
    interval_metrics = interval_metrics, trajectory_metrics = trajectory_metrics,
    coverage = coverage, node_metrics = node_metrics, graph = graph, plot_data = plot_data), plots),
    class = c("biomastats_land_change", "list"))
}

# Same cell areas as get_area(), in the cell order of raster::getValues().
.land_change_cell_area <- function(r) {
  area <- terra::values(terra::cellSize(.as_terra(r), unit = "ha", mask = FALSE), mat = FALSE)
  if (any(!is.finite(area) | area <= 0)) stop("Nao foi possivel calcular areas de celula validas.")
  area
}

.land_change_interval <- function(paths, start, end, dictionary, total, language) {
  codes <- dictionary$code
  k <- length(codes)
  from <- paths[[paste0("code_", start)]]
  to <- paths[[paste0("code_", end)]]
  idx <- match(from, codes) + (match(to, codes) - 1L) * k
  aggregate_matrix <- function(weight) {
    m <- matrix(0, k, k, dimnames = list(as.character(codes), as.character(codes)))
    sums <- rowsum(weight, idx, reorder = FALSE)
    m[as.integer(rownames(sums))] <- sums[, 1]
    m
  }
  m <- aggregate_matrix(paths$area_ha)
  counts <- aggregate_matrix(paths$pixels)
  initial <- rowSums(m); final <- colSums(m); persistent <- diag(m)
  probability <- sweep(m, 1, ifelse(initial > 0, initial, NA_real_), "/")
  gain <- final - persistent; loss <- initial - persistent; net <- gain - loss
  cm <- data.frame(start_year = start, end_year = end, dictionary,
    initial_ha = as.numeric(initial), final_ha = as.numeric(final),
    persistence_ha = as.numeric(persistent), gain_ha = as.numeric(gain), loss_ha = as.numeric(loss),
    net_ha = as.numeric(net), gross_change_ha = as.numeric(gain + loss),
    swap_ha = as.numeric(2 * pmin(gain, loss)), net_ha_per_year = as.numeric(net / (end - start)),
    persistence_fraction = as.numeric(ifelse(initial > 0, persistent / initial, NA_real_)))
  cm$label <- if (language == "pt") cm$classe else cm$class
  positions <- which(m > 0, arr.ind = TRUE)
  a <- positions[, 1]; b <- positions[, 2]
  transitions <- data.frame(start_year = start, end_year = end, from = codes[a], to = codes[b],
    from_classe = dictionary$classe[a], to_classe = dictionary$classe[b],
    from_class = dictionary$class[a], to_class = dictionary$class[b],
    from_color = dictionary$color[a], to_color = dictionary$color[b],
    pixels = counts[positions], area_ha = m[positions], share = m[positions] / total,
    source_fraction = probability[positions])
  changed <- sum(m) - sum(persistent)
  quantity <- sum(abs(net)) / 2
  summary <- data.frame(start_year = start, end_year = end, duration_years = end - start,
    common_pixels = sum(counts), total_ha = total, persistence_ha = sum(persistent),
    change_ha = changed, change_fraction = changed / total,
    quantity_ha = quantity, allocation_ha = max(0, changed - quantity))
  list(transitions = transitions, class_metrics = cm, interval_metrics = summary,
       matrices = list(pixels = counts, area_ha = m, probability = probability))
}

.land_change_plot_data <- function(paths, nodes, years, total) {
  nodes$x <- match(nodes$year, years)
  nodes$ymin <- nodes$ymax <- 0
  gap <- total * 0.012
  lower <- upper <- matrix(0, nrow(paths), length(years))
  for (j in seq_along(years)) {
    ids <- which(nodes$year == years[j])
    heights <- nodes$area_ha[ids]
    nodes$ymin[ids] <- c(0, utils::head(cumsum(heights), -1)) + (seq_along(ids) - 1) * gap
    nodes$ymax[ids] <- nodes$ymin[ids] + heights
    for (id in ids) {
      selected <- which(paths[[paste0("code_", years[j])]] == nodes$code[id])
      if (!length(selected)) next
      cumulative <- cumsum(paths$area_ha[selected])
      lower[selected, j] <- nodes$ymin[id] + cumulative - paths$area_ha[selected]
      upper[selected, j] <- nodes$ymin[id] + cumulative
    }
  }
  nodes$code_id <- as.character(nodes$code)
  rgb <- grDevices::col2rgb(nodes$color) / 255
  luminance <- as.numeric(c(0.2126, 0.7152, 0.0722) %*% rgb)
  nodes$label_colour <- ifelse(luminance < 0.45, "white", "black")
  flows <- lapply(seq_len(length(years) - 1L), function(j) {
    do.call(rbind, lapply(seq_len(nrow(paths)), function(i) {
      t <- seq(0, 1, length.out = 25)
      smooth <- t * t * (3 - 2 * t)
      x <- j + 0.08 + t * 0.84
      low <- lower[i, j] + smooth * (lower[i, j + 1] - lower[i, j])
      high <- upper[i, j] + smooth * (upper[i, j + 1] - upper[i, j])
      data.frame(x = c(x, rev(x)), y = c(low, rev(high)),
        flow_id = paste(j, i, sep = ":"), trajectory_id = paths$trajectory_id[i],
        code_id = as.character(paths[[paste0("code_", years[1])]][i]))
    }))
  })
  list(nodes = nodes, flows = do.call(rbind, flows))
}

.land_change_plots <- function(plot_data, class_metrics, dictionary, years, language) {
  tr <- function(pt, en) if (language == "pt") pt else en
  palette <- stats::setNames(dictionary$color, as.character(dictionary$code))
  label <- if (language == "pt") dictionary$classe else dictionary$class
  legend <- vapply(paste(dictionary$code, label), function(x) paste(strwrap(x, 32), collapse = "\n"), character(1))
  colour_scale <- function() ggplot2::scale_fill_manual(values = palette,
    limits = names(palette), breaks = names(palette),
    labels = stats::setNames(legend, names(palette)), drop = FALSE,
    name = tr("Classe (c\u00f3digo)", "Class (code)"))
  nodes <- plot_data$nodes[plot_data$nodes$area_ha > 0, , drop = FALSE]
  p <- ggplot2::ggplot() +
    ggplot2::geom_polygon(data = plot_data$flows,
      ggplot2::aes(x = x, y = y, group = flow_id, fill = code_id), alpha = 0.65, colour = NA) +
    ggplot2::geom_rect(data = nodes, ggplot2::aes(xmin = x - 0.08, xmax = x + 0.08,
      ymin = ymin, ymax = ymax, fill = code_id), colour = "#555555", linewidth = 0.2) +
    ggplot2::geom_text(data = nodes, ggplot2::aes(x = x, y = (ymin + ymax) / 2,
      label = code, colour = label_colour), size = 3, check_overlap = TRUE) +
    ggplot2::scale_colour_identity() + colour_scale() +
    ggplot2::scale_x_continuous(breaks = seq_along(years), labels = years) +
    ggplot2::labs(x = tr("Ano", "Year"), y = tr("\u00c1rea (ha)", "Area (ha)"),
      subtitle = tr("Faixas: cor da classe inicial; blocos: classe de cada ano",
                    "Flows: initial class colour; blocks: class at each year")) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(alpha = 1))) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
  cm <- class_metrics
  cm$code_id <- as.character(cm$code)
  cm$interval <- paste(cm$start_year, cm$end_year, sep = " - ")
  net <- ggplot2::ggplot(cm, ggplot2::aes(x = factor(code, levels = dictionary$code),
    y = net_ha, fill = code_id)) + ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
    ggplot2::geom_col(show.legend = TRUE) + colour_scale() +
    ggplot2::facet_wrap(~interval) + ggplot2::coord_flip() +
    ggplot2::labs(x = tr("Classe (c\u00f3digo)", "Class (code)"),
      y = tr("Saldo de \u00e1rea (ha)", "Net area change (ha)")) + ggplot2::theme_minimal(base_size = 12)
  list(plot = p, change_plot = net)
}

utils::globalVariables(c("flow_id", "code_id", "xmin", "xmax", "ymin", "ymax", "net_ha", "interval", "label_colour"))
