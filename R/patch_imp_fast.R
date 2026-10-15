#' Priorizacao otimizada de fragmentos compativel com lconnect
#'
#' Reutiliza a matriz de distancias de um objeto lconnect, sem reconstruir
#' geometrias ou recalcular distancias a cada remocao de fragmento.
#' @param landscape Objeto de classe lconnect, produzido por
#'   [land_connectivity()] ou lconnect::upload_land(). Deve usar CRS em metros.
#' @param metric Indice de priorizacao: "IIC" (padrao) ou "AWF".
#' @param parallel Paralelizar remocoes IIC com future.apply, usando o plano
#'   definido pelo usuario. FALSE por padrao; nao altera o plano global.
#' @return Objeto de classe pimp com landscape (sf com coluna attributes) e
#'   prioritization (reducao percentual do indice apos remover cada fragmento).
#'   O atributo metric identifica o indice. Nao escreve arquivos.
#' @details IIC usa caminhos topologicos nao ponderados, arestas com
#'   0 < distancia < max_dist e exclui vertices isolados, reproduzindo
#'   lconnect 0.1.2. A area do limite permanece fixa em todas as remocoes.
#'   AWF usa exp(-k*d), k = -log(0.5)/(max_dist/2), sem diagonal, para todos
#'   os pares, mesmo alem do limiar. Sua contribuicao marginal e calculada
#'   por operacoes matriciais, incluindo corretamente as duas parcelas da
#'   diagonal que precisam ser removidas. AWF tem unidades de area ao quadrado.
#'   Quando o indice inicial e zero, a prioridade e indefinida e retorna NaN;
#'   status = "undefined_baseline" e anexado ao objeto. Uma paisagem com um
#'   fragmento tambem nao possui ranking relativo e recebe esse status.
#'   Memoria quadratica no numero de fragmentos; IIC recalcula caminhos apos
#'   cada remocao e pode continuar custoso em paisagens grandes.
#' @references [lconnect](https://cran.r-project.org/package=lconnect).
#' @md
#' @export
patch_imp_fast <- function(landscape, metric = c("IIC", "AWF"), parallel = FALSE) {
  metric <- match.arg(metric)
  .connectivity_check_flag(parallel, "parallel")
  checked <- .connectivity_check_land(landscape)
  importance <- .patch_imp_engine(checked$D, checked$areas, landscape$max_dist,
    as.numeric(landscape$area_l), metric, parallel)
  p <- landscape$landscape
  if (!"fragment_id" %in% names(p)) {
    p$fragment_id <- if (!is.null(landscape$fragment_id)) landscape$fragment_id else seq_len(nrow(p))
  }
  p$attributes <- importance
  status <- if (all(is.nan(importance))) "undefined_baseline" else "ok"
  structure(list(landscape = p, prioritization = importance), class = "pimp",
    metric = metric, status = status)
}

.connectivity_check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(paste(name, "deve ser logico escalar."))
}

.connectivity_check_land <- function(landscape) {
  if (!inherits(landscape, "lconnect") || !inherits(landscape$landscape, "sf")) {
    stop("landscape deve ser um objeto lconnect com landscape sf.")
  }
  p <- landscape$landscape
  if (!nrow(p) || is.na(sf::st_crs(p)) || sf::st_is_longlat(p) ||
      !.connectivity_metres(sf::st_crs(p))) stop("landscape deve ter fragmentos em CRS projetado em metros.")
  d <- as.matrix(landscape$distance)
  a <- as.numeric(units::set_units(sf::st_area(p), "m^2"))
  md <- landscape$max_dist
  al <- as.numeric(landscape$area_l)
  if (!is.numeric(d) || !identical(dim(d), c(nrow(p), nrow(p))) ||
      any(!is.finite(d) | d < 0) || !isTRUE(all.equal(unname(d), unname(t(d)))) ||
      any(diag(d) != 0) || any(!is.finite(a) | a <= 0) ||
      !is.numeric(md) || length(md) != 1L || !is.finite(md) || md <= 0 ||
      length(al) != 1L || !is.finite(al) || al <= 0) {
    stop("Distancias, areas, area_l e max_dist devem ser validos e compativeis.")
  }
  list(D = d, areas = a)
}

.connectivity_metres <- function(crs) {
  if (is.null(crs$ud_unit)) return(FALSE)
  tryCatch(isTRUE(all.equal(as.numeric(units::set_units(
    units::set_units(1, crs$ud_unit, mode = "standard"), "m")), 1)), error = function(e) FALSE)
}

# Metric graph: lconnect 0.1.2 drops zero distances and isolated vertices.
.build_lconnect_graph <- function(D, max_dist) {
  idx <- which(D > 0 & D < max_dist & upper.tri(D), arr.ind = TRUE)
  if (!nrow(idx)) return(igraph::make_empty_graph(n = 0, directed = FALSE))
  igraph::graph_from_data_frame(data.frame(from = as.character(idx[, 1]),
    to = as.character(idx[, 2])), directed = FALSE)
}

.iic_lconnect <- function(D, areas, max_dist, area_l) {
  g <- .build_lconnect_graph(D, max_dist)
  if (!igraph::vcount(g)) return(0)
  vertex <- as.integer(igraph::vertex_attr(g, "name"))
  paths <- igraph::distances(g, weights = NA)
  contribution <- outer(areas[vertex], areas[vertex]) / (1 + paths)
  contribution[!is.finite(contribution)] <- 0
  sum(contribution) / area_l^2
}

.awf_kernel <- function(D, max_dist) {
  out <- exp(-(-log(0.5) / (max_dist / 2)) * D)
  diag(out) <- 0
  out
}

.awf_lconnect <- function(D, areas, max_dist) {
  sum(areas * as.numeric(.awf_kernel(D, max_dist) %*% areas))
}

.patch_imp_engine <- function(D, areas, max_dist, area_l, metric, parallel = FALSE) {
  if (length(areas) < 2L) return(rep(NaN, length(areas)))
  if (metric == "AWF") {
    weighted <- as.numeric(.awf_kernel(D, max_dist) %*% areas)
    full <- sum(areas * weighted)
    if (!is.finite(full) || full == 0) return(rep(NaN, length(areas)))
    return(100 * (2 * areas * weighted) / full)
  }
  full <- .iic_lconnect(D, areas, max_dist, area_l)
  if (parallel && !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Instale future.apply para parallel = TRUE.")
  }
  if (!is.finite(full) || full == 0) return(rep(NaN, length(areas)))
  remove_one <- function(i) {
    keep <- setdiff(seq_along(areas), i)
    remaining <- if (length(keep)) .iic_lconnect(D[keep, keep, drop = FALSE],
      areas[keep], max_dist, area_l) else 0
    100 * (full - remaining) / full
  }
  if (parallel) future.apply::future_sapply(seq_along(areas), remove_one, future.seed = TRUE)
    else vapply(seq_along(areas), remove_one, numeric(1))
}

# All lconnect indices without repeated geometry or distance calculations.
.lconnect_metrics_fast <- function(D, areas, max_dist, area_l) {
  n <- length(areas)
  inclusive <- D <= max_dist
  diag(inclusive) <- FALSE
  clusters <- igraph::components(igraph::graph_from_adjacency_matrix(
    inclusive * 1, mode = "undirected", diag = FALSE))$membership
  component_areas <- as.numeric(rowsum(areas, clusters))
  paths <- igraph::distances(.build_lconnect_graph(D, max_dist), weights = NA)
  connected <- paths[upper.tri(paths) & is.finite(paths)]
  c(NC = length(component_areas), LNK = sum(D[upper.tri(D)] < max_dist),
    SLC = max(component_areas), MSC = sum(areas) / length(component_areas),
    CCP = sum((component_areas / sum(areas))^2), LCP = sum((component_areas / area_l)^2),
    CPL = if (length(connected)) mean(connected) else NA_real_,
    ECS = sum(component_areas^2) / sum(areas), AWF = .awf_lconnect(D, areas, max_dist),
    IIC = .iic_lconnect(D, areas, max_dist, area_l))
}
