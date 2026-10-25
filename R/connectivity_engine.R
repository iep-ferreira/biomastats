# Motor de IIC, PC e AWF com a decomposicao de Saura & Rubio (2010).
#
# IIC e PC tem a forma I = sum_ij a_i a_j M_ij / A_L^2, com M_ii = 1:
#   IIC: M_ij = 1 / (1 + nl_ij), nl = passos no grafo d_ij < max_dist;
#   PC:  M_ij = p*_ij = exp(-k d*_ij), d* = menor soma de distancias.
# A soma inclui todos os fragmentos, inclusive i = j e isolados
# (Pascual-Hortal & Saura 2006; Saura & Pascual-Hortal 2007).

# Constante k de p_ij = exp(-k d_ij): k informado ou p_ref na distancia
# d_ref. O padrao reproduz o lconnect: p = 0,5 a max_dist / 2.
.calibrate_k <- function(max_dist, k = NULL, p_ref = 0.5, d_ref = NULL) {
  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) stop("k deve ser um numero positivo.")
    return(k)
  }
  if (is.null(d_ref)) d_ref <- max_dist / 2
  if (!is.numeric(p_ref) || length(p_ref) != 1L || !is.finite(p_ref) || p_ref <= 0 || p_ref >= 1) {
    stop("p_ref deve estar em (0, 1).")
  }
  if (!is.numeric(d_ref) || length(d_ref) != 1L || !is.finite(d_ref) || d_ref <= 0) {
    stop("d_ref deve ser uma distancia positiva.")
  }
  -log(p_ref) / d_ref
}

# Arestas uteis do grafo completo do PC. A aresta i-j e descartada quando
# ao menos dois fragmentos m oferecem desvio estritamente mais curto,
# d_im + d_mj < d_ij: removido qualquer r, ainda resta um desvio, logo a
# aresta nunca integra um melhor caminho de G nem de G - r. Theta(n^3).
.pc_edges <- function(D) {
  shortcuts <- matrix(0L, nrow(D), ncol(D))
  for (m in seq_len(nrow(D))) shortcuts <- shortcuts + (outer(D[, m], D[m, ], "+") < D)
  shortcuts < 2L
}

# Grafo com todos os fragmentos como vertices (isolados inclusive).
# IIC: aresta binaria d_ij < max_dist, inclusive contato (d = 0).
# PC: arestas podadas com peso d_ij.
.path_graph <- function(D, metric, max_dist) {
  keep <- upper.tri(D) & if (metric == "IIC") D < max_dist else .pc_edges(D)
  idx <- which(keep, arr.ind = TRUE)
  g <- igraph::make_graph(as.vector(t(idx)), n = nrow(D), directed = FALSE)
  if (metric == "PC") igraph::E(g)$weight <- D[idx]
  g
}

.path_lengths <- function(g, metric, from = igraph::V(g), to = igraph::V(g)) {
  if (metric == "IIC") igraph::distances(g, v = from, to = to, weights = NA)
  else igraph::distances(g, v = from, to = to)
}

.affinity <- function(L, metric, k) if (metric == "IIC") 1 / (1 + L) else exp(-k * L)

.awf_kernel <- function(D, k) {
  out <- exp(-k * D)
  diag(out) <- 0
  out
}

.awf_value <- function(D, areas, k) sum(areas * as.numeric(.awf_kernel(D, k) %*% areas))

# Indice sem A_L^2 (IIC/PC) ou AWF.
.index_sum <- function(D, areas, max_dist, metric, k = NULL) {
  if (!length(areas)) return(0)
  if (metric == "AWF") return(.awf_value(D, areas, k))
  M <- .affinity(.path_lengths(.path_graph(D, metric, max_dist), metric), metric, k)
  sum(areas * as.numeric(M %*% areas))
}

# Fragmentos i != r com algum melhor caminho por r: L_ir + L_rj = L_ij.
# Somente pares com i e j nesse conjunto perdem afinidade quando r sai.
.through_set <- function(L, r, tol) {
  lr <- L[, r]
  ok <- is.finite(lr)
  ok[r] <- FALSE
  idx <- which(ok)
  if (length(idx) < 2L) return(integer())
  via <- outer(lr[idx], lr[idx], "+")
  direct <- L[idx, idx, drop = FALSE]
  tight <- abs(via - direct) <= tol * pmax(1, direct)
  diag(tight) <- FALSE
  idx[rowSums(tight) > 0]
}

# dI_r = intra + fluxo + conector (Saura & Rubio 2010), com
#   intra_r = a_r^2, fluxo_r = 2 a_r sum_{i != r} a_i M_ir,
#   conector_r = sum_{i,j != r} a_i a_j (M_ij - M^(-r)_ij).
# Intra e fluxo tem forma fechada; o conector so e recalculado para
# candidatos e apenas entre as origens e destinos que passam por r.
.decomposed_importance <- function(D, areas, max_dist, metric, k = NULL, parallel = FALSE) {
  g <- .path_graph(D, metric, max_dist)
  L <- .path_lengths(g, metric)
  M <- .affinity(L, metric, k)
  Ma <- as.numeric(M %*% areas)
  total <- sum(areas * Ma)
  tol <- if (metric == "IIC") 0 else 1e-9
  through <- lapply(seq_along(areas), function(r) .through_set(L, r, tol))
  candidates <- which(lengths(through) >= 2L)
  connector_of <- function(r) {
    s <- through[[r]]
    M_r <- .affinity(.path_lengths(igraph::delete_vertices(g, r), metric,
      from = s - (s > r), to = s - (s > r)), metric, k)
    sum(outer(areas[s], areas[s]) * (M[s, s, drop = FALSE] - M_r))
  }
  connector <- numeric(length(areas))
  if (length(candidates)) {
    connector[candidates] <- if (parallel) {
      future.apply::future_sapply(candidates, connector_of, future.seed = TRUE)
    } else vapply(candidates, connector_of, numeric(1))
  }
  fractions <- data.frame(intra = 100 * areas^2 / total,
    flux = 100 * 2 * areas * (Ma - areas) / total, connector = 100 * connector / total)
  list(importance = unname(rowSums(fractions)), fractions = fractions, index = total,
       candidates = length(candidates))
}

# Remocao explicita, referencia de validacao: n + 1 indices.
.removal_importance <- function(D, areas, max_dist, metric, k = NULL) {
  full <- .index_sum(D, areas, max_dist, metric, k)
  imp <- vapply(seq_along(areas), function(r) {
    100 * (full - .index_sum(D[-r, -r, drop = FALSE], areas[-r], max_dist, metric, k)) / full
  }, numeric(1))
  list(importance = imp, fractions = NULL, index = full, candidates = NA_integer_)
}

# IIC do lconnect 0.1.2 (R/con_metric.R, linhas 188-194): omite fragmentos
# isolados e contatos a distancia zero. Apenas para compatibilidade.
.iic_lconnect <- function(D, areas, max_dist, area_l) {
  idx <- which(D > 0 & D < max_dist & upper.tri(D), arr.ind = TRUE)
  if (!nrow(idx)) return(0)
  g <- igraph::graph_from_data_frame(data.frame(from = as.character(idx[, 1]),
    to = as.character(idx[, 2])), directed = FALSE)
  vertex <- as.integer(igraph::vertex_attr(g, "name"))
  contribution <- outer(areas[vertex], areas[vertex]) / (1 + igraph::distances(g, weights = NA))
  contribution[!is.finite(contribution)] <- 0
  sum(contribution) / area_l^2
}

# Importancia de cada fragmento. Retorna importance (%), fractions
# (intra, flux, connector em % do indice, ou NULL), index (na escala do
# indice), k, method e candidates (numero de candidatos a conector).
.patch_imp_engine <- function(D, areas, max_dist, area_l, metric, parallel = FALSE,
                              method = "decomposition", iic_rule = "definition", k = NULL) {
  n <- length(areas)
  kk <- if (metric == "IIC") NA_real_ else k
  undefined <- function(index, method) list(importance = rep(NaN, n), fractions = NULL,
    index = index, k = kk, method = method, candidates = NA_integer_)
  if (metric == "AWF") {
    weighted <- as.numeric(.awf_kernel(D, k) %*% areas)
    full <- sum(areas * weighted)
    if (n < 2L || !is.finite(full) || full == 0) return(undefined(full, "closed_form"))
    imp <- 100 * (2 * areas * weighted) / full
    return(list(importance = imp, fractions = data.frame(intra = 0, flux = imp, connector = 0),
      index = full, k = kk, method = "closed_form", candidates = 0L))
  }
  if (metric == "IIC" && iic_rule == "lconnect") {
    full <- .iic_lconnect(D, areas, max_dist, area_l)
    if (n < 2L || !is.finite(full) || full == 0) return(undefined(full, "removal"))
    imp <- vapply(seq_len(n), function(i) {
      keep <- seq_len(n)[-i]
      100 * (full - .iic_lconnect(D[keep, keep, drop = FALSE], areas[keep], max_dist, area_l)) / full
    }, numeric(1))
    return(list(importance = imp, fractions = NULL, index = full, k = kk,
      method = "removal", candidates = NA_integer_))
  }
  if (n < 2L) return(undefined(if (n) 1 * areas^2 / area_l^2 else 0, method))
  res <- if (method == "removal") .removal_importance(D, areas, max_dist, metric, k)
    else .decomposed_importance(D, areas, max_dist, metric, k, parallel)
  res$index <- res$index / area_l^2
  res$k <- kk
  res$method <- method
  res
}

# Todos os indices de paisagem sem recalcular geometrias ou distancias.
# NC e componentes seguem o single linkage do lconnect (d <= max_dist).
.lconnect_metrics_fast <- function(D, areas, max_dist, area_l, k_awf = NULL, pc = NA_real_) {
  if (is.null(k_awf)) k_awf <- .calibrate_k(max_dist)
  inclusive <- D <= max_dist
  diag(inclusive) <- FALSE
  clusters <- igraph::components(igraph::graph_from_adjacency_matrix(
    inclusive * 1, mode = "undirected", diag = FALSE))$membership
  component_areas <- as.numeric(rowsum(areas, clusters))
  paths <- .path_lengths(.path_graph(D, "IIC", max_dist), "IIC")
  connected <- paths[upper.tri(paths) & is.finite(paths)]
  c(NC = length(component_areas), LNK = sum(D[upper.tri(D)] < max_dist),
    SLC = max(component_areas), MSC = sum(areas) / length(component_areas),
    CCP = sum((component_areas / sum(areas))^2), LCP = sum((component_areas / area_l)^2),
    CPL = if (length(connected)) mean(connected) else NA_real_,
    ECS = sum(component_areas^2) / sum(areas), AWF = .awf_value(D, areas, k_awf),
    IIC = sum(areas * as.numeric((1 / (1 + paths)) %*% areas)) / area_l^2,
    IIC_lconnect = .iic_lconnect(D, areas, max_dist, area_l), PC = pc)
}
