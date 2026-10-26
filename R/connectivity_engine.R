# Shared connectivity engine for patch_imp_fast() and land_connectivity():
# IIC, PC and AWF with the Saura & Rubio (2010) decomposition.
#
# IIC and PC share the form I = sum_ij a_i a_j M_ij / A_L^2 with M_ii = 1:
#   IIC: M_ij = 1 / (1 + nl_ij), nl = steps in the graph d_ij < max_dist;
#   PC:  M_ij = p*_ij = exp(-k d*_ij), d* = shortest sum of distances.
# The sum covers every patch, including i = j and isolates
# (Pascual-Hortal & Saura 2006; Saura & Pascual-Hortal 2007).

.connectivity_check_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) stop(paste(name, "deve ser logico escalar."))
}

.connectivity_metres <- function(crs) {
  if (is.null(crs$ud_unit)) return(FALSE)
  tryCatch(isTRUE(all.equal(as.numeric(units::set_units(
    units::set_units(1, crs$ud_unit, mode = "standard"), "m")), 1)), error = function(e) FALSE)
}

# Decay constant of p_ij = exp(-k d_ij): k itself, or probability p_ref at
# distance d_ref. The default reproduces lconnect: p = 0.5 at max_dist / 2.
.calibrate_k <- function(max_dist, k = NULL, p_ref = 0.5, d_ref = NULL) {
  if (!is.null(k)) {
    if (!is.numeric(k) || length(k) != 1L || !is.finite(k) || k <= 0) {
      stop("k deve ser um numero positivo.")
    }
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

# Validates `parallel`: FALSE, TRUE (user's future plan) or a number of
# processes (a temporary multisession plan opened by the caller).
.connectivity_check_parallel <- function(parallel) {
  ok <- (is.logical(parallel) && length(parallel) == 1L && !is.na(parallel)) ||
    (is.numeric(parallel) && length(parallel) == 1L && is.finite(parallel) &&
       parallel >= 1 && parallel == trunc(parallel))
  if (!ok) stop("parallel deve ser TRUE, FALSE ou um numero inteiro de processos.")
  if (!isFALSE(parallel) && !requireNamespace("future.apply", quietly = TRUE)) {
    stop("Instale future.apply para usar parallel.")
  }
}

# Whether PC edges are pruned. Pruning only applies to PC (every binary IIC edge
# is already a shortest path). "auto" prunes reasonably dense networks: at least
# `min_n` patches and a fraction `min_density` of pairs with a finite link;
# below that, the Theta(n^3) pass costs more than it saves.
.resolve_prune <- function(prune, metric, D, min_n = 50L, min_density = 0.25) {
  if (!identical(prune, "auto") && !(is.logical(prune) && length(prune) == 1L && !is.na(prune))) {
    stop("prune deve ser \"auto\", TRUE ou FALSE.")
  }
  if (metric != "PC") return(FALSE)
  if (is.logical(prune)) return(prune)
  nrow(D) >= min_n && mean(is.finite(D[upper.tri(D)])) >= min_density
}

# Splits 1:n into about four blocks per worker.
.chunks <- function(n, workers) {
  size <- max(1L, ceiling(n / (4L * workers)))
  split(seq_len(n), ceiling(seq_len(n) / size))
}

# Applies f to blocks of 1:n, sequentially or with future.apply on the current plan.
.map_chunks <- function(n, f, parallel = FALSE, simplify = TRUE) {
  out <- if (isFALSE(parallel)) lapply(list(seq_len(n)), f)
    else future.apply::future_lapply(.chunks(n, future::nbrOfWorkers()), f, future.seed = NULL)
  if (simplify) unlist(out, use.names = FALSE) else unlist(out, recursive = FALSE)
}

# Useful edges of the complete PC graph. Edge i-j is dropped when at least two
# patches m offer a strictly shorter detour, d_im + d_mj < d_ij: after removing
# any single r a detour remains, so the edge is never on a shortest path of
# G or G - r. Theta(n^3) once; in parallel, each worker counts a block of m.
.pc_edges <- function(D, parallel = FALSE) {
  count_block <- function(ms) {
    shortcuts <- matrix(0L, nrow(D), ncol(D))
    for (m in ms) shortcuts <- shortcuts + (outer(D[, m], D[m, ], "+") < D)
    list(shortcuts)
  }
  Reduce(`+`, .map_chunks(nrow(D), count_block, parallel, simplify = FALSE)) < 2L
}

# Graph with every patch as a vertex, isolates included.
# IIC: binary edge d_ij < max_dist, contacts (d = 0) included.
# PC: edge weighted by d_ij for every finite distance; dominated edges are
# dropped when `prune` is TRUE.
.path_graph <- function(D, metric, max_dist, prune = FALSE, parallel = FALSE) {
  keep <- upper.tri(D) & if (metric == "IIC") D < max_dist else is.finite(D)
  if (metric == "PC" && prune) keep <- keep & .pc_edges(D, parallel)
  idx <- which(keep, arr.ind = TRUE)
  g <- igraph::make_graph(as.vector(t(idx)), n = nrow(D), directed = FALSE)
  if (metric == "PC") igraph::E(g)$weight <- D[idx]
  g
}

# Best-path lengths: steps (IIC) or summed distance (PC).
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

# Index of a path graph without the A_L^2 denominator (IIC, PC).
.graph_index_sum <- function(g, areas, metric, k = NULL) {
  M <- .affinity(.path_lengths(g, metric), metric, k)
  sum(areas * as.numeric(M %*% areas))
}

# Patches i != r with some best path through r (L_ir + L_rj = L_ij). Only
# pairs with both ends in this set can lose affinity when r is removed.
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

# dI_r = intra + flux + connector (Saura & Rubio 2010):
#   intra_r = a_r^2, flux_r = 2 a_r sum_{i != r} a_i M_ir,
#   connector_r = sum_{i,j != r} a_i a_j (M_ij - M^(-r)_ij).
# Intra and flux are closed-form; the connector is recomputed only for
# candidates and only between the origins and destinations routed through r.
# The candidate test and connector of each r are independent: they dominate the
# time and are what `parallel` distributes, in blocks of r.
.decomposed_importance <- function(D, areas, max_dist, metric, k = NULL, parallel = FALSE,
                                   prune = FALSE) {
  g <- .path_graph(D, metric, max_dist, prune, parallel)
  L <- .path_lengths(g, metric)
  M <- .affinity(L, metric, k)
  Ma <- as.numeric(M %*% areas)
  total <- sum(areas * Ma)
  tol <- if (metric == "IIC") 0 else 1e-9
  # Connector of each r in a block; NA when r is not a candidate (|S_r| < 2).
  connector_block <- function(rs) {
    vapply(rs, function(r) {
      s <- .through_set(L, r, tol)
      if (length(s) < 2L) return(NA_real_)
      s_r <- s - (s > r)  # vertex ids shift after deleting r
      M_r <- .affinity(.path_lengths(igraph::delete_vertices(g, r), metric, from = s_r, to = s_r),
                       metric, k)
      sum(outer(areas[s], areas[s]) * (M[s, s, drop = FALSE] - M_r))
    }, numeric(1))
  }
  connector <- .map_chunks(length(areas), connector_block, parallel)
  candidates <- sum(!is.na(connector))
  connector[is.na(connector)] <- 0
  fractions <- data.frame(intra = 100 * areas^2 / total,
                          flux = 100 * 2 * areas * (Ma - areas) / total,
                          connector = 100 * connector / total)
  list(importance = unname(rowSums(fractions)), fractions = fractions, index = total,
       candidates = candidates, edges = igraph::ecount(g))
}

# Explicit removal, the validation reference. The (pruned or full) graph is
# built once and each removal deletes one vertex: by the two-witness rule,
# pruning stays exact in G - r.
.removal_importance <- function(D, areas, max_dist, metric, k = NULL, prune = FALSE) {
  g <- .path_graph(D, metric, max_dist, prune)
  full <- .graph_index_sum(g, areas, metric, k)
  imp <- vapply(seq_along(areas), function(r) {
    100 * (full - .graph_index_sum(igraph::delete_vertices(g, r), areas[-r], metric, k)) / full
  }, numeric(1))
  list(importance = imp, fractions = NULL, index = full, candidates = NA_integer_,
       edges = igraph::ecount(g))
}

# IIC of lconnect 0.1.2 (R/con_metric.R, lines 188-194): drops isolates and
# zero-distance contacts. Kept only for compatibility (iic_rule = "lconnect").
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

.iic_lconnect_importance <- function(D, areas, max_dist, area_l) {
  full <- .iic_lconnect(D, areas, max_dist, area_l)
  imp <- vapply(seq_along(areas), function(r) {
    keep <- seq_along(areas)[-r]
    100 * (full - .iic_lconnect(D[keep, keep, drop = FALSE], areas[keep], max_dist, area_l)) / full
  }, numeric(1))
  list(importance = imp, fractions = NULL, index = full, candidates = NA_integer_)
}

.awf_importance <- function(D, areas, k) {
  weighted <- as.numeric(.awf_kernel(D, k) %*% areas)
  full <- sum(areas * weighted)
  imp <- 100 * (2 * areas * weighted) / full
  list(importance = imp, fractions = data.frame(intra = 0, flux = imp, connector = 0),
       index = full, candidates = 0L)
}

# Importance of every patch. Returns importance (%), fractions (intra, flux,
# connector as % of the index, or NULL), index (on the index scale), k,
# method, candidates (number of connector candidates), pruned and edges (edges
# of the path graph). `parallel` is FALSE or TRUE (the plan is already open).
.patch_imp_engine <- function(D, areas, max_dist, area_l, metric, parallel = FALSE,
                              method = "decomposition", iic_rule = "definition", k = NULL,
                              prune = "auto") {
  if (metric == "AWF") method <- "closed_form"
  if (metric == "IIC" && iic_rule == "lconnect") method <- "removal"
  pruned <- .resolve_prune(prune, metric, D)
  res <- if (metric == "AWF") {
    .awf_importance(D, areas, k)
  } else if (metric == "IIC" && iic_rule == "lconnect") {
    .iic_lconnect_importance(D, areas, max_dist, area_l)
  } else {
    out <- if (method == "removal") .removal_importance(D, areas, max_dist, metric, k, pruned)
      else .decomposed_importance(D, areas, max_dist, metric, k, parallel, pruned)
    out$index <- out$index / area_l^2
    out
  }
  res$pruned <- pruned
  if (is.null(res$edges)) res$edges <- NA_integer_
  # One patch has no relative ranking; a zero index makes every ratio undefined.
  if (length(areas) < 2L || !is.finite(res$index) || res$index == 0) {
    res$importance <- rep(NaN, length(areas))
    res$fractions <- NULL
    res$candidates <- NA_integer_
  }
  res$k <- if (metric == "IIC") NA_real_ else k
  res$method <- method
  res
}

# pimp object with the lconnect contract plus fractions and engine attributes.
.as_pimp <- function(landscape, res, metric, iic_rule) {
  p <- landscape$landscape
  if (!"fragment_id" %in% names(p)) {
    p$fragment_id <- if (!is.null(landscape$fragment_id)) landscape$fragment_id
      else seq_len(nrow(p))
  }
  p$attributes <- res$importance
  status <- if (all(is.nan(res$importance))) "undefined_baseline" else "ok"
  structure(list(landscape = p, prioritization = res$importance, fractions = res$fractions),
    class = "pimp", metric = metric, status = status, index = res$index, k = res$k,
    method = res$method, iic_rule = if (metric == "IIC") iic_rule else NA_character_,
    connector_candidates = res$candidates, pruned = res$pruned, edges = res$edges)
}
