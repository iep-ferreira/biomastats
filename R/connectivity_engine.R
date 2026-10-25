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

# Useful edges of the complete PC graph. Edge i-j is dropped when at least two
# patches m offer a strictly shorter detour, d_im + d_mj < d_ij: after removing
# any single r a detour remains, so the edge is never on a shortest path of
# G or G - r. Theta(n^3) once.
.pc_edges <- function(D) {
  shortcuts <- matrix(0L, nrow(D), ncol(D))
  for (m in seq_len(nrow(D))) shortcuts <- shortcuts + (outer(D[, m], D[m, ], "+") < D)
  shortcuts < 2L
}

# Graph with every patch as a vertex, isolates included.
# IIC: binary edge d_ij < max_dist, contacts (d = 0) included.
# PC: pruned edges weighted by d_ij.
.path_graph <- function(D, metric, max_dist) {
  keep <- upper.tri(D) & if (metric == "IIC") D < max_dist else .pc_edges(D)
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

# Index without the A_L^2 denominator (IIC, PC) or AWF.
.index_sum <- function(D, areas, max_dist, metric, k = NULL) {
  if (!length(areas)) return(0)
  if (metric == "AWF") return(.awf_value(D, areas, k))
  M <- .affinity(.path_lengths(.path_graph(D, metric, max_dist), metric), metric, k)
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
    s_r <- s - (s > r)  # vertex ids shift after deleting r
    M_r <- .affinity(.path_lengths(igraph::delete_vertices(g, r), metric, from = s_r, to = s_r),
                     metric, k)
    sum(outer(areas[s], areas[s]) * (M[s, s, drop = FALSE] - M_r))
  }
  connector <- numeric(length(areas))
  if (length(candidates)) {
    connector[candidates] <- if (parallel) {
      future.apply::future_sapply(candidates, connector_of, future.seed = TRUE)
    } else vapply(candidates, connector_of, numeric(1))
  }
  fractions <- data.frame(intra = 100 * areas^2 / total,
                          flux = 100 * 2 * areas * (Ma - areas) / total,
                          connector = 100 * connector / total)
  list(importance = unname(rowSums(fractions)), fractions = fractions, index = total,
       candidates = length(candidates))
}

# Explicit removal, the validation reference: n + 1 index evaluations.
.removal_importance <- function(D, areas, max_dist, metric, k = NULL) {
  full <- .index_sum(D, areas, max_dist, metric, k)
  imp <- vapply(seq_along(areas), function(r) {
    100 * (full - .index_sum(D[-r, -r, drop = FALSE], areas[-r], max_dist, metric, k)) / full
  }, numeric(1))
  list(importance = imp, fractions = NULL, index = full, candidates = NA_integer_)
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
# method and candidates (number of connector candidates).
.patch_imp_engine <- function(D, areas, max_dist, area_l, metric, parallel = FALSE,
                              method = "decomposition", iic_rule = "definition", k = NULL) {
  if (metric == "AWF") method <- "closed_form"
  if (metric == "IIC" && iic_rule == "lconnect") method <- "removal"
  res <- if (metric == "AWF") {
    .awf_importance(D, areas, k)
  } else if (metric == "IIC" && iic_rule == "lconnect") {
    .iic_lconnect_importance(D, areas, max_dist, area_l)
  } else {
    out <- if (method == "removal") .removal_importance(D, areas, max_dist, metric, k)
      else .decomposed_importance(D, areas, max_dist, metric, k, parallel)
    out$index <- out$index / area_l^2
    out
  }
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
    connector_candidates = res$candidates)
}
