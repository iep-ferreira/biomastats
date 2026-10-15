#' Fit a continuous land-cover regime model with one or more transitions
#'
#' Fits one selected series at a time. `segmented` is a continuous broken-stick;
#' `bent_cable` is the quadratic bent-cable of Chiu and Lockhart;
#' `prob_bent_uniform` and `prob_bent_logistic` are expected bent-cables with
#' uniform (U-BC) and logistic (L-BC) random thresholds (Ferreira and Zocchi,
#' 2020). The quadratic bent-cable and U-BC share the same mean function; U-BC
#' additionally reports the recovered threshold kernel.
#' @param object A prepared `biomastats_time_series` object or native input.
#' @param series A single class code.
#' @param model One model, or `"auto"` to compare all models by `criterion`.
#' @param breaks Number of transitions. `NULL` (default) selects 0 to
#'   `max_breaks` by `criterion`; zero is the linear trend.
#' @param max_breaks Largest number of transitions considered when `breaks = NULL`.
#' @param criterion `"AICc"` (default), `"AIC"` or `"BIC"`.
#' @param min_separation Minimum distance in years between transition centres.
#' @param bootstrap Number of bootstrap refits for change-point intervals.
#' @param bootstrap_type `"residual"` (i.i.d. residuals) or `"block"` (moving
#'   blocks of residuals, preserving short-range autocorrelation).
#' @param level Confidence level for the change points.
#' @param seed Optional reproducibility seed.
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_series_fit` object with `model_table` (all candidates),
#' `coefficients`, `fitted`, `curve` (model evaluated on a dense interior grid,
#' without creating observations), `change_points` (point estimate, bootstrap
#' interval, scale, 10--90 percent transition band and slopes per transition),
#' `kernels` (recovered threshold density and CDF per transition),
#' `transition_bands`, `bootstrap`, dictionary, metadata, `plot` and
#' `interval_plot`. Transition bands are threshold quantiles, not confidence
#' intervals. Information criteria count coefficients, centres, scales and
#' variance. Bootstrap intervals are conditional on the number of transitions.
#' @export
series_fit_regime <- function(object, series,
                              model = c("segmented", "linear", "bent_cable", "prob_bent_uniform",
                                        "prob_bent_logistic", "auto"),
                              breaks = NULL, max_breaks = 3L, criterion = c("AICc", "AIC", "BIC"),
                              min_separation = 3, bootstrap = 200L,
                              bootstrap_type = c("residual", "block"), level = .95, seed = NULL,
                              language = c("pt", "en"), build_plot = TRUE) {
  model <- match.arg(model); criterion <- match.arg(criterion)
  bootstrap_type <- match.arg(bootstrap_type); language <- match.arg(language)
  if (!is.numeric(bootstrap) || length(bootstrap) != 1L || bootstrap < 0) stop("bootstrap deve ser inteiro não negativo.")
  if (!is.numeric(level) || length(level) != 1L || level <= 0 || level >= 1) stop("level deve estar entre zero e um.")
  if (!is.null(breaks) && (!is.numeric(breaks) || length(breaks) != 1L || breaks < 0)) stop("breaks deve ser NULL ou inteiro não negativo.")
  if (!is.numeric(max_breaks) || length(max_breaks) != 1L || max_breaks < 0) stop("max_breaks deve ser inteiro não negativo.")
  obj <- .as_series_object(object)
  d <- .series_one(obj, series); d <- d[is.finite(d$area), ]
  if (nrow(d) < 6) stop("A série precisa de pelo menos seis observações completas.")
  if (stats::var(d$area) == 0) stop("A série é constante; não há modelo de mudança para ajustar.")

  origin <- min(d$year); t <- d$year - origin
  models <- if (model == "auto") c("segmented", "bent_cable", "prob_bent_uniform", "prob_bent_logistic") else model
  ks <- if (!is.null(breaks)) as.integer(breaks) else 0:as.integer(max_breaks)
  fits <- list()
  for (m in models) for (k in ks) {
    if (m == "linear" && k > 0) next
    f <- .fit_regime_k(t, d$area, if (k == 0) "linear" else m, k, min_sep = min_separation)
    if (!is.null(f)) { f$family <- m; fits[[length(fits) + 1L]] <- f }
  }
  fits <- fits[!duplicated(vapply(fits, function(f) paste(f$model, f$breaks), ""))]
  if (!length(fits)) stop("Nenhum modelo de regime convergiu.")
  tab <- do.call(rbind, lapply(fits, function(f) data.frame(model = f$model, breaks = f$breaks,
    parameters = f$n_par, rss = f$rss, rmse = f$rmse, r_squared = f$r_squared,
    AIC = f$aic, AICc = f$aicc, BIC = f$bic, converged = f$converged)))
  best <- which.min(tab[[criterion]]); tab$selected <- seq_len(nrow(tab)) == best
  selected <- fits[[best]]; selected$origin <- origin

  if (!is.null(seed)) set.seed(seed)
  boots <- .bootstrap_change(t, d$area, selected, as.integer(bootstrap), bootstrap_type, min_separation)
  out <- list(series = as.numeric(series), model = selected$model, breaks = selected$breaks,
    criterion = criterion, model_table = tab,
    coefficients = data.frame(term = names(selected$coef), estimate = as.numeric(selected$coef)),
    fitted = data.frame(year = d$year, observed = d$area, fitted = selected$fitted, residual = selected$residual),
    change_points = NULL, bootstrap = boots, bootstrap_type = bootstrap_type, level = level,
    class_dictionary = obj$class_dictionary, metadata = obj$metadata, fit = selected,
    language = language, plot = NULL, interval_plot = NULL)
  out$change_points <- .change_table(selected, boots, level)
  class(out) <- "biomastats_series_fit"
  out <- .series_enrich_fit(out)
  if (isTRUE(build_plot)) {
    out$plot <- .series_fit_plot(out, language)
    out$interval_plot <- .series_interval_plot(out, language)
  }
  out
}


# ---- Continuous regime models ----------------------------------------------

.h_uniform <- function(x, tau, scale) ifelse(x < tau - scale, 0, ifelse(x > tau + scale, x - tau, (x - tau + scale)^2 / (4 * scale)))
.softplus <- function(z) ifelse(z > 30, z, log1p(exp(z)))
.h_logistic <- function(x, tau, scale) scale * .softplus((x - tau) / scale)
.regime_has_scale <- function(model) model %in% c("bent_cable", "prob_bent_uniform", "prob_bent_logistic")

.regime_matrix <- function(t, taus, scales, model) {
  X <- cbind(1, t)
  for (j in seq_along(taus)) X <- cbind(X, switch(model,
    segmented = pmax(0, t - taus[j]),
    prob_bent_logistic = .h_logistic(t, taus[j], scales[j]),
    .h_uniform(t, taus[j], scales[j])))
  colnames(X) <- c("(Intercept)", "year", if (length(taus)) paste0("change", seq_along(taus)))
  X
}

# Evaluates the model at calendar years.
.regime_design <- function(year, fit) .regime_matrix(year - fit$origin, fit$taus, fit$scales, fit$model)

# Ordered centres from unconstrained u, keeping min_sep between neighbours.
.taus_from_u <- function(u, lo, hi, sep) {
  K <- length(u); taus <- numeric(K); a <- lo
  for (j in seq_len(K)) {
    b <- hi - (K - j) * sep
    if (b <= a) return(NULL)
    taus[j] <- a + (b - a) * stats::plogis(u[j]); a <- taus[j] + sep
  }
  taus
}

.u_from_taus <- function(taus, lo, hi, sep) {
  K <- length(taus); u <- numeric(K); a <- lo
  for (j in seq_len(K)) {
    b <- hi - (K - j) * sep
    p <- min(max((taus[j] - a) / (b - a), 1e-4), 1 - 1e-4)
    u[j] <- stats::qlogis(p); a <- taus[j] + sep
  }
  u
}

.regime_finalize <- function(t, y, model, taus, scales, converged) {
  X <- .regime_matrix(t, taus, scales, model)
  z <- stats::lm.fit(X, y)
  if (z$rank < ncol(X)) return(NULL)
  n <- length(y); K <- length(taus); rss <- sum(z$residuals^2)
  p <- ncol(X) + K + if (.regime_has_scale(model)) K else 0L
  p <- p + 1L # residual variance
  s2 <- rss / n; aic <- n * log(s2) + 2 * p
  list(model = model, breaks = K, x = t, y = y, taus = taus,
       scales = if (.regime_has_scale(model)) scales else rep(NA_real_, K),
       coef = z$coefficients, fitted = as.numeric(X %*% z$coefficients), residual = as.numeric(z$residuals),
       rss = rss, rmse = sqrt(s2), r_squared = 1 - rss / sum((y - mean(y))^2), n_par = p,
       aic = aic, aicc = if (n > p + 1) aic + 2 * p * (p + 1) / (n - p - 1) else Inf,
       bic = n * log(s2) + log(n) * p, sigma2 = rss / max(1, n - ncol(X)),
       sigma = sqrt(rss / max(1, n - ncol(X))), vcov = rss / max(1, n - ncol(X)) * solve(crossprod(X)),
       converged = converged)
}

# Profiled least squares: linear coefficients are exact given centres and scales;
# centres and scales are refined by bounded quasi-Newton from grid starts.
.fit_regime_k <- function(t, y, model, K, start = NULL, min_sep = 3, edge = 2) {
  if (model == "linear" || K == 0) return(.regime_finalize(t, y, "linear", numeric(), numeric(), TRUE))
  lo <- min(t) + edge; hi <- max(t) - edge
  if (hi - lo <= (K - 1) * min_sep) return(NULL)
  has_scale <- .regime_has_scale(model)
  srange <- c(.25, max(1, diff(range(t)) / 4))
  s0 <- min(max(diff(range(t)) * .06, srange[1]), 2.5)
  rss <- function(taus, scales) {
    X <- .regime_matrix(t, taus, scales, model)
    z <- stats::lm.fit(X, y)
    if (z$rank < ncol(X)) Inf else sum(z$residuals^2)
  }
  if (is.null(start)) {
    grid <- seq(lo, hi, length.out = 14)
    combos <- if (K == 1) matrix(grid, nrow = 1) else utils::combn(grid, K)
    combos <- combos[, apply(combos, 2, function(v) all(diff(v) >= min_sep)), drop = FALSE]
    if (!ncol(combos)) return(NULL)
    score <- apply(combos, 2, function(v) rss(v, rep(s0, K)))
    keep <- order(score)[seq_len(min(6L, sum(is.finite(score))))]
    starts <- lapply(keep, function(i) list(taus = combos[, i], scales = rep(s0, K)))
  } else starts <- list(start)
  objective <- function(u) {
    taus <- .taus_from_u(u[seq_len(K)], lo, hi, min_sep)
    if (is.null(taus)) return(1e20)
    v <- rss(taus, if (has_scale) exp(u[K + seq_len(K)]) else rep(NA_real_, K))
    if (is.finite(v)) v else 1e20
  }
  best <- NULL
  for (s in starts) {
    u0 <- c(.u_from_taus(s$taus, lo, hi, min_sep),
            if (has_scale) log(pmin(pmax(s$scales, srange[1]), srange[2])))
    op <- tryCatch(stats::optim(u0, objective, method = "L-BFGS-B",
      lower = c(rep(-8, K), if (has_scale) rep(log(srange[1]), K)),
      upper = c(rep(8, K), if (has_scale) rep(log(srange[2]), K)),
      control = list(maxit = 300)), error = function(e) NULL)
    if (is.null(op) || op$value >= 1e19) next
    if (is.null(best) || op$value < best$value) best <- op
  }
  if (is.null(best)) return(NULL)
  taus <- .taus_from_u(best$par[seq_len(K)], lo, hi, min_sep)
  scales <- if (has_scale) exp(best$par[K + seq_len(K)]) else rep(NA_real_, K)
  .regime_finalize(t, y, model, taus, scales, best$convergence == 0)
}

.bootstrap_change <- function(t, y, fit, B, type, min_sep) {
  empty <- data.frame(replicate = integer(), transition = integer(), year = numeric(), scale = numeric())
  if (!B || fit$breaks == 0) return(empty)
  n <- length(y); e <- fit$residual - mean(fit$residual); block <- max(2L, round(n^(1 / 3)))
  draw <- function() {
    if (type == "residual") return(sample(e, n, replace = TRUE))
    starts <- sample.int(n - block + 1L, ceiling(n / block), replace = TRUE)
    unlist(lapply(starts, function(s) e[s:(s + block - 1L)]))[seq_len(n)]
  }
  start <- list(taus = fit$taus, scales = fit$scales)
  rows <- lapply(seq_len(B), function(b) {
    z <- tryCatch(.fit_regime_k(t, fit$fitted + draw(), fit$model, fit$breaks, start = start, min_sep = min_sep),
                  error = function(err) NULL)
    if (is.null(z)) return(NULL)
    data.frame(replicate = b, transition = seq_len(fit$breaks), year = fit$origin + z$taus, scale = z$scales)
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows)) do.call(rbind, rows) else empty
}

.threshold_quantile <- function(p, tau, scale, model) {
  if (model == "prob_bent_logistic") stats::qlogis(p, tau, scale) else tau + (2 * p - 1) * scale
}

.change_table <- function(fit, boots, level) {
  K <- fit$breaks
  if (!K) return(data.frame())
  a <- (1 - level) / 2
  q <- function(v) if (sum(is.finite(v)) >= 20) stats::quantile(v, c(a, .5, 1 - a), na.rm = TRUE, names = FALSE) else rep(NA_real_, 3)
  slopes <- fit$coef[2] + c(0, cumsum(fit$coef[-(1:2)]))
  do.call(rbind, lapply(seq_len(K), function(j) {
    b <- boots[boots$transition == j, ]
    tq <- q(b$year); sq <- q(b$scale)
    centre <- fit$origin + fit$taus[j]
    band <- if (.regime_has_scale(fit$model))
      .threshold_quantile(c(.1, .9), centre, fit$scales[j], if (fit$model == "bent_cable") "uniform" else fit$model) else c(centre, centre)
    data.frame(transition = j, estimate = centre, conf_low = tq[1], conf_median = tq[2], conf_high = tq[3],
               scale = fit$scales[j], scale_low = sq[1], scale_high = sq[3],
               band_low = band[1], band_high = band[2],
               slope_before = unname(slopes[j]), slope_after = unname(slopes[j + 1]), level = level,
               bootstrap_n = sum(is.finite(b$year)))
  }))
}
