#' Association between two land-cover series
#'
#' Compares two freely chosen classes: plots both series and measures their
#' association with significance. Detailed analyses can start from the
#' returned `data` table.
#' @param object A prepared `biomastats_time_series` object or native input.
#' @param series_x,series_y Two distinct class codes.
#' @param transformation Association between observed `"level"`s, annual
#'   `"difference"`s, or `"detrended"` residuals of separate linear trends.
#' @param method `"pearson"`, `"spearman"` or `"kendall"`.
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_series_correlation` object with `data` (year and both
#'   areas), `paired` (values used in the association), `association` (one row:
#'   coefficient, `p_value`, and for Pearson `n_effective` and `p_serial`),
#'   dictionary, metadata and `plots` (`series`, `association`). `p_value`
#'   assumes independent years; `p_serial` uses the approximate AR(1) effective
#'   sample size n(1 - r_x r_y)/(1 + r_x r_y), capped at n.
#' @export
series_correlation <- function(object, series_x, series_y,
                               transformation = c("level", "difference", "detrended"),
                               method = c("pearson", "spearman", "kendall"),
                               language = c("pt", "en"), build_plot = TRUE) {
  transformation <- match.arg(transformation); method <- match.arg(method)
  language <- match.arg(language)
  obj <- .as_series_object(object)
  if (identical(as.character(series_x), as.character(series_y))) stop("series_x e series_y devem ser classes distintas.")
  data <- merge(.series_one(obj, series_x), .series_one(obj, series_y), by = "year", suffixes = c("_x", "_y"))
  if (any(!is.finite(as.matrix(data))) || any(diff(data$year) != 1))
    stop("As s\u00e9ries devem ter anos consecutivos e completos.")
  if (nrow(data) < 6) stop("S\u00e3o necess\u00e1rios pelo menos seis pares anuais.")
  paired <- data
  if (transformation == "difference")
    paired <- data.frame(year = data$year[-1], area_x = diff(data$area_x), area_y = diff(data$area_y))
  if (transformation == "detrended") {
    paired$area_x <- stats::residuals(stats::lm(area_x ~ year, data = data))
    paired$area_y <- stats::residuals(stats::lm(area_y ~ year, data = data))
  }
  out <- list(data = data, paired = paired,
    association = cbind(data.frame(series_x = as.numeric(series_x), series_y = as.numeric(series_y),
                                   transformation = transformation),
                        .series_assoc(paired$area_x, paired$area_y, method)),
    class_dictionary = obj$class_dictionary, metadata = obj$metadata, plots = NULL)
  class(out) <- "biomastats_series_correlation"
  if (isTRUE(build_plot)) out$plots <- .series_correlation_plots(out, language)
  out
}

# Coefficient, conventional test and, for Pearson, the AR(1) serial adjustment.
.series_assoc <- function(x, y, method) {
  n <- length(x)
  ct <- if (stats::sd(x) > 0 && stats::sd(y) > 0) suppressWarnings(stats::cor.test(x, y, method = method, exact = FALSE)) else NULL
  r <- if (is.null(ct)) NA_real_ else unname(ct$estimate)
  acf1 <- function(v) as.numeric(stats::acf(v, lag.max = 1, plot = FALSE)$acf[2])
  product <- if (method == "pearson" && is.finite(r)) acf1(x) * acf1(y) else NA_real_
  neff <- if (is.finite(product)) max(3, min(n, n * (1 - product) / (1 + product))) else NA_real_
  p_serial <- if (is.finite(neff))
    2 * stats::pt(-abs(r * sqrt((neff - 2) / max(1 - r^2, .Machine$double.eps))), df = neff - 2) else NA_real_
  data.frame(method = method, n = n, estimate = r, p_value = if (is.null(ct)) NA_real_ else ct$p.value,
             n_effective = neff, p_serial = p_serial)
}

.series_correlation_plots <- function(out, language = "pt") {
  a <- out$association
  keys <- as.character(c(a$series_x, a$series_y))
  ids <- .series_identity(out, keys, language)
  lab <- stats::setNames(ids$label, keys)
  long <- data.frame(year = rep(out$data$year, 2), area = c(out$data$area_x, out$data$area_y),
                     code = factor(rep(keys, each = nrow(out$data)), levels = keys))
  series <- ggplot2::ggplot(long, ggplot2::aes(year, area, colour = code)) +
    ggplot2::geom_line(linewidth = .9) +
    ggplot2::geom_point(shape = 21, fill = "white", size = 1.8, stroke = .8) +
    ggplot2::facet_wrap(~code, ncol = 1, scales = "free_y", labeller = ggplot2::as_labeller(lab)) +
    ggplot2::scale_colour_manual(values = stats::setNames(.series_ink(ids$colour), keys), labels = lab,
                                 name = .tr(language, "Classe", "Class")) +
    .series_theme() +
    ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .series_area_label(out$metadata, language))

  coef <- c(pearson = "r", spearman = "\u03c1", kendall = "\u03c4")[a$method]
  tr <- c(level = .tr(language, "n\u00edveis", "levels"),
          difference = .tr(language, "primeiras diferen\u00e7as", "first differences"),
          detrended = .tr(language, "res\u00edduos sem tend\u00eancia linear", "linearly detrended residuals"))[a$transformation]
  stat <- sprintf("%s %s = %.2f \u00b7 p = %.3g", tools::toTitleCase(a$method), coef, a$estimate, a$p_value)
  if (is.finite(a$p_serial)) stat <- paste0(stat, sprintf(.tr(language,
    " \u00b7 p serial = %.3g (n efetivo = %.1f)", " \u00b7 serial p = %.3g (effective n = %.1f)"), a$p_serial, a$n_effective))
  p <- out$paired
  association <- ggplot2::ggplot(p, ggplot2::aes(area_x, area_y)) +
    ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "grey30", fill = "grey85",
                         linewidth = .7, linetype = "22") +
    ggplot2::geom_point(ggplot2::aes(fill = year), shape = 21, colour = "grey15", size = 2.8, stroke = .5) +
    ggplot2::scale_fill_viridis_c(name = .tr(language, "Ano", "Year")) +
    .series_theme() + ggplot2::theme(legend.key.width = ggplot2::unit(3, "lines")) +
    ggplot2::labs(x = lab[[1]], y = lab[[2]],
      subtitle = paste0(.tr(language, "Associa\u00e7\u00e3o entre ", "Association between "), tr, "\n", stat),
      caption = .tr(language,
        "p assume anos independentes; p serial corrige a autocorrela\u00e7\u00e3o AR(1) (Pearson). Reta apenas descritiva.",
        "p assumes independent years; serial p corrects AR(1) autocorrelation (Pearson). Line is descriptive only."))
  list(series = series, association = association)
}
