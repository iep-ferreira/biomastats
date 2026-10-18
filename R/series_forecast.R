#' Forecast one selected land-cover regime model
#' @param fit A fitted model returned by [series_fit_regime()].
#' @param horizon Number of annual observations to project.
#' @param level Conditional Gaussian prediction interval coverage.
#' @param language Plot labels, `"pt"` or `"en"`; defaults to the fit language.
#' @param build_plot Return an editable ggplot.
#' @return A `biomastats_series_forecast` object with `projection` and `plot`.
#' The projection extends the last fitted regime, remains conditional on the
#' selected model and its transitions, and does not reselect it. Negative
#' predictions are flagged, never clipped.
#' @export
series_forecast <- function(fit, horizon = 5L, level = .95, language = NULL, build_plot = TRUE) {
  if (!inherits(fit, "biomastats_series_fit")) stop("fit deve ser retornado por series_fit_regime().")
  if (!is.numeric(horizon) || length(horizon) != 1L || horizon < 0) stop("horizon deve ser inteiro n\u00e3o negativo.")
  if (!is.numeric(level) || level <= 0 || level >= 1) stop("level deve estar entre zero e um.")
  language <- language %||% fit$language %||% "pt"
  yrs <- if (horizon) max(fit$fitted$year) + seq_len(as.integer(horizon)) else numeric()
  X <- .regime_design(yrs, fit$fit)
  pred <- as.numeric(X %*% fit$fit$coef)
  se <- sqrt(pmax(0, rowSums((X %*% fit$fit$vcov) * X) + fit$fit$sigma2))
  z <- stats::qnorm((1 + level) / 2)
  out <- list(series = fit$series, model = fit$model, breaks = fit$breaks, level = level,
    projection = data.frame(year = yrs, prediction = pred, lower = pred - z * se, upper = pred + z * se,
                            negative = pred < 0),
    class_dictionary = fit$class_dictionary, metadata = fit$metadata, plot = NULL)
  class(out) <- "biomastats_series_forecast"
  if (isTRUE(build_plot)) out$plot <- .series_forecast_plot(fit, out, language)
  out
}


.series_forecast_plot <- function(fit, out, language = "pt") {
  if (!nrow(out$projection)) return(.series_fit_plot(fit, language))
  last <- max(fit$fitted$year); end <- max(out$projection$year)
  p <- .series_fit_plot(fit, language, xlim = c(min(fit$fitted$year), end))
  colour <- .series_identity(fit)$colour
  curve <- .series_curve(fit$fit, end); curve <- curve[curve$year >= last, ]
  p + ggplot2::annotate("rect", xmin = last, xmax = end, ymin = -Inf, ymax = Inf, fill = "grey60", alpha = .07) +
    ggplot2::geom_ribbon(data = out$projection, ggplot2::aes(year, ymin = lower, ymax = upper),
                         fill = colour, alpha = .22) +
    ggplot2::geom_line(data = curve, ggplot2::aes(year, fitted), colour = colour, linetype = "42", linewidth = 1.1) +
    ggplot2::geom_point(data = out$projection, ggplot2::aes(year, prediction), colour = colour, size = 1.8) +
    ggplot2::labs(caption = .tr(language,
      paste0("\u00c1rea sombreada: proje\u00e7\u00e3o. Faixa: intervalo preditivo condicional de ", 100 * out$level,
             "%; n\u00e3o inclui a incerteza das transi\u00e7\u00f5es nem da escolha do modelo."),
      paste0("Shaded area: projection. Ribbon: ", 100 * out$level,
             "% conditional prediction interval; excludes transition and model-selection uncertainty.")))
}
