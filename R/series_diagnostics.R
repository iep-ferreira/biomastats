#' Diagnose residual autocorrelation and fit quality
#' @param fit A `biomastats_series_fit` object.
#' @param max_lag Maximum residual lag inspected.
#' @param language Plot labels, `"pt"` or `"en"`; defaults to the fit language.
#' @param build_plot Return editable diagnostic ggplots.
#' @return A `biomastats_series_diagnostics` object with `quality` (RMSE, MAE,
#' R2, information criteria, Durbin--Watson, lag-1 residual autocorrelation,
#' Ljung--Box and Shapiro--Wilk), residual ACF/PACF with approximate 95 percent
#' bounds, and plots (`reference`, `residuals`, `acf`, `pacf`, `qq`,
#' `observed_fitted`).
#' @export
series_diagnostics <- function(fit, max_lag = 10L, language = NULL, build_plot = TRUE) {
  if (!inherits(fit, "biomastats_series_fit")) stop("fit deve ser retornado por series_fit_regime().")
  language <- language %||% fit$language %||% "pt"
  e <- fit$fitted$residual; n <- length(e)
  lag_use <- min(as.integer(max_lag), max(1L, floor(n / 3)))
  lb <- if (n > lag_use + 2) stats::Box.test(e, lag = lag_use, type = "Ljung-Box", fitdf = 0) else NULL
  sw <- if (n >= 3 && n <= 5000 && stats::sd(e) > 0) stats::shapiro.test(e) else NULL
  ac <- stats::acf(e, lag.max = lag_use, plot = FALSE); pc <- stats::pacf(e, lag.max = lag_use, plot = FALSE)
  bound <- 1.96 / sqrt(n)
  out <- list(
    quality = data.frame(model = fit$model, breaks = fit$breaks, rmse = sqrt(mean(e^2)), mae = mean(abs(e)),
      r_squared = fit$fit$r_squared, AIC = fit$fit$aic, AICc = fit$fit$aicc, BIC = fit$fit$bic,
      durbin_watson = sum(diff(e)^2) / sum(e^2), residual_acf1 = as.numeric(ac$acf[2]),
      ljung_box_lag = lag_use,
      ljung_box_statistic = if (is.null(lb)) NA_real_ else unname(lb$statistic),
      ljung_box_p_value = if (is.null(lb)) NA_real_ else lb$p.value,
      shapiro_p_value = if (is.null(sw)) NA_real_ else sw$p.value),
    residuals = fit$fitted,
    acf = data.frame(lag = as.integer(ac$lag), acf = as.numeric(ac$acf), bound = bound),
    pacf = data.frame(lag = as.integer(pc$lag), pacf = as.numeric(pc$acf), bound = bound),
    series = fit$series, class_dictionary = fit$class_dictionary, metadata = fit$metadata, plots = NULL)
  class(out) <- "biomastats_series_diagnostics"
  if (isTRUE(build_plot)) {
    out$plots <- .series_diagnostic_plots(out, language)
    out$plots$reference <- .series_fit_plot(fit, language)
  }
  out
}


# Internal diagnostic graphics.
.series_lollipop <- function(d, value, ylab, colour, language) {
  d$value <- d[[value]]
  d <- d[d$lag > 0 | value == "ccf", , drop = FALSE]
  d$exceeds <- abs(d$value) > d$bound
  ggplot2::ggplot(d, ggplot2::aes(lag, value)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = -bound, ymax = bound), fill = "grey85", alpha = .7) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey40", linewidth = .4) +
    ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), linewidth = .9, colour = colour) +
    ggplot2::geom_point(ggplot2::aes(fill = exceeds), shape = 21, size = 2.8, colour = colour, stroke = .9) +
    ggplot2::scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = colour), guide = "none") +
    ggplot2::scale_x_continuous(breaks = unique(d$lag)) +
    ggplot2::scale_y_continuous(limits = c(-1, 1)) +
    .series_theme() + ggplot2::labs(x = .tr(language, "Defasagem (anos)", "Lag (years)"), y = ylab)
}

.series_diagnostic_plots <- function(out, language = "pt") {
  colour <- .series_identity(out, language = language)$colour %||% "#346675"
  r <- out$residuals
  qq <- data.frame(theoretical = stats::qnorm(stats::ppoints(nrow(r))), sample = sort(r$residual))
  band <- .tr(language, "Faixa cinza: \u00b11,96/\u221an (ru\u00eddo branco).", "Grey band: \u00b11.96/\u221an (white noise).")
  list(
    residuals = ggplot2::ggplot(r, ggplot2::aes(year, residual)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
      ggplot2::geom_segment(ggplot2::aes(xend = year, yend = 0), colour = colour, alpha = .5) +
      ggplot2::geom_point(shape = 21, fill = colour, colour = "grey15", size = 2) +
      .series_theme() + ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .tr(language, "Res\u00edduo", "Residual")),
    acf = .series_lollipop(out$acf, "acf", .tr(language, "Autocorrela\u00e7\u00e3o residual", "Residual autocorrelation"), colour, language) +
      ggplot2::labs(caption = band),
    pacf = .series_lollipop(out$pacf, "pacf", .tr(language, "Autocorrela\u00e7\u00e3o parcial residual", "Residual partial autocorrelation"), colour, language) +
      ggplot2::labs(caption = band),
    qq = ggplot2::ggplot(qq, ggplot2::aes(theoretical, sample)) +
      ggplot2::geom_abline(intercept = mean(r$residual), slope = stats::sd(r$residual), colour = "grey40", linetype = "22") +
      ggplot2::geom_point(shape = 21, fill = colour, colour = "grey15", size = 2) + .series_theme() +
      ggplot2::labs(x = .tr(language, "Quantis normais", "Normal quantiles"), y = .tr(language, "Res\u00edduos ordenados", "Ordered residuals")),
    observed_fitted = ggplot2::ggplot(r, ggplot2::aes(fitted, observed)) +
      ggplot2::geom_abline(colour = "grey40", linetype = "22") +
      ggplot2::geom_point(shape = 21, fill = colour, colour = "grey15", size = 2) + .series_theme() +
      ggplot2::labs(x = .tr(language, "Ajustado", "Fitted"), y = .tr(language, "Observado", "Observed")))
}
