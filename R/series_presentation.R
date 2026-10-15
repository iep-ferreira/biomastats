# Internal, shared presentation of native time-series objects. Non-ASCII text is
# written with \u escapes so labels keep their accents under any locale.

.tr <- function(language, pt, en) if (identical(language, "en")) en else pt

.series_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "grey90", linewidth = .35),
      axis.line = ggplot2::element_line(colour = "grey35", linewidth = .35),
      axis.ticks = ggplot2::element_line(colour = "grey35", linewidth = .35),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "bottom", legend.key.width = ggplot2::unit(1.6, "lines"),
      plot.title = ggplot2::element_text(face = "bold"), plot.title.position = "plot",
      plot.subtitle = ggplot2::element_text(colour = "grey30"),
      plot.caption = ggplot2::element_text(hjust = 0, colour = "grey40", size = 9),
      plot.caption.position = "plot")
}

.series_area_label <- function(metadata, language) {
  unit <- metadata$area_unit %||% "km2"
  unit <- if (unit == "km2") "km²" else unit
  paste0(.tr(language, "Área", "Area"), " (", unit, ")")
}

.series_identity <- function(out, code = out$series, language = "pt") {
  d <- out$class_dictionary
  i <- match(as.numeric(code), d$code)
  list(label = paste(code, if (language == "pt") d$classe[i] else d$class[i]), colour = d$color[i])
}

.series_model_label <- function(model, language) {
  switch(model,
    linear = .tr(language, "Tendência linear", "Linear trend"),
    segmented = .tr(language, "Segmentado contínuo", "Continuous segmented"),
    bent_cable = .tr(language, "Bent-cable quadrático", "Quadratic bent-cable"),
    prob_bent_uniform = .tr(language, "Bent-cable probabilístico uniforme (U-BC)", "Probabilistic bent-cable, uniform (U-BC)"),
    prob_bent_logistic = .tr(language, "Bent-cable probabilístico logístico (L-BC)", "Probabilistic bent-cable, logistic (L-BC)"),
    model)
}

.series_plot_data <- function(out, language) {
  d <- out$class_dictionary
  labs <- if (language == "pt") paste(d$code, d$classe) else paste(d$code, d$class)
  ggplot2::ggplot(out$data, ggplot2::aes(year, area, colour = factor(code))) +
    ggplot2::geom_line(linewidth = .9, na.rm = TRUE) +
    ggplot2::geom_point(shape = 21, fill = "white", size = 1.8, stroke = .8, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = stats::setNames(d$color, d$code), labels = stats::setNames(labs, d$code),
                                 name = .tr(language, "Classe", "Class")) +
    .series_theme() +
    ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .series_area_label(out$metadata, language))
}

# Dense interior grid: the fitted mean function evaluated between observations.
.series_curve <- function(fit, end = max(fit$x) + fit$origin) {
  start <- min(fit$x) + fit$origin
  centres <- fit$origin + fit$taus
  extra <- c(centres, if (.regime_has_scale(fit$model)) c(centres - fit$scales, centres + fit$scales))
  year <- sort(unique(c(seq(start, end, length.out = 801), fit$x + fit$origin, extra)))
  year <- year[year >= start & year <= end]
  data.frame(year = year, fitted = as.numeric(.regime_design(year, fit) %*% fit$coef))
}

.series_enrich_fit <- function(out) {
  f <- out$fit
  out$curve <- .series_curve(f)
  out$kernels <- data.frame(transition = integer(), year = numeric(), density = numeric(), cdf = numeric())
  cp <- out$change_points
  out$transition_bands <- if (nrow(cp) && .regime_has_scale(f$model))
    data.frame(transition = cp$transition, lower = cp$band_low, upper = cp$band_high) else
    data.frame(transition = integer(), lower = numeric(), upper = numeric())
  if (f$model %in% c("prob_bent_uniform", "prob_bent_logistic")) {
    logistic <- f$model == "prob_bent_logistic"
    span <- range(out$fitted$year)
    out$kernels <- do.call(rbind, lapply(seq_len(f$breaks), function(j) {
      tau <- f$origin + f$taus[j]; s <- f$scales[j]
      yr <- sort(unique(c(seq(span[1], span[2], length.out = 801), tau + c(-1, 1) * s)))
      data.frame(transition = j, year = yr,
        density = if (logistic) stats::dlogis(yr, tau, s) else stats::dunif(yr, tau - s, tau + s),
        cdf = if (logistic) stats::plogis(yr, tau, s) else stats::punif(yr, tau - s, tau + s))
    }))
  }
  out
}

.series_fit_plot <- function(out, language = "pt", xlim = range(out$fitted$year)) {
  id <- .series_identity(out, language = language)
  keys <- c(fit = paste(.tr(language, "Ajuste", "Fit"), "—", .series_model_label(out$model, language)),
            kernel = .tr(language, "Kernel recuperado do limiar", "Recovered threshold kernel"))
  cp <- out$change_points
  p <- ggplot2::ggplot()
  if (nrow(out$transition_bands)) p <- p +
    ggplot2::geom_rect(data = out$transition_bands,
      ggplot2::aes(xmin = lower, xmax = upper, ymin = -Inf, ymax = Inf, fill = "band"), alpha = .16) +
    ggplot2::scale_fill_manual(values = c(band = id$colour), name = NULL,
      labels = .tr(language, "Faixa de transição (quantis 10%–90% do limiar)",
                   "Transition band (threshold quantiles 10%–90%)"))
  if (nrow(cp)) p <- p + ggplot2::geom_vline(xintercept = cp$estimate, linetype = "22", colour = "grey25", linewidth = .45)
  sec <- ggplot2::waiver()
  if (nrow(out$kernels)) {
    # Affine map of densities onto the area axis; the secondary axis inverts it.
    rg <- range(c(out$fitted$observed, out$curve$fitted)); top <- max(out$kernels$density)
    a <- diff(rg) / top; b <- rg[1]
    k <- out$kernels; k$display <- b + a * k$density
    p <- p + ggplot2::geom_line(data = k, ggplot2::aes(year, display, group = transition, colour = "kernel", linetype = "kernel"),
                                linewidth = .75)
    sec <- ggplot2::sec_axis(transform = ~ (. - b) / a,
      name = .tr(language, "Densidade do limiar (1/ano)", "Threshold density (1/year)"))
  }
  p <- p +
    ggplot2::geom_point(data = out$fitted, ggplot2::aes(year, observed, shape = "obs"),
                        fill = "white", colour = "grey15", size = 2, stroke = .7) +
    ggplot2::geom_line(data = out$curve, ggplot2::aes(year, fitted, colour = "fit", linetype = "fit"), linewidth = 1.15) +
    ggplot2::scale_colour_manual(values = c(fit = .series_ink(id$colour), kernel = "grey40"), labels = keys, name = NULL) +
    ggplot2::scale_linetype_manual(values = c(fit = "solid", kernel = "42"), labels = keys, name = NULL) +
    ggplot2::scale_shape_manual(values = c(obs = 21), labels = .tr(language, "Observado", "Observed"), name = NULL) +
    ggplot2::scale_y_continuous(sec.axis = sec) +
    ggplot2::coord_cartesian(xlim = xlim) +
    ggplot2::guides(shape = ggplot2::guide_legend(order = 1), colour = ggplot2::guide_legend(order = 2),
                    linetype = ggplot2::guide_legend(order = 2), fill = ggplot2::guide_legend(order = 3)) +
    .series_theme() + ggplot2::theme(legend.box = "vertical", legend.spacing.y = ggplot2::unit(0, "pt")) +
    ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .series_area_label(out$metadata, language),
      subtitle = paste0(id$label, " · ", out$breaks, .tr(language, " transição(ões)", " transition(s)"),
                        if (nrow(cp)) paste0(" · τ = ", paste(sprintf("%.1f", cp$estimate), collapse = "; "))),
      caption = if (nrow(cp)) .tr(language,
        "Linhas verticais: centros estimados. A faixa descreve o limiar aleatório, não é intervalo de confiança.",
        "Vertical lines: estimated centres. The band describes the random threshold; it is not a confidence interval."))
  p
}

.series_interval_plot <- function(out, language = "pt") {
  d <- out$change_points
  if (!nrow(d)) return(NULL)
  d$label <- factor(paste0("τ", d$transition), levels = rev(paste0("τ", d$transition)))
  b <- out$bootstrap
  p <- ggplot2::ggplot(d, ggplot2::aes(y = label))
  if (nrow(b)) {
    b$label <- factor(paste0("τ", b$transition), levels = levels(d$label))
    p <- p + ggplot2::geom_jitter(data = b, ggplot2::aes(year, label), height = .12, width = 0,
                                  size = .9, alpha = .18, colour = "grey45")
  }
  p + ggplot2::geom_errorbar(ggplot2::aes(xmin = conf_low, xmax = conf_high), orientation = "y",
                             width = .25, linewidth = .9, colour = "#9C3026", na.rm = TRUE) +
    ggplot2::geom_point(ggplot2::aes(x = estimate), shape = 21, size = 3.2, stroke = .9,
                        fill = .series_identity(out)$colour, colour = "grey10") +
    .series_theme() +
    ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .tr(language, "Transição", "Transition"),
      subtitle = paste0(.series_model_label(out$model, language), " — ",
                        .tr(language, "IC bootstrap ", "Bootstrap CI "), 100 * out$level, "% (",
                        out$bootstrap_type, ")"),
      caption = .tr(language,
        "Ponto: estimativa pontual. Barra: IC percentil. Pontos cinza: réplicas bootstrap.",
        "Point: estimate. Bar: percentile CI. Grey dots: bootstrap replicates."))
}

# Darkens light dictionary colours for thin lines and outlines; fills keep the
# original dictionary colour.
.series_ink <- function(colour) {
  m <- grDevices::col2rgb(colour) / 255
  light <- colSums(m * c(.299, .587, .114)) > .62
  m[, light] <- m[, light] * .6
  ifelse(light, grDevices::rgb(m[1, ], m[2, ], m[3, ]), colour)
}
