#' Diversity and entropy of land-cover classes from get_area()
#'
#' Treats each land-cover class as a "species" and its area as abundance, and
#' computes, for every year, classic diversity, entropy, evenness and dominance
#' indices of the landscape composition. Input comes directly from
#' [get_area()], inheriting its class dictionary, colours, units and metadata.
#'
#' With p_i = area of class i / total area of the year and S classes present:
#' \itemize{
#'   \item `richness`: S (Hill number of order 0).
#'   \item `shannon`: Shannon entropy H' = -sum(p_i log p_i), in the unit of `base`.
#'   \item `shannon_max`: maximum entropy log(S).
#'   \item `pielou`: Pielou evenness J' = H' / log(S).
#'   \item `relative_entropy`: H' / log(S_total), with S_total the number of
#'     classes in the whole series, so years stay comparable when S changes.
#'   \item `hill_1`: exp(H') (effective number of equally common classes, q = 1).
#'   \item `simpson`: Simpson dominance D = sum(p_i^2).
#'   \item `simpson_diversity`: Gini-Simpson 1 - D.
#'   \item `inv_simpson`: 1 / D (Hill number of order 2, `hill_2`).
#'   \item `simpson_evenness`: (1 / D) / S.
#'   \item `berger_parker`: dominance max(p_i), with `dominant_class`.
#'   \item `dominance`: log(S) - H' (departure from maximum evenness).
#' }
#' Hill numbers are always in effective classes, whatever `base` is.
#'
#' @param data Object returned by [get_area()], its `time_series` ggplot or its
#'   `aggregate_data` table.
#' @param classes Optional class codes to retain; others are dropped before
#'   proportions are computed.
#' @param min_area Minimum area (same unit as `get_area()`, km2) for a class to
#'   count as present in a year.
#' @param base Logarithm base for entropies: `exp(1)` (nats, default), `2`
#'   (bits) or `10`.
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_class_diversity` list with:
#' \itemize{
#'   \item `indices`: one row per year with the indices above, `total_area`
#'     and `dominant_class`. Years without valid pixels stay `NA`.
#'   \item `contributions`: one row per class and year with `area`,
#'     `proportion` and `shannon_contribution` (-p log p), which sum to
#'     `shannon` in each year.
#'   \item `change`: net change of each index between the first and last
#'     observed years.
#'   \item `class_dictionary`, `metadata` and `plots` (`indices`, `hill`,
#'     `contribution`, `dominance`).
#' }
#' No files are written; tables and ggplots are ready for export.
#' @export
class_diversity <- function(data, classes = NULL, min_area = 0, base = exp(1),
                            language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  if (!is.numeric(min_area) || length(min_area) != 1L || !is.finite(min_area) || min_area < 0)
    stop("min_area deve ser um n\u00famero n\u00e3o negativo.")
  if (!is.numeric(base) || length(base) != 1L || !is.finite(base) || base <= 1)
    stop("base deve ser um n\u00famero maior que 1.")
  input <- .series_input(data, classes)
  d <- input$data
  d$area[is.finite(d$area) & d$area <= min_area] <- 0
  s_total <- length(unique(d$code[is.finite(d$area) & d$area > 0]))
  lg <- function(x) log(x) / log(base)

  contributions <- do.call(rbind, lapply(split(d, d$year), function(z) {
    total <- sum(z$area)
    p <- if (is.finite(total) && total > 0) z$area / total else rep(NA_real_, nrow(z))
    data.frame(year = z$year, code = z$code, area = z$area, proportion = p,
               shannon_contribution = ifelse(is.finite(p) & p > 0, -p * lg(p), ifelse(is.finite(p), 0, NA_real_)))
  }))
  rownames(contributions) <- NULL

  indices <- do.call(rbind, lapply(split(contributions, contributions$year), function(z) {
    p <- z$proportion[is.finite(z$proportion) & z$proportion > 0]
    yr <- z$year[1]
    if (!length(p)) return(data.frame(year = yr, total_area = NA_real_, richness = NA_integer_,
      shannon = NA_real_, shannon_max = NA_real_, pielou = NA_real_, relative_entropy = NA_real_,
      hill_1 = NA_real_, simpson = NA_real_, simpson_diversity = NA_real_, inv_simpson = NA_real_,
      simpson_evenness = NA_real_, berger_parker = NA_real_, dominant_class = NA_real_, dominance = NA_real_))
    S <- length(p); H <- -sum(p * lg(p)); D <- sum(p^2)
    data.frame(year = yr, total_area = sum(z$area), richness = S, shannon = H, shannon_max = lg(S),
      pielou = if (S > 1) H / lg(S) else NA_real_,
      relative_entropy = if (s_total > 1) H / lg(s_total) else NA_real_,
      hill_1 = exp(-sum(p * log(p))), simpson = D, simpson_diversity = 1 - D, inv_simpson = 1 / D,
      simpson_evenness = (1 / D) / S, berger_parker = max(p),
      dominant_class = z$code[which.max(z$proportion)], dominance = lg(S) - H)
  }))
  rownames(indices) <- NULL

  ok <- indices[is.finite(indices$shannon), , drop = FALSE]
  metrics <- c("richness", "shannon", "pielou", "hill_1", "simpson_diversity", "inv_simpson", "berger_parker")
  change <- if (nrow(ok) >= 2) data.frame(index = metrics, start = ok$year[1], end = ok$year[nrow(ok)],
      first = unlist(ok[1, metrics]), last = unlist(ok[nrow(ok), metrics]), row.names = NULL) else
    data.frame(index = character(), start = integer(), end = integer(), first = numeric(), last = numeric())
  change$net_change <- change$last - change$first

  out <- list(indices = indices, contributions = contributions, change = change,
    class_dictionary = input$dictionary,
    metadata = c(input$metadata, list(min_area = min_area, log_base = base, classes_total = s_total,
      method = "class areas as abundances; proportions over classes present in each year")),
    plots = NULL)
  class(out) <- "biomastats_class_diversity"
  if (isTRUE(build_plot)) out$plots <- .diversity_plots(out, language)
  out
}

.diversity_plots <- function(out, language = "pt") {
  year_lab <- .tr(language, "Ano", "Year")
  unit <- switch(as.character(round(out$metadata$log_base, 6)), "2" = "bits", "10" = "dits", "nats")
  idx <- out$indices
  dic <- out$class_dictionary
  keys <- as.character(dic$code)
  class_lab <- stats::setNames(paste(dic$code, if (language == "pt") dic$classe else dic$class), keys)

  labels <- c(shannon = .tr(language, paste0("Entropia de Shannon H' (", unit, ")"), paste0("Shannon entropy H' (", unit, ")")),
              pielou = .tr(language, "Equitabilidade de Pielou J'", "Pielou evenness J'"),
              simpson_diversity = .tr(language, "Diversidade de Gini-Simpson 1 \u2212 D", "Gini-Simpson diversity 1 \u2212 D"),
              berger_parker = .tr(language, "Domin\u00e2ncia de Berger-Parker", "Berger-Parker dominance"))
  long <- do.call(rbind, lapply(names(labels), function(m)
    data.frame(year = idx$year, index = factor(labels[[m]], levels = labels), value = idx[[m]])))
  indices <- ggplot2::ggplot(long, ggplot2::aes(year, value)) +
    ggplot2::geom_line(colour = "#1F5F8B", linewidth = .9, na.rm = TRUE) +
    ggplot2::geom_point(shape = 21, fill = "white", colour = "#1F5F8B", size = 1.8, stroke = .8, na.rm = TRUE) +
    ggplot2::facet_wrap(~index, ncol = 2, scales = "free_y") +
    .series_theme() + ggplot2::labs(x = year_lab, y = NULL,
      caption = .tr(language, "Classes como esp\u00e9cies e \u00e1reas como abund\u00e2ncias.",
                    "Classes as species and areas as abundances."))

  hill_lab <- c(richness = .tr(language, "q = 0 \u00b7 riqueza", "q = 0 \u00b7 richness"),
                hill_1 = .tr(language, "q = 1 \u00b7 exp(H')", "q = 1 \u00b7 exp(H')"),
                inv_simpson = .tr(language, "q = 2 \u00b7 1/D", "q = 2 \u00b7 1/D"))
  hl <- do.call(rbind, lapply(names(hill_lab), function(m)
    data.frame(year = idx$year, order = factor(hill_lab[[m]], levels = hill_lab), value = idx[[m]])))
  hill <- ggplot2::ggplot(hl, ggplot2::aes(year, value, colour = order)) +
    ggplot2::geom_line(linewidth = 1, na.rm = TRUE) +
    ggplot2::geom_point(size = 1.6, na.rm = TRUE) +
    ggplot2::scale_colour_manual(values = c("#1F5F8B", "#C46A1B", "#5B8C3A"), name = .tr(language, "Ordem de Hill", "Hill order")) +
    ggplot2::expand_limits(y = 0) +
    .series_theme() + ggplot2::labs(x = year_lab, y = .tr(language, "N\u00famero efetivo de classes", "Effective number of classes"),
      caption = .tr(language, "Quanto mais as curvas se afastam da riqueza, maior a domin\u00e2ncia de poucas classes.",
                    "The further the curves fall below richness, the stronger the dominance of few classes."))

  ct <- out$contributions
  ct$code <- factor(as.character(ct$code), levels = keys)
  fill_scale <- ggplot2::scale_fill_manual(values = stats::setNames(dic$color, keys), labels = class_lab,
                                           name = .tr(language, "Classe", "Class"),
                                           guide = ggplot2::guide_legend(ncol = 3, title.position = "top"))
  contribution <- ggplot2::ggplot(ct[is.finite(ct$shannon_contribution), ], ggplot2::aes(year, shannon_contribution, fill = code)) +
    ggplot2::geom_area(colour = "white", linewidth = .2) +
    ggplot2::geom_line(data = idx, ggplot2::aes(year, shannon_max), inherit.aes = FALSE,
                       linetype = "22", colour = "grey25", na.rm = TRUE) +
    fill_scale + .series_theme() +
    ggplot2::labs(x = year_lab, y = .tr(language, paste0("Contribui\u00e7\u00e3o \u2212p log p (", unit, ")"),
                                         paste0("Contribution \u2212p log p (", unit, ")")),
      caption = .tr(language, "A altura empilhada \u00e9 a entropia de Shannon; tracejado: m\u00e1ximo log(S).",
                    "Stacked height is Shannon entropy; dashed: maximum log(S)."))

  dominance <- ggplot2::ggplot(ct[is.finite(ct$proportion), ], ggplot2::aes(year, proportion, fill = code)) +
    ggplot2::geom_area(colour = "white", linewidth = .2) +
    ggplot2::geom_line(data = idx, ggplot2::aes(year, berger_parker), inherit.aes = FALSE,
                       colour = "grey15", linewidth = .8, na.rm = TRUE) +
    ggplot2::scale_y_continuous(labels = function(x) paste0(100 * x, "%")) +
    fill_scale + .series_theme() +
    ggplot2::labs(x = year_lab, y = .tr(language, "Propor\u00e7\u00e3o da \u00e1rea", "Share of area"),
      caption = .tr(language, "Linha: domin\u00e2ncia de Berger-Parker (propor\u00e7\u00e3o da classe mais abundante).",
                    "Line: Berger-Parker dominance (share of the most abundant class)."))

  list(indices = indices, hill = hill, contribution = contribution, dominance = dominance)
}
