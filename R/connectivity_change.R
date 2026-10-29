#' Mudanca temporal da conectividade: ECA, dECA, fracoes e ligacoes
#'
#' Consolida a conectividade de varios anos em tabelas e graficos de serie
#' temporal, em analogia ao `MK_dECA()` do Makurhini: area de habitat, area
#' conectada equivalente (ECA), variacoes dA e dECA, razao rECA, tipo de mudanca,
#' fracoes intra, direct e step da paisagem e participacao das ligacoes. Pode
#' devolver uma animacao (gganimate) dos mapas de importancia ao longo dos anos.
#' @param x Lista nomeada por ano de objetos de [land_connectivity()], ou
#'   poligonos de [reclass_to_polygons()] com varios anos (lista temporal ou sf
#'   com coluna year), analisados aqui com os argumentos de `...`.
#' @param years Anos a usar; NULL usa todos.
#' @param ... Argumentos de [land_connectivity()] quando x sao poligonos (por
#'   exemplo max_dist, metric_patch, boundary, distance, link_importance).
#'   Use o mesmo limite em todos os anos para que A_L seja constante.
#' @param reference "previous" (padrao): cada ano comparado ao anterior, como no
#'   Makurhini; "first": todos comparados ao primeiro ano.
#' @param animate Construir a animacao gganimate do mapa de importancia (FALSE
#'   por padrao; exige o pacote gganimate).
#' @param language Idioma dos rotulos: pt (padrao) ou en.
#' @param build_plot Construir os ggplots (TRUE por padrao).
#' @return Lista biomastats_connectivity_change com series (data.frame por ano,
#'   limiar e indice: habitat_ha, ECA_ha, ECA_LA e ECAnorm em %, dA, dECA, rECA,
#'   change, change_makurhini, intra, direct, step), metric_series (todas as
#'   colunas de metrics por ano), patch_series (sf de patch_metrics por ano),
#'   link_series e link_summary (participacao das ligacoes, ou NULL),
#'   connectivity (objetos de land_connectivity por ano), metadata, eca_plot,
#'   change_plot, fractions_plot, map_plot, link_plot e animation (objeto
#'   gganim, ou NULL).
#' @details ECA = A_L sqrt(indice) (Saura et al. 2011), em ha. dA e dECA sao
#'   variacoes percentuais em relacao a referencia; rECA = dECA / dA. O tipo de
#'   mudanca segue o Makurhini: perda de conectividade maior que a de habitat
#'   (dECA < dA < 0), perda de habitat maior (dA < dECA < 0), perdas iguais,
#'   manutencao (ambas zero) e ganho. Diferentemente do Makurhini, que compara o
#'   primeiro ano com A_L, o primeiro ano e a linha de base (NA).
#'
#'   Os identificadores de fragmentos sao refeitos a cada vetorizacao, de modo que
#'   ligacoes nao sao seguidas entre anos pelo id; link_summary resume, por ano, o
#'   numero de ligacoes, a soma e o maximo das importancias e a parcela das 10
#'   maiores, e link_plot mostra a curva de importancia por posicao. map_plot
#'   mapeia a importancia dos fragmentos (e das ligacoes, quando calculadas) no
#'   primeiro limiar, com um painel por ano; animation mostra o mesmo mapa ano a
#'   ano e e renderizada com `gganimate::animate()` ou `gganimate::anim_save()`.
#' @references Saura, S. et al. (2011) Ecological Indicators 11: 407-416.
#'   Godinez-Gomez, O. & Correa Ayram, C. A. (2020) Makurhini,
#'   <https://github.com/connectscape/Makurhini>.
#' @md
#' @examples
#' \dontrun{
#' poligonos <- reclass_to_polygons(metricas, years = c(1985, 2000, 2024), save_shp = FALSE)
#' mudanca <- connectivity_change(poligonos, max_dist = 500, metric_patch = c("IIC", "PC"),
#'   link_importance = TRUE, animate = TRUE)
#' mudanca$series
#' mudanca$change_plot
#' gganimate::animate(mudanca$animation, fps = 2)
#' as_makurhini(mudanca, "dECA")
#' }
#' @export
connectivity_change <- function(x, years = NULL, ..., reference = c("previous", "first"),
                                animate = FALSE, language = c("pt", "en"), build_plot = TRUE) {
  reference <- match.arg(reference)
  language <- match.arg(language)
  .connectivity_check_flag(animate, "animate")
  .connectivity_check_flag(build_plot, "build_plot")
  if (animate && !requireNamespace("gganimate", quietly = TRUE)) {
    stop("Instale gganimate para animate = TRUE.")
  }
  con <- .change_inputs(x, years, language, ...)
  if (length(con) < 2L) stop("Sao necessarios ao menos dois anos.")
  la <- vapply(con, function(z) z$metadata$landscape_area_m2, numeric(1))
  if (any(abs(la - la[1]) > 1e-6 * la[1])) {
    warning("A_L difere entre os anos; use o mesmo limite para comparar ECA.")
  }
  year_values <- as.numeric(names(con))
  metric_series <- do.call(rbind, lapply(seq_along(con), function(i) {
    data.frame(year = year_values[i], con[[i]]$metrics,
               habitat_area_m2 = con[[i]]$metadata$habitat_area_m2, landscape_area_m2 = la[[i]])
  }))
  series <- .change_series(metric_series, reference, language)
  patch_series <- do.call(rbind, lapply(seq_along(con), function(i) {
    p <- con[[i]]$patch_metrics
    p$year <- year_values[i]
    p[, c("year", setdiff(names(p), "year"))]
  }))
  links <- lapply(seq_along(con), function(i) {
    l <- con[[i]]$link_metrics
    if (is.null(l) || !nrow(l)) return(NULL)
    data.frame(year = year_values[i], l)
  })
  link_series <- do.call(rbind, links)
  link_summary <- if (!is.null(link_series)) .change_link_summary(link_series)
  rownames(metric_series) <- rownames(patch_series) <- NULL
  if (!is.null(link_series)) rownames(link_series) <- NULL
  plots <- list(eca_plot = NULL, change_plot = NULL, fractions_plot = NULL, map_plot = NULL,
                link_plot = NULL, animation = NULL)
  map <- .change_map(patch_series, link_series, language)
  if (build_plot || animate) {
    if (build_plot) {
      plots$eca_plot <- .change_eca_plot(series, language)
      plots$change_plot <- .change_delta_plot(series, language)
      plots$fractions_plot <- .change_fractions_plot(series, language)
      plots$map_plot <- map$base + ggplot2::facet_wrap(~year)
      plots$link_plot <- if (!is.null(link_series)) .change_link_plot(link_series, language)
    }
    if (animate) {
      plots$animation <- map$base + gganimate::transition_manual(year) +
        ggplot2::labs(title = "{current_frame}")
    }
  }
  metadata <- list(schema_version = "1.0",
    package_version = as.character(utils::packageVersion("biomastats")),
    years = year_values, reference = reference, landscape_area_m2 = la,
    max_dist_m = sort(unique(metric_series$max_dist_m)),
    metrics = unique(series$metric), area_unit = "ha",
    eca_rule = "ECA = A_L sqrt(index) (Saura et al. 2011)",
    change_rule = paste("dA, dECA = 100 (x - x_ref) / x_ref; rECA = dECA / dA;",
      "change classes of Makurhini MK_dECA"),
    map = list(metric = map$metric, max_dist_m = map$max_dist))
  structure(c(list(series = series, metric_series = metric_series, patch_series = patch_series,
    link_series = link_series, link_summary = link_summary, connectivity = con,
    metadata = metadata), plots), class = c("biomastats_connectivity_change", "list"))
}

# One land_connectivity per year, named by year and in chronological order.
.change_inputs <- function(x, years, language, ...) {
  if (is.list(x) && !inherits(x, "sf") && length(x) &&
      all(vapply(x, inherits, logical(1), what = "biomastats_connectivity"))) {
    if (is.null(names(x)) || anyNA(suppressWarnings(as.numeric(names(x))))) {
      stop("A lista de land_connectivity deve ser nomeada pelos anos.")
    }
    if (length(list(...))) stop("Argumentos extras so se aplicam a poligonos.")
    out <- x
  } else {
    available <- if (inherits(x, "sf")) {
      if (!"year" %in% names(x)) stop("Poligonos sf precisam da coluna year.")
      sort(unique(stats::na.omit(x$year)))
    } else {
      if (!is.list(x) || is.null(names(x))) stop("x deve ser lista nomeada por ano ou sf com year.")
      as.numeric(names(x))
    }
    if (!is.null(years)) {
      if (!all(years %in% available)) stop("years indisponiveis em x.")
      available <- years
    }
    dots <- list(...)
    dots$build_plot <- FALSE
    dots$language <- language
    out <- lapply(available, function(y) do.call(land_connectivity, c(list(x, year = y), dots)))
    names(out) <- as.character(available)
  }
  if (!is.null(years)) {
    keep <- as.character(years)
    if (!all(keep %in% names(out))) stop("years indisponiveis em x.")
    out <- out[keep]
  }
  out[order(as.numeric(names(out)))]
}

.change_series <- function(m, reference, language) {
  metrics <- c("IIC", "PC")[c(TRUE, any(is.finite(m$PC)))]
  rows <- lapply(metrics, function(metric) {
    eca <- m[[paste0("ECA_", metric)]]
    data.frame(year = m$year, max_dist_m = m$max_dist_m, metric = metric,
      habitat_ha = m$habitat_area_m2 / 10000, ECA_ha = eca / 10000,
      ECA_LA = 100 * eca / m$landscape_area_m2, ECAnorm = m[[paste0("ECAnorm_", metric)]],
      intra = m[[paste0(metric, "_intra")]], direct = m[[paste0(metric, "_direct")]],
      step = m[[paste0(metric, "_step")]])
  })
  s <- do.call(rbind, rows)
  s <- s[order(s$metric, s$max_dist_m, s$year), , drop = FALSE]
  key <- paste(s$metric, s$max_dist_m)
  delta <- function(v) unlist(lapply(split(v, factor(key, unique(key))), function(z) {
    ref <- if (reference == "first") rep(z[1], length(z)) else c(NA, z[-length(z)])
    out <- 100 * (z - ref) / ref
    out[1] <- NA_real_
    out
  }), use.names = FALSE)
  s$dA <- delta(s$habitat_ha)
  s$dECA <- delta(s$ECA_ha)
  s$rECA <- s$dECA / s$dA
  classes <- .change_class(s$dA, s$dECA)
  s$change <- .tr(language, classes$pt, classes$en)
  s$change_makurhini <- classes$makurhini
  rownames(s) <- NULL
  s[, c("year", "max_dist_m", "metric", "habitat_ha", "ECA_ha", "ECA_LA", "ECAnorm",
        "dA", "dECA", "rECA", "change", "change_makurhini", "intra", "direct", "step")]
}

# Change classes of Makurhini (dECAfun2), with a tolerance for equality.
.change_class <- function(dA, dECA, tol = 1e-9) {
  same <- abs(dECA - dA) <= tol * pmax(1, abs(dA))
  zero <- abs(dA) <= tol & abs(dECA) <= tol
  type <- ifelse(is.na(dA) | is.na(dECA), "baseline",
    ifelse(zero, "kept", ifelse(same & dA < 0, "equal",
    ifelse(dECA < dA & dA < 0, "connectivity", ifelse(dA < dECA & dECA < 0, "habitat", "gain")))))
  pt <- c(baseline = "Linha de base", kept = "Sem mudan\u00e7a", equal = "Perdas iguais",
    connectivity = "Perda de conectividade > de habitat",
    habitat = "Perda de habitat > de conectividade", gain = "Ganho de habitat ou conectividade")
  en <- c(baseline = "Baseline", kept = "No change", equal = "Equal loss",
    connectivity = "Connectivity loss > habitat loss",
    habitat = "Habitat loss > connectivity loss", gain = "Habitat or connectivity gain")
  mk <- c(baseline = NA, kept = "Habitat or connectivity maintained", equal = "Equal loss",
    connectivity = "+ Connectivity loss", habitat = "+ Habitat loss",
    gain = "Habitat or connectivity gain")
  list(pt = unname(pt[type]), en = unname(en[type]), makurhini = unname(mk[type]))
}

.change_link_summary <- function(l) {
  groups <- split(l, list(l$year, l$max_dist_m, l$metric), drop = TRUE)
  out <- do.call(rbind, lapply(groups, function(g) {
    imp <- sort(g$importance, decreasing = TRUE)
    data.frame(year = g$year[1], max_dist_m = g$max_dist_m[1], metric = g$metric[1],
      links = nrow(g), positive_links = sum(imp > 0), importance_sum = sum(imp),
      importance_max = max(imp), top10_share = if (sum(imp) > 0)
        100 * sum(utils::head(imp, 10)) / sum(imp) else NA_real_)
  }))
  out <- out[order(out$metric, out$max_dist_m, out$year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.change_eca_plot <- function(s, language) {
  long <- rbind(
    data.frame(s[, c("year", "max_dist_m", "metric")], quantity = .tr(language,
      "\u00c1rea de habitat", "Habitat area"), value = s$habitat_ha),
    data.frame(s[, c("year", "max_dist_m", "metric")], quantity = "ECA", value = s$ECA_ha))
  long$panel <- paste0(long$metric, " | ", long$max_dist_m, " m")
  ggplot2::ggplot(long, ggplot2::aes(x = year, y = value, colour = quantity)) +
    ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
    ggplot2::scale_colour_manual(values = c("#5b8c2d", "#245b78"), name = NULL) +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::labs(x = .tr(language, "Ano", "Year"), y = .tr(language, "\u00c1rea (ha)", "Area (ha)")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

.change_delta_plot <- function(s, language) {
  s <- s[!is.na(s$dA), , drop = FALSE]
  long <- rbind(data.frame(s[, c("year", "max_dist_m", "metric", "change")], quantity = "dA",
                           value = s$dA),
                data.frame(s[, c("year", "max_dist_m", "metric", "change")], quantity = "dECA",
                           value = s$dECA))
  long$panel <- paste0(long$metric, " | ", long$max_dist_m, " m")
  long$period <- factor(long$year)
  ggplot2::ggplot(long, ggplot2::aes(x = period, y = value, fill = quantity)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.75) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
    ggplot2::scale_fill_manual(values = c(dA = "#5b8c2d", dECA = "#245b78"), name = NULL) +
    ggplot2::facet_wrap(~panel, scales = "free_y") +
    ggplot2::labs(x = .tr(language, "Ano (vs. refer\u00eancia)", "Year (vs. reference)"),
                  y = .tr(language, "Varia\u00e7\u00e3o (%)", "Change (%)")) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

.change_fractions_plot <- function(s, language) {
  parts <- c("intra", "direct", "step")
  long <- do.call(rbind, lapply(parts, function(part) data.frame(
    s[, c("year", "max_dist_m", "metric")], fraction = part, value = s[[part]])))
  long$fraction <- factor(long$fraction, levels = rev(parts))
  long$panel <- paste0(long$metric, " | ", long$max_dist_m, " m")
  long$period <- factor(long$year)
  ggplot2::ggplot(long, ggplot2::aes(x = period, y = value, fill = fraction)) +
    ggplot2::geom_col(width = 0.75) +
    ggplot2::scale_fill_manual(values = c(intra = "#2e74b5", direct = "#5b8c2d", step = "#d08c2d"),
      labels = c(intra = "intra", direct = .tr(language, "direta", "direct"),
                 step = .tr(language, "trampolins", "stepping stones")),
      name = .tr(language, "Fra\u00e7\u00e3o", "Fraction")) +
    ggplot2::facet_wrap(~panel) +
    ggplot2::labs(x = .tr(language, "Ano", "Year"),
                  y = .tr(language, "Parcela do numerador (%)", "Share of the numerator (%)")) +
    ggplot2::theme_minimal(base_size = 12)
}

# Importance map of the preferred index (PC, IIC, AWF) at the first threshold,
# with links coloured by importance when they were computed.
.change_map <- function(patches, links, language) {
  metric <- c("PC", "IIC", "AWF")[c("PC", "IIC", "AWF") %in% sub("^d", "",
    intersect(names(patches), c("dPC", "dIIC", "dAWF")))][1]
  md <- min(patches$max_dist_m)
  p <- patches[patches$max_dist_m == md, , drop = FALSE]
  p$importance <- p[[paste0("d", metric)]]
  plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = p, ggplot2::aes(fill = importance), colour = "#555555",
                     linewidth = 0.15) +
    ggplot2::scale_fill_gradient(low = "#f3f6f1", high = "#125c40",
      name = paste0("d", metric, " (%)"))
  if (!is.null(links)) {
    l <- links[links$max_dist_m == md & links$metric == metric, , drop = FALSE]
    if (nrow(l)) {
      coords <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(p)))
      key <- paste(p$year, p$fragment_id)
      a <- match(paste(l$year, l$from), key)
      b <- match(paste(l$year, l$to), key)
      l$x <- coords[a, 1]; l$y <- coords[a, 2]; l$xend <- coords[b, 1]; l$yend <- coords[b, 2]
      l <- l[order(l$importance), , drop = FALSE]
      plot <- plot + ggplot2::geom_segment(data = l, ggplot2::aes(x = x, y = y, xend = xend,
        yend = yend, colour = importance), linewidth = 0.6) +
        ggplot2::scale_colour_gradient(low = "#c6dbef", high = "#08306b",
          name = .tr(language, "Liga\u00e7\u00e3o (%)", "Link (%)"))
    }
  }
  plot <- plot + ggplot2::coord_sf(datum = NA) +
    ggplot2::labs(x = NULL, y = NULL, subtitle = paste0("d", metric, ", ", md, " m")) +
    ggplot2::theme_minimal(base_size = 12)
  list(base = plot, metric = metric, max_dist = md)
}

.change_link_plot <- function(l, language) {
  l <- l[order(l$year, l$max_dist_m, l$metric, -l$importance), , drop = FALSE]
  key <- paste(l$year, l$max_dist_m, l$metric)
  l$rank <- stats::ave(seq_along(key), key, FUN = seq_along)
  l$panel <- paste0(l$metric, " | ", l$max_dist_m, " m")
  l$period <- factor(l$year)
  ggplot2::ggplot(l, ggplot2::aes(x = rank, y = importance, colour = period)) +
    ggplot2::geom_line(linewidth = 0.7) + ggplot2::geom_point(size = 1) +
    ggplot2::scale_colour_viridis_d(end = 0.9, name = .tr(language, "Ano", "Year")) +
    ggplot2::facet_wrap(~panel, scales = "free") +
    ggplot2::labs(x = .tr(language, "Posi\u00e7\u00e3o da liga\u00e7\u00e3o", "Link rank"),
                  y = .tr(language, "Import\u00e2ncia da liga\u00e7\u00e3o (%)", "Link importance (%)")) +
    ggplot2::theme_minimal(base_size = 12)
}

utils::globalVariables(c("year", "quantity", "period", "rank", "importance", "current_frame"))
