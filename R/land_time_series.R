#' Prepare annual land-cover time series for native analysis
#'
#' Creates a reusable analytical object from [get_area()]. It preserves the
#' dictionary, units and metadata. Use [series_correlation()],
#' [series_fit_regime()], [series_diagnostics()] and [series_forecast()] for
#' separate analytical stages.
#' @param data `get_area(obj)$time_series`, the complete `get_area()` result, or
#'   its `aggregate_data` table.
#' @param classes Optional class codes to retain.
#' @param language Labels for the plots, `"pt"` or `"en"`.
#' @param build_plot Return a ggplot that can be extended with ggplot2.
#' @return A `biomastats_time_series` list with `data`, `summary`, dictionary,
#'   metadata and `plot`. It has no fitted model and writes no files.
#' @export
land_time_series <- function(data, classes = NULL, language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  input <- .series_input(data, classes)
  d <- input$data
  summary <- do.call(rbind, lapply(split(d, d$code), function(z) {
    ok <- is.finite(z$area)
    first <- z$area[which(ok)[1]]
    last <- z$area[utils::tail(which(ok), 1)]
    data.frame(code = z$code[1], n = sum(ok), missing = sum(!ok), start = min(z$year),
      end = max(z$year), first_area = first, last_area = last, net_change = last - first,
      percent_change = if (is.finite(first) && first != 0) 100 * (last - first) / first else NA_real_,
      status = if (any(!ok)) "missing_data" else if (sum(ok) < 6) "insufficient_data" else
        if (stats::var(z$area) == 0) "constant" else "ready")
  }))
  rownames(summary) <- NULL
  out <- list(data = d, summary = summary, class_dictionary = input$dictionary,
              metadata = input$metadata, language = language, plot = NULL)
  class(out) <- "biomastats_time_series"
  if (isTRUE(build_plot)) out$plot <- .series_plot_data(out, language)
  out
}

# ---- Input contract --------------------------------------------------------

.as_series_object <- function(x) if (inherits(x, "biomastats_time_series")) x else land_time_series(x, build_plot = FALSE)

.series_one <- function(obj, code) {
  z <- obj$data[obj$data$code == as.numeric(code), c("year", "area")]
  if (!nrow(z)) stop("S\u00e9rie n\u00e3o encontrada no objeto.")
  z[order(z$year), ]
}

.series_input <- function(x, classes = NULL) {
  if (inherits(x, "ggplot")) x <- x$data
  if (is.list(x) && !is.data.frame(x)) x <- x$aggregate_data %||% x$time_series
  if (inherits(x, "ggplot")) x <- x$data
  if (!is.data.frame(x)) stop("data deve ser uma tabela ou objeto retornado por get_area().")
  cc <- if ("code" %in% names(x)) "code" else if ("land_class" %in% names(x)) "land_class" else stop("data deve conter land_class ou code.")
  ac <- if ("area_raw" %in% names(x)) "area_raw" else if ("area" %in% names(x)) "area" else stop("data deve conter area.")
  d <- data.frame(code = as.numeric(as.character(x[[cc]])), year = as.integer(x$year), area = as.numeric(x[[ac]]))
  if (anyDuplicated(d[c("code", "year")])) stop("Cada combina\u00e7\u00e3o de classe e ano deve ser \u00fanica; h\u00e1 duplicado.")
  md <- attr(x, "metadata") %||% list()
  collection <- md$collection %||% attr(x, "collection") %||% 10
  dic <- attr(x, "class_dictionary") %||% .biomastats_dictionary(collection, sort(unique(d$code)))
  years <- seq(min(d$year), max(d$year)); codes <- sort(unique(d$code))
  d <- merge(expand.grid(code = codes, year = years), d, all.x = TRUE, sort = TRUE)
  if (!is.null(classes)) {
    if (!all(classes %in% codes)) stop("classes cont\u00e9m c\u00f3digo ausente.")
    d <- d[d$code %in% classes, ]
  }
  list(data = d, dictionary = dic[dic$code %in% unique(d$code), ],
       metadata = c(md[setdiff(names(md), c("collection", "area_unit", "years"))],
                    list(collection = collection, area_unit = md$area_unit %||% "km2", years = years)))
}

