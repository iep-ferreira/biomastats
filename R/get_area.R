#' Calculate Land Classes Area
#'
#' Calculates areas in square kilometres and preserves the native analytical
#' contract on the returned list, table and ggplot. Absent classes in observed
#' years receive zero; years without valid pixels remain NA.
#' @param data Native maps object returned by load_rasters.
#' @param dec_places Decimal places for displayed areas. Statistical analysis
#'   uses the unrounded area_raw column.
#' @param plot_type Either "areaplot" (default) or "profile".
#' @return List with Years, Occupied area, aggregate_data, time_series and
#'   collection. Attributes metadata and class_dictionary are inherited by the
#'   list, table and time_series ggplot. Metadata records coverage and units.
#' @export
get_area <- function(data = NULL, dec_places = 3, plot_type = "areaplot") {
  plot_type <- match.arg(plot_type, c("areaplot", "profile"))
  if (!is.list(data) || length(data$time_range) != 2L ||
      any(!is.finite(data$time_range)) || any(data$time_range != trunc(data$time_range)) ||
      data$time_range[2] < data$time_range[1]) stop("data deve ser um objeto de mapas com time_range valido.")
  years <- seq.int(data$time_range[1], data$time_range[2])
  if (length(data$raster) != length(years)) stop("Anos e rasters incompativeis.")
  if (length(dec_places) != 1L || !is.numeric(dec_places) || !is.finite(dec_places) ||
      dec_places < 0 || dec_places != trunc(dec_places)) stop("dec_places deve ser inteiro nao negativo.")
  collection <- .biomastats_collection(data$collection)
  areas <- coverage <- source <- vector("list", length(years))
  common_support <- TRUE
  for (i in seq_along(years)) {
    r <- terra::rast(data$raster[[i]])
    if (terra::nlyr(r) != 1L || !nzchar(terra::crs(r))) stop("Cada ano deve ter um raster categorico com CRS.")
    v <- terra::values(r, mat = FALSE)
    if (any(!is.na(v) & (!is.finite(v) | v != trunc(v)))) stop("Classes devem ser codigos inteiros.")
    a <- terra::values(terra::cellSize(r, unit = "km", mask = FALSE), mat = FALSE)
    ok <- !is.na(v)
    if (i == 1L) { first_raster <- r; first_mask <- ok } else {
      common_support <- common_support && isTRUE(terra::compareGeom(first_raster,r,stopOnError=FALSE)) && identical(first_mask,ok)
    }
    areas[[i]] <- tapply(a[ok], v[ok], sum)
    coverage[[i]] <- data.frame(year = years[i], valid_cells = sum(ok),
      observed_area_km2 = sum(a[ok]), grid_cells = length(v))
    source[[i]] <- list(crs = terra::crs(r), resolution = terra::res(r))
  }
  codes <- sort(unique(as.numeric(unlist(lapply(areas, names)))))
  if (!length(codes)) stop("Nenhuma classe observada nos rasters.")
  dic <- .biomastats_dictionary(collection, codes, data$class_dictionary)
  df <- do.call(rbind, lapply(seq_along(years), function(i) {
    a <- as.numeric(areas[[i]][match(as.character(codes), names(areas[[i]]))])
    if (coverage[[i]]$valid_cells > 0) a[is.na(a)] <- 0
    data.frame(land_class = as.character(codes), year = years[i], area = round(a, dec_places),
      area_raw = a, land_class_name = dic$class, classe = dic$classe, color = dic$color)
  }))
  palette <- stats::setNames(dic$color, as.character(dic$code))
  labels <- paste(dic$code, dic$class)
  p <- ggplot2::ggplot(df, ggplot2::aes(x = year, y = area, group = land_class))
  if (plot_type == "profile") p <- p +
    ggplot2::geom_line(ggplot2::aes(colour = land_class)) +
    ggplot2::geom_point(ggplot2::aes(colour = land_class)) +
    ggplot2::scale_colour_manual(values = palette, breaks = names(palette), labels = labels, name = "Land use")
  else p <- p + ggplot2::geom_area(ggplot2::aes(fill = land_class)) +
    ggplot2::scale_fill_manual(values = palette, breaks = names(palette), labels = labels, name = "Land use")
  p <- p + ggplot2::theme_minimal() + ggplot2::labs(x = "Year", y = "Area (km2)")
  metadata <- list(schema_version = "1.0", collection = collection, classes = codes,
    years = years, area_unit = "km2", source = stats::setNames(source, years),
    coverage = do.call(rbind, coverage), common_support = common_support, upstream = data$metadata,
    absent_class = "zero only in years with observed pixels", precision = "area_raw unrounded")
  attr(df, "metadata") <- attr(p, "metadata") <- metadata
  attr(df, "class_dictionary") <- attr(p, "class_dictionary") <- dic
  p$data <- df
  out <- list(Years = years, "Occupied area" = areas, aggregate_data = df,
    time_series = p, collection = collection)
  attr(out, "metadata") <- metadata
  attr(out, "class_dictionary") <- dic
  out
}
