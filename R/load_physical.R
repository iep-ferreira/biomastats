#' Load physical-environment layers aligned to the land-cover grid
#'
#' Reads terrain and soil layers for the study area of a maps object and aligns
#' them to its grid (same CRS, extent and resolution), so they can be combined
#' cell by cell with the land-cover maps. Works worldwide:
#' \itemize{
#'   \item `elevation`, `slope` and `aspect` from Copernicus DEM GLO-30
#'     (Cloud-Optimized GeoTIFF; only the study-area window is read). Slope and
#'     aspect are computed on the DEM grid (metric distances also for
#'     geographic grids) before alignment;
#'   \item soil properties at `depth` from SoilGrids 2.0 (WCS subset):
#'     `clay`, `sand`, `silt` (%), `soc` (g/kg), `ph` (pH in water),
#'     `bdod` (g/cm3), `cec` (cmol(c)/kg) and `nitrogen` (g/kg).
#' }
#' External covariates are resampled onto the land-cover grid (bilinear;
#' nearest neighbour for aspect) and masked to the study area; the land-cover
#' maps themselves are not modified. Each layer is an independent block: a
#' source that does not answer within `timeout` seconds or fails is skipped
#' (status `"skipped"`) and the other layers are still returned.
#' @param maps Maps object ([load_rasters()] or [load_lulc()]).
#' @param layers Layers to load (see Details).
#' @param year Year of the reference grid; `NULL` uses the last year.
#' @param depth SoilGrids depth interval: `"0-5cm"`, `"5-15cm"`, `"15-30cm"`,
#'   `"30-60cm"`, `"60-100cm"` or `"100-200cm"`.
#' @param timeout Seconds to wait for each response; default 5
#'   (option `biomastats.timeout`).
#' @param language Plot labels, `"pt"` or `"en"`.
#' @param build_plot Return editable ggplots.
#' @return A `biomastats_physical` list: `rasters` (named list of in-memory
#'   `RasterLayer` on the reference grid), `status` (layer, product, version,
#'   catalog_year, unit, seconds, status, message), `metadata` and `plots`.
#' @examples
#' \dontrun{
#' load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"))
#' meio <- load_physical(mapas, layers = c("elevation", "slope", "clay"))
#' meio$status
#' }
#' @export
load_physical <- function(maps, layers = c("elevation", "slope", "aspect"), year = NULL, depth = "0-5cm",
                          timeout = NULL, language = c("pt", "en"), build_plot = TRUE) {
  language <- match.arg(language)
  timeout <- .layer_timeout(timeout)
  terrain <- c("elevation", "slope", "aspect")
  soil <- names(.soil_properties())
  layers <- match.arg(layers, c(terrain, soil), several.ok = TRUE)
  depth <- match.arg(depth, c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm"))
  ref <- .reference_raster(maps, year); r0 <- ref$raster
  bb <- .grid_bbox_lonlat(r0, margin = 0.005)
  out <- list()
  rows <- list()
  if (any(layers %in% terrain)) {
    dem <- .layer_block("Copernicus DEM", .with_gdal_network(timeout, .copernicus_read(bb, timeout)))
    for (l in intersect(terrain, layers)) {
      blk <- if (dem$status != "ok") list(value = NULL, status = "skipped", message = dem$message, seconds = 0)
        else .layer_block(paste("terreno", l), .align_layer(.terrain_layer(dem$value, l), r0, if (l == "aspect") "near" else "bilinear"))
      blk$seconds <- blk$seconds + if (l == intersect(terrain, layers)[1]) dem$seconds else 0
      out[[l]] <- blk$value
      rows[[l]] <- data.frame(layer = l, product = "Copernicus DEM GLO-30", version = "GLO-30 COG",
        catalog_year = "2011-2015 (TanDEM-X)", unit = c(elevation = "m", slope = "graus", aspect = "graus")[[l]],
        seconds = blk$seconds, status = blk$status, message = blk$message, stringsAsFactors = FALSE)
    }
  }
  for (p in intersect(soil, layers)) {
    info <- .soil_properties()[[p]]
    blk <- .layer_block(paste("SoilGrids", p), .align_layer(.soilgrids_read(info, depth, bb, timeout), r0, "bilinear"))
    out[[p]] <- blk$value
    rows[[p]] <- data.frame(layer = p, product = paste("SoilGrids 2.0", depth), version = "SoilGrids 2.0",
      catalog_year = "2021", unit = info$unit, seconds = blk$seconds, status = blk$status, message = blk$message,
      stringsAsFactors = FALSE)
  }
  rasters <- lapply(names(out)[!vapply(out, is.null, TRUE)], function(n) .to_rasterlayer(out[[n]], n))
  names(rasters) <- names(out)[!vapply(out, is.null, TRUE)]
  status <- do.call(rbind, rows[layers]); rownames(status) <- NULL
  res <- list(rasters = rasters, status = status,
              metadata = list(year = ref$year, depth = depth, crs = terra::crs(r0), resolution = terra::res(r0),
                              resampling = "bilinear (aspect: nearest neighbour) onto the land-cover grid"),
              shape = maps$shape, plots = NULL)
  class(res) <- "biomastats_physical"
  if (isTRUE(build_plot) && length(rasters)) res$plots <- lapply(names(rasters), function(n)
    .layer_raster_plot(rasters[[n]], maps$shape, .physical_label(n, status, language), language))
  if (!is.null(res$plots)) names(res$plots) <- names(rasters)
  res
}

.soil_properties <- function() list(
  clay = list(coverage = "clay", unit = "%", factor = 0.1),
  sand = list(coverage = "sand", unit = "%", factor = 0.1),
  silt = list(coverage = "silt", unit = "%", factor = 0.1),
  soc = list(coverage = "soc", unit = "g/kg", factor = 0.1),
  ph = list(coverage = "phh2o", unit = "pH", factor = 0.1),
  bdod = list(coverage = "bdod", unit = "g/cm3", factor = 0.01),
  cec = list(coverage = "cec", unit = "cmol(c)/kg", factor = 0.1),
  nitrogen = list(coverage = "nitrogen", unit = "g/kg", factor = 0.01))

# Bounding box of a grid in longitude/latitude (only the corner points are transformed).
.grid_bbox_lonlat <- function(r, margin = 0) {
  e <- terra::ext(r)
  bb <- c(xmin = unname(e$xmin), ymin = unname(e$ymin), xmax = unname(e$xmax), ymax = unname(e$ymax))
  if (!terra::is.lonlat(r)) {
    pts <- sf::st_sfc(sf::st_multipoint(rbind(c(e$xmin, e$ymin), c(e$xmax, e$ymax), c(e$xmin, e$ymax), c(e$xmax, e$ymin))),
                      crs = sf::st_crs(terra::crs(r)))
    b <- sf::st_bbox(sf::st_transform(pts, 4326)); bb <- c(xmin = b[["xmin"]], ymin = b[["ymin"]], xmax = b[["xmax"]], ymax = b[["ymax"]])
  }
  bb + c(-margin, -margin, margin, margin)
}

# 1 x 1 degree tiles named by their lower-left corner.
.copernicus_tiles <- function(bb) {
  g <- expand.grid(lat = floor(bb[["ymin"]]):floor(bb[["ymax"]] - 1e-9), lon = floor(bb[["xmin"]]):floor(bb[["xmax"]] - 1e-9))
  sprintf("Copernicus_DSM_COG_10_%s%02d_00_%s%03d_00_DEM", ifelse(g$lat < 0, "S", "N"), abs(g$lat),
          ifelse(g$lon < 0, "W", "E"), abs(g$lon))
}

.copernicus_read <- function(bb, timeout) {
  root <- getOption("biomastats.copernicus_root", "https://copernicus-dem-30m.s3.amazonaws.com")
  e <- terra::ext(bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]])
  parts <- lapply(.copernicus_tiles(bb), function(t) {
    u <- sprintf("%s/%s/%s.tif", root, t, t)
    if (!.remote_exists(u, timeout)) return(NULL)
    tryCatch(terra::crop(terra::rast(.vsi(u)), e), error = function(err) NULL)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) stop("nenhuma tile do Copernicus DEM respondeu para a \u00e1rea")
  r <- if (length(parts) > 1L) do.call(terra::merge, unname(parts)) else parts[[1]]
  terra::toMemory(r)
}

.terrain_layer <- function(dem, layer) {
  if (layer == "elevation") return(dem)
  terra::terrain(dem, v = layer, unit = "degrees")
}

.soilgrids_read <- function(info, depth, bb, timeout) {
  root <- getOption("biomastats.soilgrids_wcs", "https://maps.isric.org/mapserv")
  cov <- sprintf("%s_%s_mean", info$coverage, depth)
  crs <- "http://www.opengis.net/def/crs/EPSG/0/4326"
  url <- sprintf(paste0("%s?map=/map/%s.map&SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&COVERAGEID=%s",
                        "&FORMAT=image/tiff&SUBSET=long(%.6f,%.6f)&SUBSET=lat(%.6f,%.6f)&SUBSETTINGCRS=%s&OUTPUTCRS=%s"),
                 root, info$coverage, cov, bb[["xmin"]], bb[["xmax"]], bb[["ymin"]], bb[["ymax"]], crs, crs)
  f <- tempfile(fileext = ".tif")
  .fetch_file(url, f, timeout)
  r <- terra::toMemory(terra::rast(f))
  unlink(f)
  r * info$factor
}

# Resamples an external layer onto the reference grid and masks it to the study area.
.align_layer <- function(x, r0, method) {
  y <- terra::project(x, r0, method = method)
  terra::mask(y, r0)
}

.physical_label <- function(layer, status, language) {
  names_pt <- c(elevation = "Eleva\u00e7\u00e3o", slope = "Declividade", aspect = "Aspecto", clay = "Argila", sand = "Areia",
                silt = "Silte", soc = "Carbono org\u00e2nico", ph = "pH", bdod = "Densidade do solo", cec = "CTC",
                nitrogen = "Nitrog\u00eanio")
  names_en <- c(elevation = "Elevation", slope = "Slope", aspect = "Aspect", clay = "Clay", sand = "Sand",
                silt = "Silt", soc = "Organic carbon", ph = "pH", bdod = "Bulk density", cec = "CEC",
                nitrogen = "Nitrogen")
  unit <- status$unit[status$layer == layer]
  unit <- if (identical(language, "en")) sub("graus", "degrees", unit) else unit
  sprintf("%s (%s)", if (identical(language, "en")) names_en[[layer]] else names_pt[[layer]], unit)
}

# Map of one numeric layer with the study-area outline (shared by the layer functions).
.layer_raster_plot <- function(r, shape, label, language, caption = NULL) {
  d <- as.data.frame(raster::rasterToPoints(r))
  names(d) <- c("x", "y", "value")
  p <- ggplot2::ggplot(d) + ggplot2::geom_raster(ggplot2::aes(x, y, fill = value)) +
    ggplot2::scale_fill_viridis_c(name = label)
  if (!is.null(shape)) p <- p + ggplot2::geom_sf(data = sf::st_transform(sf::st_geometry(shape), sf::st_crs(raster::crs(r))),
                                                 fill = NA, colour = "grey15", linewidth = .3, inherit.aes = FALSE)
  p + .series_theme() + ggplot2::theme(axis.title = ggplot2::element_blank(), legend.key.width = ggplot2::unit(2.5, "lines")) +
    ggplot2::labs(caption = caption)
}
