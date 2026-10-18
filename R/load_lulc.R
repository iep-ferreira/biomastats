#' Load land cover from global open sources in the load_rasters format
#'
#' Portable counterpart of [load_rasters()] for land cover outside MapBiomas.
#' It reads ESA WorldCover 10 m (2020 and 2021, worldwide) directly from its
#' Cloud-Optimized GeoTIFFs: only the window of the study area is transferred,
#' each tile is cropped and the merged result is masked with the same
#' cell-centre rule as `raster::mask`. The returned object has the same
#' structure as `load_rasters()` (`shape`, `time_range`, `raster`,
#' `collection`), so [get_area()], [land_vis()], [land_dist()],
#' [class_diversity()], [fragment_metrics()], [land_change()] and the other
#' analysis functions work unchanged. Class names and colours come from
#' `dict_build("esa_worldcover")`.
#'
#' A year whose tiles do not answer within `timeout` seconds (or fail) is
#' skipped and reported in `metadata$status`; when no year can be read the
#' function warns and returns `NULL`, so later analyses are not affected.
#' @param shape_path Path of the study-area shapefile (or an `sf` object);
#'   `NULL` uses the package example polygon.
#' @param source Land-cover source; currently `"esa_worldcover"`.
#' @param years Years to load (2020 and/or 2021).
#' @param timeout Seconds to wait for each remote response; default 5
#'   (option `biomastats.timeout`).
#' @return A maps object: `shape` (sf, EPSG:4326), `time_range`, `raster`
#'   (list of in-memory `RasterLayer`, one per year), `collection`
#'   (`"esa_worldcover"`) and `metadata` (product, version, catalogue year,
#'   tiles, status and time per year).
#' @examples
#' \dontrun{
#' wc <- load_lulc(system.file("shp", "UFSCar.shp", package = "biomastats"))
#' get_area(wc)$time_series
#' }
#' @export
load_lulc <- function(shape_path = NULL, source = c("esa_worldcover"), years = c(2020, 2021), timeout = NULL) {
  source <- match.arg(source)
  timeout <- .layer_timeout(timeout)
  if (is.null(shape_path)) shape_path <- system.file("shp", "polygon.shp", package = "biomastats")
  years <- sort(unique(as.integer(years)))
  versions <- c("2020" = "v100", "2021" = "v200")
  if (!length(years) || any(!as.character(years) %in% names(versions)))
    stop("ESA WorldCover est\u00e1 dispon\u00edvel para 2020 e 2021.")
  shape <- if (inherits(shape_path, c("sf", "sfc"))) sf::st_transform(sf::st_as_sf(shape_path), 4326)
    else sf::st_transform(sf::st_read(shape_path, quiet = TRUE), 4326)
  area <- .study_area(shape)
  tiles <- .worldcover_tiles(sf::st_bbox(area))
  blocks <- lapply(years, function(y) .layer_block(sprintf("ESA WorldCover %d", y),
    .with_gdal_network(timeout, .worldcover_read(area, tiles, y, versions[[as.character(y)]], timeout))))
  status <- data.frame(year = years, product = "ESA WorldCover 10 m", version = unname(versions[as.character(years)]),
    catalog_year = years, tiles = paste(tiles, collapse = ","),
    status = vapply(blocks, `[[`, "", "status"), message = vapply(blocks, `[[`, "", "message"),
    seconds = vapply(blocks, `[[`, 0, "seconds"), stringsAsFactors = FALSE)
  ok <- status$status == "ok"
  if (!any(ok)) {
    warning("Nenhum ano do ESA WorldCover p\u00f4de ser carregado; o bloco foi ignorado.", call. = FALSE)
    return(NULL)
  }
  kept <- years[ok]
  if (length(kept) > 1 && any(diff(kept) != 1)) kept <- kept[length(kept)]
  rasters <- lapply(blocks[match(kept, years)], `[[`, "value")
  list(shape = shape, time_range = c(min(kept), max(kept)), raster = rasters, collection = source,
       metadata = list(source = source, product = "ESA WorldCover 10 m", provider = "ESA",
                       license = "CC BY 4.0", catalog_years = kept, status = status,
                       method = "COG window read, crop per tile, merge, mask by cell centre"))
}

# 3 x 3 degree tiles named by their lower-left corner (e.g. S24W051).
.worldcover_tiles <- function(bb) {
  lat <- seq(floor(bb[["ymin"]] / 3) * 3, floor((bb[["ymax"]] - 1e-9) / 3) * 3, by = 3)
  lon <- seq(floor(bb[["xmin"]] / 3) * 3, floor((bb[["xmax"]] - 1e-9) / 3) * 3, by = 3)
  g <- expand.grid(lat = lat, lon = lon)
  sprintf("%s%02d%s%03d", ifelse(g$lat < 0, "S", "N"), abs(g$lat), ifelse(g$lon < 0, "W", "E"), abs(g$lon))
}

.worldcover_url <- function(tile, year, version) {
  root <- getOption("biomastats.worldcover_root", "https://esa-worldcover.s3.eu-central-1.amazonaws.com")
  sprintf("%s/%s/%d/map/ESA_WorldCover_10m_%d_%s_%s_Map.tif", root, version, year, year, version, tile)
}

.worldcover_read <- function(area, tiles, year, version, timeout) {
  v <- terra::vect(area)
  # A missing tile (HTTP 404, e.g. ocean) is ignored; an unreachable server skips the block.
  parts <- lapply(tiles, function(t) {
    u <- .worldcover_url(t, year, version)
    if (!.remote_exists(u, timeout)) return(NULL)
    tryCatch(terra::crop(terra::rast(.vsi(u)), v), error = function(e) NULL)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) stop("nenhuma tile do ESA WorldCover respondeu para a \u00e1rea")
  r <- if (length(parts) > 1L) do.call(terra::merge, unname(parts)) else parts[[1]]
  r <- terra::mask(r, v, touches = FALSE)
  r <- terra::classify(r, cbind(0, NA))  # 0 = no data in WorldCover
  .to_rasterlayer(r, sprintf("esa_worldcover_%d", year))
}
