#' Catalogue of external layers and their reference years
#'
#' Lists the free, keyless sources used by [load_lulc()], [load_features()] and
#' [load_physical()]: land cover (ESA WorldCover), elevation (Copernicus DEM),
#' soils (SoilGrids) and features (OpenStreetMap regional extracts and Overture
#' Maps). All cover the whole world, including Brazil. Each loader also records
#' the exact version and catalogue year of the data it actually read.
#' @param online If `TRUE`, resolves the current Overture release from its STAC
#'   catalogue (one request, skipped when it does not answer within `timeout`).
#' @param timeout Seconds to wait for a response; default 5
#'   (option `biomastats.timeout`).
#' @return A data frame with `id`, `theme`, `provider`, `product`, `version`,
#'   `catalog_year`, `coverage`, `resolution`, `access`, `license` and `url`.
#' @examples
#' layer_catalog()
#' @export
layer_catalog <- function(online = FALSE, timeout = NULL) {
  out <- .layer_registry()
  if (isTRUE(online)) {
    timeout <- .layer_timeout(timeout)
    rel <- .layer_block("Overture release", .fetch_json(paste0(.overture_stac_root(), "/catalog.json"), timeout)$latest)
    if (rel$status == "ok") {
      i <- out$id == "overture"
      out$version[i] <- rel$value
      out$catalog_year[i] <- substr(rel$value, 1, 4)
    }
  }
  out
}
