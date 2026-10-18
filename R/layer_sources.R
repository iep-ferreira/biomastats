# Internal infrastructure shared by the external-layer functions
# (load_lulc, load_features, load_physical, feature_raster, distance_to_nearest).
#
# Network rule: a request that does not respond within `timeout` seconds
# (default 5, option biomastats.timeout), fails or is rate limited skips only
# its block. The block returns NULL with status "skipped" and a message; the
# analysis continues, no partial file is left in the cache and the GDAL
# network settings are restored on exit.


.layer_timeout <- function(timeout = NULL) {
  t <- if (is.null(timeout)) getOption("biomastats.timeout", 5) else timeout
  if (!is.numeric(t) || length(t) != 1L || !is.finite(t) || t <= 0)
    stop("timeout deve ser um n\u00famero positivo de segundos.")
  t
}

# Runs one data block; errors and timeouts skip the block.
.layer_block <- function(label, expr) {
  t0 <- Sys.time()
  value <- tryCatch(force(expr), error = function(e) structure(list(), message = conditionMessage(e),
                                                              class = "biomastats_skipped"))
  skipped <- inherits(value, "biomastats_skipped") || is.null(value)
  msg <- if (inherits(value, "biomastats_skipped")) attr(value, "message") else if (is.null(value)) "sem dados" else ""
  if (skipped) message(sprintf("biomastats: bloco '%s' ignorado (%s).", label, msg))
  list(value = if (skipped) NULL else value, status = if (skipped) "skipped" else "ok", message = msg,
       seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}

# Temporary GDAL network settings for remote (COG/VRT) reads.
.with_gdal_network <- function(timeout, expr) {
  keys <- c(GDAL_HTTP_TIMEOUT = timeout, GDAL_HTTP_CONNECTTIMEOUT = timeout, GDAL_HTTP_MAX_RETRY = 0,
            GDAL_DISABLE_READDIR_ON_OPEN = "EMPTY_DIR", CPL_VSIL_CURL_ALLOWED_EXTENSIONS = ".tif,.tiff,.vrt")
  old <- vapply(names(keys), function(k) { v <- terra::getGDALconfig(k); if (length(v)) v[1] else "" }, "")
  for (k in names(keys)) terra::setGDALconfig(k, as.character(keys[[k]]))
  on.exit(for (k in names(keys)) terra::setGDALconfig(k, old[[k]]), add = TRUE)
  force(expr)
}

.vsi <- function(url) if (grepl("^https?://", url)) paste0("/vsicurl/", url) else url

# Fast reachability check before a GDAL remote read (GDAL retries internally).
# Returns FALSE for a missing file (HTTP 404); stops if the server does not answer.
.remote_exists <- function(url, timeout) {
  if (!grepl("^https?://", url)) return(file.exists(url))
  res <- curl::curl_fetch_memory(url, handle = curl::new_handle(nobody = TRUE, connecttimeout = timeout,
                                                                timeout = timeout, followlocation = TRUE))
  if (res$status_code == 404) return(FALSE)
  if (res$status_code >= 400) stop(sprintf("HTTP %d em %s", res$status_code, url))
  TRUE
}

# Downloads a file; aborts when the server does not connect or stalls for `timeout` seconds.
.fetch_file <- function(url, dest, timeout) {
  part <- paste0(dest, ".part")
  on.exit(unlink(part), add = TRUE)
  h <- curl::new_handle(connecttimeout = timeout, low_speed_limit = 1024, low_speed_time = timeout,
                        followlocation = TRUE)
  res <- curl::curl_fetch_disk(url, part, handle = h)
  if (res$status_code >= 400) stop(sprintf("HTTP %d em %s", res$status_code, url))
  if (!file.rename(part, dest)) { file.copy(part, dest, overwrite = TRUE) }
  headers <- curl::parse_headers_list(res$headers)
  list(path = dest, last_modified = headers[["last-modified"]] %||% NA_character_)
}

# Small JSON documents must answer within `timeout` seconds.
.fetch_json <- function(url, timeout) {
  res <- curl::curl_fetch_memory(url, handle = curl::new_handle(connecttimeout = timeout, timeout = timeout))
  if (res$status_code >= 400) stop(sprintf("HTTP %d em %s", res$status_code, url))
  jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE)
}

.layer_cache_dir <- function(...) {
  d <- file.path(tools::R_user_dir("biomastats", which = "cache"), ...)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Study area as a single valid polygon in EPSG:4326, from a maps object, sf or shapefile.
.study_area <- function(x) {
  s <- if (inherits(x, c("sf", "sfc"))) x
    else if (is.list(x) && !is.null(x$shape)) x$shape
    else if (is.character(x) && length(x) == 1L && file.exists(x)) sf::st_read(x, quiet = TRUE)
    else stop("Informe o objeto de mapas, um objeto sf ou o caminho de um shapefile.")
  g <- sf::st_make_valid(sf::st_union(sf::st_transform(sf::st_geometry(s), 4326)))
  sf::st_sf(geometry = g)
}

# Reference grid (SpatRaster) of one year of a maps object.
.reference_raster <- function(maps, year = NULL) {
  if (!is.list(maps) || is.null(maps$raster) || length(maps$time_range) != 2L)
    stop("maps deve ser um objeto de mapas (load_rasters ou load_lulc).")
  years <- seq(maps$time_range[1], maps$time_range[2])
  if (is.null(year)) year <- max(years)
  if (!year %in% years) stop("year fora do intervalo do objeto de mapas.")
  list(raster = .as_terra(maps$raster[[match(year, years)]]), year = year)
}

# In-memory RasterLayer built directly from a SpatRaster (avoids the gc() in raster::raster()).
.to_rasterlayer <- function(r, name = names(r)[1]) {
  e <- terra::ext(r)
  out <- raster::raster(nrows = terra::nrow(r), ncols = terra::ncol(r), xmn = e$xmin, xmx = e$xmax,
                        ymn = e$ymin, ymx = e$ymax, crs = terra::crs(r, proj = TRUE))
  out <- raster::setValues(out, as.numeric(terra::values(r, mat = FALSE)))
  names(out) <- name
  out
}

# Year of an HTTP date or ISO date, independent of the locale.
.year_of <- function(x) {
  y <- regmatches(as.character(x), regexpr("(19|20)[0-9]{2}", as.character(x)))
  if (length(y)) as.integer(y) else NA_integer_
}

# Catalogue of the external sources and the year each layer refers to.
.layer_registry <- function() {
  data.frame(
    id = c("esa_worldcover_2020", "esa_worldcover_2021", "copernicus_dem", "soilgrids",
           "osm_geofabrik", "osm_openstreetmap_fr", "overture"),
    theme = c("lulc", "lulc", "physical", "physical", "features", "features", "features"),
    provider = c("ESA", "ESA", "Copernicus / ESA", "ISRIC", "OpenStreetMap (Geofabrik)",
                 "OpenStreetMap (openstreetmap.fr)", "Overture Maps Foundation"),
    product = c("ESA WorldCover 10 m", "ESA WorldCover 10 m", "Copernicus DEM GLO-30", "SoilGrids 250 m",
                "OSM regional extract (.osm.pbf)", "OSM regional extract (.osm.pbf)", "Overture Maps GeoParquet"),
    version = c("v100", "v200", "GLO-30 COG (AWS Open Data)", "SoilGrids 2.0", "latest", "latest", "latest release"),
    catalog_year = c("2020", "2021", "2011-2015 (TanDEM-X)", "2021", "extract date", "extract date", "release date"),
    coverage = "world",
    resolution = c("10 m", "10 m", "30 m", "250 m", "vector", "vector", "vector"),
    access = c("COG over HTTP", "COG over HTTP", "COG over HTTP", "WCS (subset)", "PBF download + local cache",
               "PBF download + local cache", "STAC index + GeoParquet on S3"),
    license = c("CC BY 4.0", "CC BY 4.0", "Copernicus DEM licence (free)", "CC BY 4.0", "ODbL", "ODbL", "ODbL / CDLA"),
    url = c("https://esa-worldcover.s3.eu-central-1.amazonaws.com", "https://esa-worldcover.s3.eu-central-1.amazonaws.com",
            "https://copernicus-dem-30m.s3.amazonaws.com", "https://maps.isric.org/mapserv",
            "https://download.geofabrik.de", "http://download.openstreetmap.fr", "https://stac.overturemaps.org"),
    stringsAsFactors = FALSE)
}
