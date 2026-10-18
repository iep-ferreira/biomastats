#' Load infrastructure and landscape features for the study area
#'
#' Retrieves vector features around the same study area used by the land-cover
#' analyses, from two free, worldwide sources without request limits:
#' \itemize{
#'   \item `source = "osm"`: an OpenStreetMap regional extract (`.osm.pbf`) from
#'     Geofabrik or openstreetmap.fr, chosen automatically by
#'     `osmextract::oe_match()`, downloaded once to a local cache and read with
#'     a spatial filter (no Overpass queries);
#'   \item `source = "overture"`: Overture Maps GeoParquet. A per-release STAC
#'     index (cached) selects only the files that intersect the area, which are
#'     read with a bounding-box filter.
#' }
#' Features are returned in EPSG:4326 within the study area enlarged by
#' `buffer_m`, so distances near the border consider features just outside it.
#' Each feature is an independent block: if a source does not answer within
#' `timeout` seconds, is rate limited or fails, that feature is `NULL`, its
#' status is `"skipped"` and the remaining features and analyses continue.
#' @param x Maps object ([load_rasters()] or [load_lulc()]), `sf` object or
#'   shapefile path defining the study area.
#' @param features One or more of `"roads"`, `"railways"`, `"waterways"`,
#'   `"water_bodies"`, `"buildings"` and `"places"`.
#' @param source `"osm"` (default) or `"overture"`.
#' @param provider OSM extract provider: `"auto"` (default; the smallest extract that contains the area among
#'   Geofabrik and openstreetmap.fr, e.g. a Brazilian state), `"geofabrik"` or `"openstreetmap_fr"`.
#' @param buffer_m Buffer in metres around the study area (default 2000).
#' @param timeout Seconds to wait for each response; default 5
#'   (option `biomastats.timeout`). Large extracts keep downloading while data
#'   flows; the download aborts if it stalls for `timeout` seconds.
#' @param cache_dir Cache folder for extracts and indexes; default
#'   `tools::R_user_dir("biomastats", "cache")`.
#' @param refresh Download the OSM extract again even if it is cached.
#' @param max_extract_mb Largest OSM extract (MB) to download; a larger one is
#'   skipped with a message suggesting a smaller buffer, another provider or
#'   `source = "overture"`. Default 1000.
#' @return A `biomastats_features` list: `features` (named list of `sf`, `NULL`
#'   for skipped features), `status` (feature, source, product, version,
#'   catalog_year, n, geometry, seconds, status, message), `area`,
#'   `query_area` and `metadata`.
#' @examples
#' \dontrun{
#' f <- load_features(system.file("shp", "UFSCar.shp", package = "biomastats"),
#'                    features = c("roads", "buildings"), source = "overture")
#' f$status
#' }
#' @export
load_features <- function(x, features = c("roads", "waterways", "buildings", "places"),
                          source = c("osm", "overture"), provider = c("auto", "geofabrik", "openstreetmap_fr"),
                          buffer_m = 2000, timeout = NULL, cache_dir = NULL, refresh = FALSE, max_extract_mb = 1000) {
  source <- match.arg(source); provider <- match.arg(provider)
  timeout <- .layer_timeout(timeout)
  specs <- .feature_specs()
  features <- match.arg(features, names(specs), several.ok = TRUE)
  if (!is.numeric(buffer_m) || length(buffer_m) != 1L || buffer_m < 0) stop("buffer_m deve ser um n\u00famero n\u00e3o negativo.")
  area <- .study_area(x)
  query_area <- if (buffer_m > 0) sf::st_sf(geometry = sf::st_buffer(sf::st_geometry(area), buffer_m)) else area
  res <- if (source == "osm") .features_osm(features, specs, query_area, provider, timeout, cache_dir, refresh, max_extract_mb)
    else .features_overture(features, specs, query_area, timeout, cache_dir)
  out <- list(features = stats::setNames(lapply(res, `[[`, "value"), features),
              status = do.call(rbind, lapply(res, `[[`, "row")),
              area = area, query_area = query_area,
              metadata = list(source = source, provider = if (source == "osm") provider else NA_character_,
                              buffer_m = buffer_m, crs = "EPSG:4326", retrieved = Sys.time()))
  class(out) <- "biomastats_features"
  out
}

.feature_specs <- function() list(
  roads = list(family = "line", osm = list(layer = "lines", where = "highway IS NOT NULL"),
               overture = list(type = "transportation/segment", subtype = "road")),
  railways = list(family = "line", osm = list(layer = "lines", where = "railway IS NOT NULL"),
                  overture = list(type = "transportation/segment", subtype = "rail")),
  waterways = list(family = "line", osm = list(layer = "lines", where = "waterway IS NOT NULL"),
                   overture = list(type = "base/water", subtype = NULL)),
  water_bodies = list(family = "polygon",
                      osm = list(layer = "multipolygons", where = "\"natural\" = 'water' OR landuse = 'reservoir'"),
                      overture = list(type = "base/water", subtype = NULL)),
  buildings = list(family = "polygon", osm = list(layer = "multipolygons", where = "building IS NOT NULL"),
                   overture = list(type = "buildings/building", subtype = NULL)),
  places = list(family = "point", osm = list(layer = "points", where = "amenity IS NOT NULL OR shop IS NOT NULL OR tourism IS NOT NULL"),
                overture = list(type = "places/place", subtype = NULL)))

.osm_extra_tags <- function(layer) switch(layer, points = c("amenity", "shop", "tourism"), lines = NULL, multipolygons = NULL)

.feature_row <- function(feature, source, product, version, catalog_year, value, block) {
  data.frame(feature = feature, source = source, product = product, version = version %||% NA_character_,
             catalog_year = catalog_year %||% NA_integer_, n = if (is.null(value)) 0L else nrow(value),
             geometry = if (is.null(value) || !nrow(value)) NA_character_ else .geometry_family(value),
             seconds = block$seconds, status = block$status, message = block$message, stringsAsFactors = FALSE)
}

# Keeps only the geometry family of a feature (points, lines or polygons).
.keep_family <- function(g, family) {
  if (is.null(g) || !nrow(g)) return(g)
  type <- as.character(sf::st_geometry_type(g))
  keep <- switch(family, point = type %in% c("POINT", "MULTIPOINT"),
                 line = type %in% c("LINESTRING", "MULTILINESTRING"),
                 polygon = type %in% c("POLYGON", "MULTIPOLYGON"))
  g[keep & !sf::st_is_empty(g), , drop = FALSE]
}

.geometry_family <- function(g) {
  type <- unique(as.character(sf::st_geometry_type(g)))
  fam <- unique(ifelse(type %in% c("POINT", "MULTIPOINT"), "point",
                ifelse(type %in% c("LINESTRING", "MULTILINESTRING"), "line",
                ifelse(type %in% c("POLYGON", "MULTIPOLYGON"), "polygon", "other"))))
  if (length(fam) != 1L || fam == "other") stop("As fei\u00e7\u00f5es devem ser s\u00f3 pontos, s\u00f3 linhas ou s\u00f3 pol\u00edgonos.")
  fam
}

# ---- OpenStreetMap regional extracts ----------------------------------------

.features_osm <- function(features, specs, query_area, provider, timeout, cache_dir, refresh, max_extract_mb) {
  ext <- .layer_block("extrato OSM", {
    if (!requireNamespace("osmextract", quietly = TRUE)) stop("pacote osmextract n\u00e3o instalado")
    .osm_extract(query_area, provider, timeout, cache_dir %||% .layer_cache_dir("osm"), refresh, max_extract_mb)
  })
  wkt <- sf::st_as_text(sf::st_geometry(query_area)[[1]])
  lapply(features, function(f) {
    sp <- specs[[f]]
    if (ext$status != "ok") {
      blk <- list(seconds = 0, status = "skipped", message = paste("extrato OSM indispon\u00edvel:", ext$message))
      return(list(value = NULL, row = .feature_row(f, "osm", provider, NA, NA, NULL, blk)))
    }
    e <- ext$value
    blk <- .layer_block(paste("OSM", f), {
      g <- osmextract::oe_read(e$pbf, layer = sp$osm$layer, quiet = TRUE, extra_tags = .osm_extra_tags(sp$osm$layer),
                               query = sprintf("SELECT * FROM %s WHERE %s", sp$osm$layer, sp$osm$where),
                               wkt_filter = wkt)
      g <- .keep_family(sf::st_transform(g, 4326), sp$family)
      g[lengths(sf::st_intersects(g, query_area)) > 0, , drop = FALSE]
    })
    list(value = blk$value, row = .feature_row(f, "osm", paste("OSM", e$zone), e$last_modified, e$catalog_year, blk$value, blk))
  })
}

.osm_extract <- function(query_area, provider, timeout, cache, refresh, max_extract_mb) {
  candidates <- if (provider == "auto") c("geofabrik", "openstreetmap_fr") else provider
  matches <- lapply(candidates, function(p) tryCatch(c(osmextract::oe_match(sf::st_geometry(query_area), provider = p, quiet = TRUE),
                                                       provider = p), error = function(e) NULL))
  matches <- Filter(Negate(is.null), matches)
  if (!length(matches)) stop("nenhum extrato OSM cont\u00e9m a \u00e1rea")
  m <- matches[[which.min(vapply(matches, function(z) as.numeric(z$file_size), 0))]]
  provider <- m$provider
  if (as.numeric(m$file_size) / 2^20 > max_extract_mb)
    stop(sprintf("o menor extrato que cont\u00e9m a \u00e1rea (%s, %.0f MB) excede max_extract_mb; reduza buffer_m, aumente max_extract_mb ou use source = \"overture\"",
                 basename(m$url), as.numeric(m$file_size) / 2^20))
  dest <- file.path(cache, paste0(provider, "_", basename(m$url)))
  meta_file <- paste0(dest, ".meta.rds")
  if (refresh) unlink(c(dest, meta_file, sub("[.]osm[.]pbf$", ".gpkg", dest)))
  if (!file.exists(dest)) {
    r <- .fetch_file(m$url, dest, timeout)
    saveRDS(list(url = m$url, last_modified = r$last_modified, retrieved = Sys.time()), meta_file)
  }
  meta <- if (file.exists(meta_file)) readRDS(meta_file) else list(url = m$url, last_modified = NA, retrieved = file.mtime(dest))
  date <- if (!is.na(meta$last_modified)) meta$last_modified else format(meta$retrieved, "%Y-%m-%d")
  list(pbf = dest, url = m$url, provider = provider, zone = sub("-latest[.]osm[.]pbf$", "", basename(m$url)),
       last_modified = date, catalog_year = .year_of(date))
}

# ---- Overture Maps ------------------------------------------------------------

.overture_stac_root <- function() getOption("biomastats.overture_stac", "https://stac.overturemaps.org")

.features_overture <- function(features, specs, query_area, timeout, cache_dir) {
  rel <- .layer_block("release Overture", {
    if (!requireNamespace("arrow", quietly = TRUE)) stop("pacote arrow n\u00e3o instalado")
    .fetch_json(paste0(.overture_stac_root(), "/catalog.json"), timeout)$latest
  })
  cache <- cache_dir %||% .layer_cache_dir("overture")
  bb <- sf::st_bbox(query_area)
  lapply(features, function(f) {
    sp <- specs[[f]]
    if (rel$status != "ok") {
      blk <- list(seconds = 0, status = "skipped", message = paste("cat\u00e1logo Overture indispon\u00edvel:", rel$message))
      return(list(value = NULL, row = .feature_row(f, "overture", "Overture Maps", NA, NA, NULL, blk)))
    }
    blk <- .layer_block(paste("Overture", f), {
      idx <- .overture_index(rel$value, sp$overture$type, timeout, cache)
      g <- .overture_read(idx, bb, sp$overture$subtype, timeout)
      g <- .keep_family(g, sp$family)
      g[lengths(sf::st_intersects(g, query_area)) > 0, , drop = FALSE]
    })
    list(value = blk$value, row = .feature_row(f, "overture", paste("Overture", sp$overture$type), rel$value,
                                               as.integer(substr(rel$value, 1, 4)), blk$value, blk))
  })
}

# Bounding box and URL of every GeoParquet file of a release/type, from STAC (cached).
.overture_index <- function(release, type, timeout, cache) {
  f <- file.path(cache, release, paste0(gsub("/", "_", type), ".rds"))
  if (file.exists(f)) return(readRDS(f))
  col <- .fetch_json(sprintf("%s/%s/%s/collection.json", .overture_stac_root(), release, type), timeout)
  urls <- vapply(Filter(function(l) identical(l$rel, "item"), col$links), `[[`, "", "href")
  if (!length(urls)) stop("\u00edndice STAC do Overture sem itens")
  items <- vector("list", length(urls))
  pool <- curl::new_pool(total_con = 32, host_con = 16)
  for (i in seq_along(urls)) local({
    k <- i
    curl::curl_fetch_multi(urls[k], pool = pool, handle = curl::new_handle(connecttimeout = timeout, timeout = timeout),
      done = function(r) if (r$status_code < 400) items[[k]] <<- jsonlite::fromJSON(rawToChar(r$content), simplifyVector = FALSE),
      fail = function(e) NULL)
  })
  curl::multi_run(pool = pool)
  if (any(vapply(items, is.null, TRUE))) stop("\u00edndice STAC do Overture incompleto (servidor n\u00e3o respondeu a tempo)")
  idx <- data.frame(t(vapply(items, function(x) as.numeric(unlist(x$bbox))[1:4], numeric(4))),
                    href = vapply(items, function(x) x$assets$aws$href, ""), stringsAsFactors = FALSE)
  names(idx)[1:4] <- c("xmin", "ymin", "xmax", "ymax")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(idx, f)
  idx
}

.overture_read <- function(idx, bb, subtype, timeout) {
  hit <- which(idx$xmin <= bb[["xmax"]] & idx$xmax >= bb[["xmin"]] & idx$ymin <= bb[["ymax"]] & idx$ymax >= bb[["ymin"]])
  empty <- sf::st_sf(id = character(), geometry = sf::st_sfc(crs = 4326))
  if (!length(hit)) return(empty)
  href <- idx$href[hit]
  region <- sub("^https://[^.]+[.]s3[.]([^.]+)[.]amazonaws[.]com/.*$", "\\1", href[1])
  paths <- sub("^https://([^.]+)[.]s3[.][^.]+[.]amazonaws[.]com/", "\\1/", href)
  fs <- arrow::S3FileSystem$create(anonymous = TRUE, region = region, connect_timeout = timeout, request_timeout = timeout)
  ds <- arrow::open_dataset(paths, filesystem = fs, format = "parquet")
  xmin <- bb[["xmin"]]; xmax <- bb[["xmax"]]; ymin <- bb[["ymin"]]; ymax <- bb[["ymax"]]
  q <- dplyr::filter(ds, bbox$xmin < xmax, bbox$xmax > xmin, bbox$ymin < ymax, bbox$ymax > ymin)
  if (!is.null(subtype)) q <- dplyr::filter(q, subtype == !!subtype)
  cols <- intersect(c("id", "subtype", "class", "geometry"), names(ds))
  x <- as.data.frame(dplyr::collect(dplyr::select(q, dplyr::all_of(cols))))
  if (!nrow(x)) return(empty)
  geom <- sf::st_as_sfc(structure(lapply(x$geometry, as.raw), class = "WKB"), EWKB = FALSE, crs = 4326)
  x$geometry <- NULL
  sf::st_sf(x, geometry = geom)
}
