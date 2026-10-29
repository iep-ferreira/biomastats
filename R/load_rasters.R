#' Load Rasters from Shapefile and Raster Data
#'
#' This function loads raster data from either an online source, a local folder, or a library.
#' It then crops and masks the raster data using a provided shapefile.
#'
#' @param shape_path The study area: an `sf`/`sfc` polygon or a vector file
#'   (GeoPackage, shapefile or GeoJSON), with or without extension; with the same
#'   name, `.gpkg` is preferred to `.shp`. A shapefile needs its `.prj`.
#' @param start A numeric value indicating the start year for raster data.
#' @param end A numeric value indicating the end year for raster data.
#' @param method Raster source: `"download"` (MapBiomas tiles from the
#'   Biomastats API), `"library"` (national maps in a local folder) or `"http"`
#'   (selective reading of the area's window from public GeoTIFFs; see
#'   `product` and `url`).
#' @param export_folder_path Directory where downloaded raster files are stored.
#'   With `method = "http"` (optional), the clipped windows are written there as
#'   `<product>_<year>.tif`, with the area as `area.gpkg`, ready for
#'   [load_local_rasters()].
#' @param import_folder_path Directory containing an existing raster library.
#' @param type Data type requested from the Biomastats API. Default is `"cover"`.
#' @param collection Collection version. Default is `"10"`. With
#'   `method = "http"`, the default follows the catalogue: the MapBiomas legend
#'   for Brazilian products, the product id otherwise.
#' @param context_m Width, in metres, of a buffer added around the study area
#'   (default 0). With a positive value, map tiles, crop and mask cover the
#'   study area plus the buffer (the analysis context), and the original area is
#'   kept as `focal`. See [connectivity_context()].
#' @param product With `method = "http"`: a product id of [raster_catalog()]
#'   (for example `"mapbiomas_brasil_c10"` or `"mapbiomas_brasil_10m_c4"`).
#' @param url With `method = "http"`: instead of `product`, a URL (or GDAL path,
#'   such as `/vsicurl/...` or a local file) with `{year}` in place of the year.
#' @param dictionary,nodata With `method = "http"`: class dictionary and no-data
#'   codes, as in [load_local_rasters()].
#' @param timeout With `method = "http"`: seconds to wait for each HTTP response
#'   (default 30).
#'
#' @return A list containing the shapefile (`shape`, the study area or its
#'   context), time range, masked raster data and collection. With
#'   `context_m > 0`, `focal` holds the original study area. `source` records
#'   how the maps were obtained (loader, method, folders, type, collection and
#'   `context_m`), so that [connectivity_context()] can reload a larger area.
#' @details The buffer is computed in the local UTM zone of the study area and
#'   returned in WGS84 (EPSG:4326), like the shape. Downstream,
#'   [biomastats_metrics()] and [reclass_to_polygons()] carry `focal` along with
#'   `boundary`, and [land_connectivity()] uses it to measure connectivity of the
#'   focal habitat with the context as stepping stones.
#' @importFrom sf st_read st_crs
#' @importFrom raster crop mask crs
#' @export
#'
#' @examples
#' \dontrun{
#' load_rasters("path/to/shapefile.shp", start = 1985, end = 2020, method = "download", export_folder_path = "./maps-personal-library/")
#' # Study area plus a 2 km context
#' load_rasters(area, start = 2024, end = 2024, method = "download",
#'              export_folder_path = "./maps-personal-library/", context_m = 2000)
#' # Selective reading over HTTP (window of the area only), saved for reuse
#' raster_catalog(initiative = "brasil")
#' load_rasters("itapetininga.gpkg", start = 2023, end = 2024, method = "http",
#'              product = "mapbiomas_brasil_c10", export_folder_path = "mapas_itapetininga")
#' }

load_rasters <- function(shape_path = NULL,
                         start = 1985, end = 2020,
                         method = c("download", "library", "http"),
                         export_folder_path = NULL,
                         import_folder_path = NULL,
                         type = "cover",
                         collection = "10",
                         context_m = 0,
                         product = NULL,
                         url = NULL,
                         dictionary = NULL,
                         nodata = 0,
                         timeout = NULL
) { # starts function

  method <- match.arg(method)
  http_collection <- if (missing(collection)) NULL else collection
  if (!is.numeric(context_m) || length(context_m) != 1L || !is.finite(context_m) || context_m < 0) {
    stop("context_m deve ser uma distancia nao negativa em metros.")
  }

  sys_path <- system.file(package = "biomastats")

  if(is.null(shape_path)){
    shape_path <- file.path(sys_path, "shp/polygon.shp")
  }

  # Load the study area (a shapefile path or an sf/sfc polygon)
  s <- .read_study_area(shape_path)
  focal <- NULL
  if (context_m > 0) {
    focal <- s
    s <- .context_buffer(s, context_m)
  }

  if (method == "http") {
    return(.load_rasters_http(s, focal, start, end, product, url, http_collection, dictionary,
                              nodata, timeout, export_folder_path, context_m))
  }

  s_utm <- sf::st_crs(s)$proj4string

  # Identify the fragments required to recompose the study area (and context).
  ids <- unique(compare_shapefiles(s))

  if (method == "download") {
    if (is.null(export_folder_path)) {
      stop("Error! Path for the export maps library is undefined.")
    }
    if (length(ids) == 0L) {
      stop("No map fragments intersect the study area.", call. = FALSE)
    }
    biome_rasters <- check_maps(
      ids = ids,
      start = start,
      end = end,
      export_folder_path = export_folder_path,
      type = type,
      collection = collection
    )
  } else if (method == "library") {
    if (is.null(import_folder_path)) {
      stop("Error! Path for the import maps library is undefined.")
    }
    biome_rasters <- load_rasters_from_folder(start, end, import_folder_path)
  }

    biome_utm <- sf::st_crs(biome_rasters[[1]])$proj4string

  # Check if the CRSs of shapefile and rasters are identical
  if (biome_utm != s_utm) {
    stop("Error. CRSs differed from shp and rasters. CRS must be identical!")
  }

  maps <- NULL
  map <- NULL

  # Crop and mask raster data using the shapefile
  for (i in start:end) {
    maps[[i - (start - 1)]] <- raster::crop(biome_rasters[[i - (start - 1)]], s)
    map[[i - (start - 1)]] <- raster::mask(maps[[i - (start - 1)]], s)
  }

  source <- list(loader = "load_rasters", method = method, export_folder_path = export_folder_path,
                 import_folder_path = import_folder_path, type = type,
                 collection = collection, context_m = context_m)
  out <- list("shape" = s, "time_range" = c(start, end), "raster" = map, "collection" = collection)
  if (!is.null(focal)) out$focal <- focal
  out$source <- source
  return(out)
}

# method = "http": one GDAL path per year from a catalogue product or a URL
# template, read by the shared core of load_local_rasters() (window crop,
# harmonization and mask); the windows can be written for later reuse.
.load_rasters_http <- function(area, focal, start, end, product, url, collection, dictionary,
                               nodata, timeout, export_folder_path, context_m) {
  if (is.null(product) == is.null(url)) stop("Com method = \"http\", informe product ou url.")
  years <- .local_years(seq.int(start, end))
  if (!is.null(product)) {
    spec <- raster_catalog(product)
    template <- spec$url
    if (any(years < spec$first_year | years > spec$last_year)) {
      stop(product, " cobre ", spec$first_year, "-", spec$last_year, ".")
    }
    prefix <- product
    if (is.null(collection)) collection <- if (is.na(spec$dictionary)) product else spec$dictionary
  } else {
    if (!is.character(url) || length(url) != 1L || !grepl("{year}", url, fixed = TRUE)) {
      stop("url deve ser um texto com {year} no lugar do ano.")
    }
    template <- url
    prefix <- sub("[._-]+$", "", sub("\\{year\\}.*$", "", basename(url)))
    if (!nzchar(prefix)) prefix <- "http"
  }
  timeout <- if (is.null(timeout)) getOption("biomastats.http_timeout", 30) else timeout
  timeout <- .layer_timeout(timeout)
  paths <- vapply(years, function(y) .vsi(gsub("{year}", y, template, fixed = TRUE)), "")
  core <- .with_gdal_network(timeout, tryCatch(
    .load_raster_paths(paths, years, area, prefix, collection, dictionary, nodata, NULL, NULL,
                      required = if (is.null(focal)) area else focal),
    error = function(e) {
      msg <- conditionMessage(e)
      if (grepl("open|HTTP|curl|does not exist|not recognized", msg, ignore.case = TRUE)) {
        stop("Nao foi possivel ler ", sub("^/vsicurl/", "", paths[1]), " (", msg, ").", call. = FALSE)
      }
      stop(e)
    }))
  maps <- core$maps
  maps$metadata <- c(list(source = "http", product = product, url = template), maps$metadata)
  if (!is.null(export_folder_path)) {
    maps$metadata$exported <- .export_windows(maps, prefix, if (is.null(focal)) area else focal,
                                              export_folder_path)
  }
  .local_finish(maps, focal, list(loader = "load_rasters", method = "http", product = product,
    url = url, collection = maps$collection, dictionary = maps$class_dictionary, nodata = nodata,
    timeout = timeout, context_m = context_m))
}

# Writes each year's window as <prefix>_<year>.tif and the area as area.gpkg,
# the layout read by load_local_rasters().
.export_windows <- function(maps, prefix, area, folder) {
  if (!dir.exists(folder) && !dir.create(folder, recursive = TRUE)) {
    stop("Nao foi possivel criar export_folder_path.")
  }
  years <- seq.int(maps$time_range[1], maps$time_range[2])
  files <- file.path(folder, sprintf("%s_%d.tif", prefix, years))
  for (i in seq_along(years)) {
    r <- terra::rast(maps$raster[[i]])
    big <- max(terra::values(r, mat = FALSE), na.rm = TRUE) > 254
    terra::writeRaster(r, files[i], overwrite = TRUE, datatype = if (big) "INT2U" else "INT1U",
                       NAflag = if (big) 65535 else 255, gdal = c("COMPRESS=LZW", "TILED=YES"))
  }
  gpkg <- file.path(folder, "area.gpkg")
  if (file.exists(gpkg)) unlink(gpkg)
  sf::st_write(sf::st_sf(geometry = sf::st_geometry(area)), gpkg, quiet = TRUE)
  c(files, gpkg)
}

# Study area as sf in WGS84, from an sf/sfc polygon or a vector file (see
# .study_area_file()).
.read_study_area <- function(x) {
  if (inherits(x, "sfc")) x <- sf::st_sf(geometry = x)
  s <- if (inherits(x, "sf")) x else sf::st_read(.study_area_file(x), quiet = TRUE)
  if (is.na(sf::st_crs(s))) stop("A area de estudo precisa de CRS.")
  if (!all(as.character(sf::st_geometry_type(s)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    stop("A area de estudo deve ser poligonal.")
  }
  sf::st_transform(s, crs = 4326)
}

# Vector file of the study area, detected from a path with or without
# extension, or from a folder holding a single area file. GeoPackage, shapefile
# and GeoJSON are recognized; with both .gpkg and .shp of the same name the
# GeoPackage is used. A shapefile must keep its .prj (the CRS).
.study_area_file <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    stop("A area de estudo deve ser um objeto sf/sfc ou o caminho de um arquivo vetorial.")
  }
  formats <- c("gpkg", "shp", "geojson", "json")
  if (dir.exists(path)) {
    found <- list.files(path, pattern = "[.](gpkg|shp|geojson|json)$", ignore.case = TRUE,
                        full.names = TRUE)
    stems <- unique(tools::file_path_sans_ext(found))
    if (length(stems) != 1L) {
      stop(if (!length(stems)) "Nenhum arquivo de area (.gpkg, .shp, .geojson) em " else
             "Varios arquivos de area em ", path, if (length(stems)) paste0(": ",
             paste(basename(found), collapse = ", "), "; informe o arquivo."))
    }
    path <- stems
  }
  if (!file.exists(path) || dir.exists(path)) {
    candidates <- paste0(tools::file_path_sans_ext(path), ".", formats)
    candidates <- candidates[file.exists(candidates)]
    if (!length(candidates)) stop("Arquivo de area nao encontrado: ", path)
    path <- candidates[1]
  }
  if (tolower(tools::file_ext(path)) == "shp") {
    stem <- tools::file_path_sans_ext(path)
    sidecars <- list.files(dirname(path), full.names = FALSE)
    if (!tolower(paste0(basename(stem), ".prj")) %in% tolower(sidecars)) {
      stop("Shapefile sem o arquivo .prj (", basename(stem), ".prj): o sistema de coordenadas ",
           "se perdeu. Copie o .prj junto com o .shp ou use um GeoPackage (.gpkg).")
    }
  }
  path
}

# Area within `distance` metres of the study area, buffered in its local UTM
# zone (or in the given projected CRS) and returned in the CRS of `shape`.
.context_buffer <- function(shape, distance, crs = NULL) {
  g <- sf::st_union(sf::st_geometry(shape))
  if (distance <= 0) return(sf::st_sf(geometry = g))
  metric <- .connectivity_crs(sf::st_sf(geometry = g), crs)
  buffered <- sf::st_buffer(sf::st_transform(g, metric), distance, nQuadSegs = 30)
  sf::st_sf(geometry = sf::st_transform(buffered, sf::st_crs(shape)))
}
