#' Download one map fragment for a specific year
#'
#' Reads the cached tile manifest for a fragment, selects the requested year,
#' and downloads one of its available links. When the manifest contains more
#' than one valid link for the year, one entry is selected at random.
#'
#' @param fragment Fragment identifier.
#' @param type Data type used by the manifest cache.
#' @param collection Collection used by the manifest cache.
#' @param year Year to select from the manifest.
#' @param export_folder_path Directory where the raster file will be stored.
#' @return Invisibly returns the downloaded or existing local file path.
#' @export
#'
#' @examples
#' \dontrun{
#' download_maps(
#'   fragment = 1, type = "cover", collection = "7", year = 1985,
#'   export_folder_path = "maps"
#' )
#' }
download_maps <- function(
    fragment,
    type = "cover",
    collection = "7",
    year,
    export_folder_path) {

  if (missing(year) || length(year) != 1L || is.na(year)) {
    stop("'year' must be one non-missing value.", call. = FALSE)
  }
  if (missing(export_folder_path) || length(export_folder_path) != 1L ||
      is.na(export_folder_path) || !nzchar(export_folder_path)) {
    stop("'export_folder_path' must be provided.", call. = FALSE)
  }

  manifest_json <- fetch_tile_manifest(
    type = type,
    collection = collection,
    fragment = fragment,
    use_cache = TRUE
  )
  response <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
  manifest <- response$result
  year_records <- manifest$years[[as.character(year)]]

  if (is.null(year_records) || length(year_records) == 0L) {
    stop(
      "No download is available for fragment ", fragment,
      " in year ", year, ".",
      call. = FALSE
    )
  }

  valid_record <- vapply(
    year_records,
    function(record) {
      is.list(record) &&
        length(record$shareable_link) == 1L &&
        !is.na(record$shareable_link) &&
        nzchar(record$shareable_link) &&
        length(record$file_name) == 1L &&
        !is.na(record$file_name) &&
        nzchar(record$file_name)
    },
    logical(1L)
  )
  year_records <- year_records[valid_record]

  if (length(year_records) == 0L) {
    stop(
      "The manifest has no valid link and file name for fragment ",
      fragment, " in year ", year, ".",
      call. = FALSE
    )
  }

  selected <- year_records[[sample.int(length(year_records), size = 1L)]]
  shared_link <- selected$shareable_link
  file_name <- selected$file_name

  if (!identical(basename(file_name), file_name)) {
    stop("The manifest returned an unsafe file name.", call. = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    dir.create(export_folder_path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    stop("Could not create the export directory.", call. = FALSE)
  }

  local_path <- file.path(export_folder_path, file_name)
  local_size <- if (file.exists(local_path)) file.info(local_path)$size else NA_real_
  if (!is.na(local_size) && local_size > 0) {
    message("The map fragment is already available: ", local_path)
    return(invisible(local_path))
  }

  message("Downloading map fragment to: ", local_path)
  download_public_gdrive_file(
    file_link = shared_link,
    local_path = local_path
  )

  downloaded_size <- if (file.exists(local_path)) {
    file.info(local_path)$size
  } else {
    NA_real_
  }
  if (is.na(downloaded_size) || downloaded_size <= 0) {
    stop("The map fragment download did not produce a valid file.", call. = FALSE)
  }

  invisible(local_path)
}
