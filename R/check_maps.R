#' Check required maps and construct the raster list
#'
#' @param ids Vector of map fragment identifiers.
#' @param start Starting year.
#' @param end Ending year.
#' @param export_folder_path Directory used to store downloaded raster files.
#' @param type Data type passed to `download_maps()`.
#' @param collection Collection passed to `download_maps()`.
#' @return A list containing one merged raster for each year.
#' @importFrom raster raster merge
#' @export
#'
#' @examples
#' \dontrun{
#' check_maps(
#'   ids = c(1, 2), start = 1990, end = 2000,
#'   export_folder_path = "maps", type = "cover", collection = "10"
#' )
#' }
check_maps <- function(
    ids,
    start,
    end,
    export_folder_path,
    type = "cover",
    collection = "10") {

  if (length(ids) == 0L) {
    stop("'ids' must contain at least one fragment.", call. = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    dir.create(export_folder_path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    stop("Could not create the export directory.", call. = FALSE)
  }

  years <- seq.int(start, end)
  requirements <- expand.grid(
    fragment = ids,
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  requirements$local_path <- mapply(
    .find_local_map_file,
    fragment = requirements$fragment,
    year = requirements$year,
    MoreArgs = list(
      export_folder_path = export_folder_path,
      type = type,
      collection = collection
    ),
    USE.NAMES = FALSE
  )

  missing <- is.na(requirements$local_path)
  missing_fragments <- unique(requirements$fragment[missing])

  for (fragment in missing_fragments) {
    rows <- which(missing & requirements$fragment == fragment)
    requirements$local_path[rows] <- download_maps(
      fragment = fragment,
      type = type,
      collection = collection,
      year = requirements$year[rows],
      export_folder_path = export_folder_path
    )
  }

  maps <- vector("list", length = length(years))

  for (index in seq_along(years)) {
    local_paths <- requirements$local_path[requirements$year == years[[index]]]
    fragment_rasters <- lapply(local_paths, raster::raster)
    maps[[index]] <- Reduce(raster::merge, fragment_rasters)
  }

  maps
}
