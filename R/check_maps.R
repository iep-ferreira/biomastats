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
#'   export_folder_path = "maps", type = "cover", collection = "7"
#' )
#' }
check_maps <- function(
    ids,
    start,
    end,
    export_folder_path,
    type = "cover",
    collection = "7") {

  if (length(ids) == 0L) {
    stop("'ids' must contain at least one fragment.", call. = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    dir.create(export_folder_path, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(export_folder_path)) {
    stop("Could not create the export directory.", call. = FALSE)
  }

  maps <- vector("list", length = end - start + 1L)

  for (year in start:end) {
    current_fig <- NULL

    for (id in ids) {
      local_path <- download_maps(
        fragment = id,
        type = type,
        collection = collection,
        year = year,
        export_folder_path = export_folder_path
      )

      fragment_raster <- raster::raster(local_path)
      current_fig <- if (is.null(current_fig)) {
        fragment_raster
      } else {
        raster::merge(current_fig, fragment_raster)
      }
    }

    maps[[year - start + 1L]] <- current_fig
  }

  maps
}
