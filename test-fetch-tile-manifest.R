#!/usr/bin/env Rscript

# Quick live test for fetch_tile_manifest().
# This script requires internet access and is not part of the automated tests.

devtools::load_all(quiet = TRUE)

manifest_json <- biomastats::fetch_tile_manifest(
  type = "cover",
  collection = "7",
  fragment = 50,
  use_cache = FALSE,
  timeout = 30
)

stopifnot(
  is.character(manifest_json),
  length(manifest_json) == 1L,
  jsonlite::validate(manifest_json)
)

response <- jsonlite::fromJSON(manifest_json, simplifyVector = FALSE)
manifest <- response$result

cat("fetch_tile_manifest() succeeded\n")
cat("fragment:", manifest$fragment, "\n")
cat("version:", manifest$version, "\n")
cat("years:", manifest$first_year, "-", manifest$last_year, "\n")
cat("links:", manifest$links_count, "\n")
cat("manifest hash:", manifest$manifest_hash, "\n")


# ---- Manual download_maps() test -------------------------------------------

# Set these three values manually, then run this section in RStudio.
download_folder <- "C:/Users/Pichau/Documents/teste_download_temp"
download_fragment <- 500
download_year <- 1985

stopifnot(
  !is.na(download_folder),
  nzchar(download_folder)
)

downloaded_file <- biomastats::download_maps(
  fragment = download_fragment,
  type = "cover",
  collection = "7",
  year = download_year,
  export_folder_path = download_folder
)


make_polygon(lat = -20.22, lon = -49.60, size = 5, shape = "hexagon")


teste <- load_rasters(start = 2015, end = 2020, method = "download", export_folder_path = download_folder, type = "cover", collection = "7")

land_vis(teste, year = 2020)

### Agora com o recorte maior e menos anos

make_polygon(lat = -20.22, lon = -49.60, size = 50, shape = "hexagon")

teste <- load_rasters(start = 1985, end = 1990, method = "download", export_folder_path = download_folder, type = "cover", collection = "7")

land_vis(teste, year = 1990)

res<-get_area(teste)

land_dist(res, year = 1990)







