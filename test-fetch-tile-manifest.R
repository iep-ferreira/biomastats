#!/usr/bin/env Rscript

# Quick live test for fetch_tile_manifest().
# This script requires internet access and is not part of the automated tests.

devtools::load_all(quiet = TRUE)

manifest <- biomastats::fetch_tile_manifest(
  type = "cover",
  collection = "7",
  fragment = 50,
  timeout = 30
)

manifest



cat("fetch_tile_manifest() succeeded\n")
cat("fragment:", manifest$fragment, "\n")
cat("version:", manifest$version, "\n")
cat("years:", manifest$first_year, "-", manifest$last_year, "\n")
cat("links:", manifest$links_count, "\n")
cat("manifest hash:", manifest$manifest_hash, "\n")
