# Keep package cache writes inside the R session temporary directory during
# automated tests. CRAN checks must not modify the user's real cache.
Sys.setenv(
  R_USER_CACHE_DIR = file.path(tempdir(), "biomastats-test-user-cache")
)

# Network-facing OSM tests opt into cache explicitly when exercising it.
options(
  biomastats.osm_use_cache = FALSE,
  biomastats.osm_cache_dir = file.path(tempdir(), "biomastats-test-osm-cache"),
  biomastats.check_updates = FALSE
)
