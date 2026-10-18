# Keep package cache writes inside the R session temporary directory during
# automated tests. CRAN checks must not modify the user's real cache.
Sys.setenv(
  R_USER_CACHE_DIR = file.path(tempdir(), "biomastats-test-user-cache")
)
