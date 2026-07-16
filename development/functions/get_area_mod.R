# Experimental replacement for biomastats::get_area().
#
# This file is intentionally outside R/ so it is not part of the package
# namespace or release tarball.  The test loads it and temporarily binds it
# to the original get_area() name.
get_area_mod <- function(data, ...) {
  result <- get_area_original(data, ...)

  # Add the experimental implementation here.  Keeping this delegation while
  # developing makes the test runnable before the first change is introduced.
  result
}
