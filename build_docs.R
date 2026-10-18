#!/usr/bin/env Rscript
# Rebuild docs/ (GitHub Pages) from the bookdown books in tutorial/ (Portuguese)
# and tutorial/en/ (English).
#
# Usage (from the package root):
#   Rscript build_docs.R
#
# Requires bookdown and downlit. The book is rendered with the current package
# source, installed in a temporary library, independently of any biomastats
# version installed in the user's library. Figures of external layers are
# generated only when missing from tutorial/imagens/ (requires internet once).

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
if (.Platform$OS.type == "windows") invisible(Sys.setlocale("LC_CTYPE", ".UTF-8"))

for (pkg in c("bookdown", "downlit")) {
  if (!requireNamespace(pkg, quietly = TRUE)) stop("Install the '", pkg, "' package first.")
}

lib <- file.path(tempdir(), "biomastats-book-lib")
dir.create(lib, showWarnings = FALSE)
message("Installing biomastats from ", root, " ...")
status <- system2(file.path(R.home("bin"), "R"),
                  c("CMD", "INSTALL", shQuote(root), paste0("--library=", shQuote(lib)),
                    "--no-multiarch", "--no-test-load"), stdout = FALSE, stderr = FALSE)
if (status != 0) stop("R CMD INSTALL failed.")
.libPaths(c(lib, .libPaths()))

# Portuguese edition -> docs/; English edition -> docs/en/ (rendered second).
setwd(file.path(root, "tutorial"))
bookdown::render_book("index.Rmd", quiet = TRUE)
setwd(file.path(root, "tutorial", "en"))
bookdown::render_book("index.Rmd", quiet = TRUE)

# GitHub Pages: serve files as they are (no Jekyll processing).
file.create(file.path(root, "docs", ".nojekyll"))
message("Done. Preview: docs/index.html")
