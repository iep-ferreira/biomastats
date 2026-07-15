#!/usr/bin/env Rscript
# Rebuild docs/ (GitHub Pages) from tutorial.Rmd.
#
# Usage (from the package root):
#   Rscript build_docs.R

# Prefer the directory that contains this script when Rscript was invoked
# with a path; otherwise use the current working directory.
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
root <- if (length(file_arg)) {
  normalizePath(dirname(sub("^--file=", "", file_arg[1])), winslash = "/", mustWork = TRUE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
setwd(root)

message("Working directory: ", root)

css_src <- file.path("tutorial_includes", "site_theme.css")
if (!file.exists(css_src)) {
  stop(
    "Missing ", css_src, ". ",
    "This file is required by tutorial_includes/lang_header.html ",
    "and is what defines the tutorial visual identity."
  )
}

message("Rendering tutorial.Rmd → tutorial.html ...")
rmarkdown::render(
  input = "tutorial.Rmd",
  output_file = "tutorial.html",
  output_dir = root,
  quiet = FALSE,
  envir = new.env(parent = globalenv())
)

if (!file.exists("tutorial.html")) {
  stop("Render finished but tutorial.html was not created.")
}

# Preserve artifacts in docs/ that this script does not regenerate.
preserve <- character()
pdf_src <- file.path("docs", "tutorial.pdf")
if (file.exists(pdf_src)) {
  tmp_pdf <- tempfile(fileext = ".pdf")
  file.copy(pdf_src, tmp_pdf, overwrite = TRUE)
  preserve["tutorial.pdf"] <- tmp_pdf
}

message("Recreating docs/ ...")
if (dir.exists("docs")) {
  unlink("docs", recursive = TRUE)
}
dir.create("docs", recursive = TRUE)
dir.create(file.path("docs", "tutorial_includes"), recursive = TRUE)

ok <- file.copy("tutorial.html", file.path("docs", "index.html"), overwrite = TRUE)
includes <- list.files("tutorial_includes", full.names = TRUE)
ok <- ok && all(file.copy(includes, file.path("docs", "tutorial_includes"), overwrite = TRUE))
if (!isTRUE(ok)) stop("Failed to copy one or more files into docs/.")

for (name in names(preserve)) {
  if (!file.copy(preserve[[name]], file.path("docs", name), overwrite = TRUE)) {
    stop("Failed to restore preserved file: ", name)
  }
  unlink(preserve[[name]])
}

same_html <- tools::md5sum("tutorial.html") == tools::md5sum(file.path("docs", "index.html"))
message("docs/index.html == tutorial.html: ", same_html)
message("Done. Preview: docs/index.html")
