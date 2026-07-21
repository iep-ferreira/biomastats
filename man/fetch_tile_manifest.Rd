% Generated manually from R/fetch_tile_manifest.R
\name{fetch_tile_manifest}
\alias{fetch_tile_manifest}
\title{Fetch a tile manifest from the Biomastats API}
\usage{
fetch_tile_manifest(
  type = "cover",
  collection = "10",
  fragment,
  use_cache = TRUE,
  api_urls = NULL,
  discovery_url = NULL,
  timeout = 30,
  allow_http = FALSE
)
}
\arguments{
\item{type}{Data type used to identify the cache entry.}
\item{collection}{Collection identifier sent to the API and used to identify the cache entry.}
\item{fragment}{Character or numeric fragment identifier.}
\item{use_cache}{If TRUE, return a valid cached manifest before contacting the API.}
\item{api_urls}{Optional API endpoint URLs.}
\item{discovery_url}{Optional HTTPS URL for a JSON discovery document.}
\item{timeout}{Seconds allowed for each HTTP request.}
\item{allow_http}{Allow HTTP endpoints for local development.}
}
\value{A length-one character vector containing the unchanged JSON response.}
\description{
Fetches a tile manifest as unchanged JSON. Valid cache entries are reused for
seven days. If configured endpoints fail with connection or retryable HTTP
errors, HTTPS aliases from a discovery document are tried.
}
\examples{
\dontrun{
manifest_json <- fetch_tile_manifest(type = "cover", collection = "10", fragment = 1)
}
}
