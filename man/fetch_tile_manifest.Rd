% Generated manually from R/fetch_tile_manifest.R
\name{fetch_tile_manifest}
\alias{fetch_tile_manifest}
\title{Fetch a tile manifest from the Biomastats API}
\usage{
fetch_tile_manifest(
  type = "cover",
  collection = "7",
  fragment,
  api_urls = NULL,
  discovery_url = NULL,
  timeout = 30,
  allow_http = FALSE
)
}
\arguments{
\item{type}{Reserved data type; currently ignored.}
\item{collection}{Reserved collection identifier; currently ignored.}
\item{fragment}{Character or numeric fragment identifier.}
\item{api_urls}{Optional API endpoint URLs.}
\item{discovery_url}{Optional HTTPS URL for a JSON discovery document.}
\item{timeout}{Seconds allowed for each HTTP request.}
\item{allow_http}{Allow HTTP endpoints for local development.}
}
\value{A parsed object containing the tile manifest, normally the API response's `result` field.}
\description{
Sends the fragment identifier to the Biomastats API. If configured endpoints
fail with connection or retryable HTTP errors, HTTPS aliases from a discovery
document are tried.
}
\examples{
\dontrun{
manifest <- fetch_tile_manifest(type = "cover", collection = "7", fragment = 1)
}
}
