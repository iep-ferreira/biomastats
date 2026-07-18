#' Fetch a tile manifest from the Biomastats API
#'
#' Sends the fragment identifier to a Biomastats API endpoint and returns the
#' JSON response parsed as an R object. The `type` and `collection` arguments
#' are reserved for the versioned API contract and are intentionally ignored
#' until those parameters are implemented by the service.
#'
#' The function tries the configured endpoints first. If all of them fail with
#' a connection or retryable HTTP error, it can read a small HTTPS discovery
#' document and try the returned aliases. Discovery is never performed when
#' the initial request succeeds.
#'
#' @param type Character. Reserved data type; currently ignored. Defaults to
#'   `"cover"`.
#' @param collection Character or numeric. Reserved collection identifier;
#'   currently ignored. Defaults to `"7"`.
#' @param fragment Character or numeric fragment identifier.
#' @param api_urls Optional character vector of API endpoint URLs. If omitted,
#'   the `biomastats.api_urls` option is used, followed by the built-in URLs.
#' @param discovery_url Optional HTTPS URL for a JSON discovery document. If
#'   omitted, the `biomastats.api_discovery_url` option is used, followed by
#'   the package's public discovery document.
#' @param timeout Number of seconds allowed for each HTTP request.
#' @param allow_http Logical; allow HTTP endpoints for local development.
#'   HTTPS is required by default.
#' @return A parsed JSON object, usually a list containing the tile manifest.
#' @export
#'
#' @examples
#' \dontrun{
#' manifest <- fetch_tile_manifest(
#'   type = "cover", collection = "7", fragment = 1
#' )
#' }
fetch_tile_manifest <- function(
    type = "cover",
    collection = "7",
    fragment,
    api_urls = NULL,
    discovery_url = NULL,
    timeout = 30,
    allow_http = FALSE) {

  if (missing(fragment) || length(fragment) != 1L || is.na(fragment)) {
    stop("'fragment' must be one non-missing value.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("'timeout' must be one positive number of seconds.", call. = FALSE)
  }

  # Reserved until the API implements them. Keeping these arguments now makes
  # the public function compatible with the planned endpoint contract.
  invisible(type)
  invisible(collection)

  default_urls <- c(
    "https://api.biomastats.org/api/fragment",
    "https://biomastats-worker.ferreira-iep.workers.dev/api"
  )
  configured_urls <- getOption("biomastats.api_urls", character())
  urls <- if (is.null(api_urls)) {
    c(configured_urls, default_urls)
  } else {
    api_urls
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  valid_url <- function(url, allow_http = FALSE) {
    if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
      return(FALSE)
    }
    scheme_ok <- grepl("^https://", url, ignore.case = TRUE) ||
      (allow_http && grepl("^http://", url, ignore.case = TRUE))
    if (!scheme_ok) return(FALSE)
    parsed <- tryCatch(httr::parse_url(url), error = function(e) NULL)
    !is.null(parsed) && nzchar(parsed$hostname %||% "") &&
      is.null(parsed$username) && is.null(parsed$password)
  }

  urls <- unique(as.character(urls))
  urls <- urls[vapply(urls, valid_url, logical(1L), allow_http = allow_http)]
  if (length(urls) == 0L) {
    stop(
      "No valid API URL is configured. Use HTTPS or set allow_http = TRUE for local development.",
      call. = FALSE
    )
  }

  discovery <- discovery_url
  if (is.null(discovery)) {
    discovery <- getOption("biomastats.api_discovery_url", NA_character_)
  }
  if (length(discovery) == 0L || is.na(discovery[[1L]]) ||
      !nzchar(discovery[[1L]])) {
    discovery <- "https://github.com/iep-ferreira/biomastats/tree/main/inst/api-servers.json"
  }
  if (!valid_url(discovery, allow_http = FALSE)) {
    discovery <- NA_character_
  }

  request <- function(url) {
    response <- tryCatch(
      httr::POST(
        url,
        httr::add_headers(
          Accept = "application/json",
          `Content-Type` = "application/x-www-form-urlencoded"
        ),
        body = list(fragment = fragment),
        encode = "form",
        httr::timeout(timeout)
      ),
      error = function(error) error
    )

    if (inherits(response, "error")) {
      return(list(ok = FALSE, retryable = TRUE, message = conditionMessage(response)))
    }

    status <- httr::status_code(response)
    if (status < 200L || status >= 300L) {
      retryable <- status %in% c(404L, 408L, 425L, 429L) || status >= 500L
      return(list(
        ok = FALSE,
        retryable = retryable,
        message = paste0("HTTP ", status)
      ))
    }

    text <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed <- tryCatch(
      jsonlite::fromJSON(text, simplifyVector = FALSE),
      error = function(error) error
    )
    if (inherits(parsed, "error")) {
      return(list(
        ok = FALSE,
        retryable = FALSE,
        message = paste0("response is not valid JSON: ", conditionMessage(parsed))
      ))
    }

    if (is.list(parsed) && "error" %in% names(parsed) &&
        length(parsed$error) == 1L && nzchar(as.character(parsed$error))) {
      return(list(
        ok = FALSE,
        retryable = FALSE,
        message = as.character(parsed$error)
      ))
    }

    value <- if (is.list(parsed) && "result" %in% names(parsed)) {
      parsed$result
    } else {
      parsed
    }

    list(ok = TRUE, value = value)
  }

  tried <- character()
  for (url in urls) {
    tried <- c(tried, url)
    result <- request(url)
    if (isTRUE(result$ok)) return(result$value)
    if (!isTRUE(result$retryable)) {
      stop(
        "The API rejected the request at ", url, " (", result$message, ").",
        call. = FALSE
      )
    }
  }

  discovery_response <- if (is.na(discovery)) {
    NULL
  } else {
    tryCatch(
      httr::GET(discovery, httr::timeout(timeout)),
      error = function(error) error
    )
  }
  if (!is.null(discovery_response) &&
      !inherits(discovery_response, "error") &&
      httr::status_code(discovery_response) >= 200L &&
      httr::status_code(discovery_response) < 300L) {
    discovery_text <- httr::content(
      discovery_response,
      as = "text",
      encoding = "UTF-8"
    )
    if (length(discovery_text) != 1L ||
        nchar(discovery_text, type = "bytes") > 100000L) {
      discovery_text <- ""
    }
    discovery_data <- tryCatch(
      jsonlite::fromJSON(discovery_text, simplifyVector = FALSE),
      error = function(error) NULL
    )

    aliases <- character()
    if (is.list(discovery_data)) {
      schema_ok <- is.null(discovery_data$schema_version) ||
        identical(as.integer(discovery_data$schema_version), 1L)
      if (schema_ok) {
        aliases <- c(
          discovery_data$base_urls %||% character(),
          discovery_data$urls %||% character()
        )
        servers <- discovery_data$servers %||% list()
        if (is.list(servers)) {
          aliases <- c(
            aliases,
            vapply(
              servers,
              function(server) {
                if (is.character(server) && length(server) == 1L) return(server)
                if (is.list(server)) return(server$url %||% "")
                ""
              },
              character(1L)
            )
          )
        }
        api_versions <- discovery_data$api_versions %||% list()
        if (is.list(api_versions)) {
          version_aliases <- if (!is.null(api_versions[["v1"]])) {
            api_versions[["v1"]]
          } else {
            unlist(api_versions, use.names = FALSE)
          }
          aliases <- c(
            aliases,
            version_aliases
          )
        }
      }
    }

    aliases <- unique(as.character(aliases))
    aliases <- aliases[
      vapply(aliases, valid_url, logical(1L), allow_http = allow_http)
    ]
    aliases <- aliases[!aliases %in% tried]

    for (url in aliases) {
      tried <- c(tried, url)
      result <- request(url)
      if (isTRUE(result$ok)) return(result$value)
    }
  }

  stop(
    "Could not fetch the manifest for fragment ", fragment,
    ". Servers attempted: ", paste(tried, collapse = ", "),
    ". The API or discovery document may be unavailable.",
    call. = FALSE
  )
}
