#' Fetch a tile manifest from the Biomastats API
#'
#' Fetches a tile manifest as an unchanged JSON string. By default, a valid
#' cached response is returned before any network request is made. Cached
#' manifests are keyed by API-contract version, `type`, `collection`, and
#' `fragment`, and expire
#' after seven days.
#'
#' If the configured API endpoints fail with a connection or retryable HTTP
#' error, the function can read a small HTTPS discovery document and try its
#' aliases. Discovery is never performed when an endpoint succeeds.
#'
#' The current API contract requires `collection` and `fragment` in every
#' request. `type` remains a client-side cache discriminator.
#'
#' @param type Character data type. Defaults to `"cover"`.
#' @param collection Character or numeric collection identifier. Defaults to
#'   `"10"`.
#' @param fragment Character or numeric fragment identifier.
#' @param use_cache Logical. If `TRUE`, return a valid cached manifest before
#'   contacting the API. If `FALSE`, bypass cache reads and refresh the cache
#'   with a successful API response.
#' @param api_urls Optional character vector of API endpoint URLs. If omitted,
#'   the `biomastats.api_urls` option is used, followed by the built-in URLs.
#' @param discovery_url Optional HTTPS URL for a JSON discovery document. If
#'   omitted, the `biomastats.api_discovery_url` option is used, followed by
#'   the package's public discovery document on GitHub.
#' @param timeout Number of seconds allowed for each HTTP request.
#' @param allow_http Logical; allow HTTP endpoints for local development.
#'   HTTPS is required by default.
#' @return A length-one character vector containing the API JSON exactly as it
#'   was received or read from cache.
#' @export
#'
#' @examples
#' \dontrun{
#' manifest_json <- fetch_tile_manifest(
#'   type = "cover", collection = "10", fragment = 1
#' )
#' }
fetch_tile_manifest <- function(
    type = "cover",
    collection = "10",
    fragment,
    use_cache = TRUE,
    api_urls = NULL,
    discovery_url = NULL,
    timeout = 30,
    allow_http = FALSE) {

  if (missing(fragment) || length(fragment) != 1L || is.na(fragment)) {
    stop("'fragment' must be one non-missing value.", call. = FALSE)
  }
  if (length(type) != 1L || is.na(type) || !nzchar(as.character(type))) {
    stop("'type' must be one non-empty value.", call. = FALSE)
  }
  if (length(collection) != 1L || is.na(collection) ||
      !nzchar(as.character(collection))) {
    stop("'collection' must be one non-empty value.", call. = FALSE)
  }
  if (!is.logical(use_cache) || length(use_cache) != 1L || is.na(use_cache)) {
    stop("'use_cache' must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L ||
      is.na(timeout) || timeout <= 0) {
    stop("'timeout' must be one positive number of seconds.", call. = FALSE)
  }

  type <- as.character(type)
  collection <- as.character(collection)
  fragment <- as.character(fragment)
  cache_values <- c(type = type, collection = collection, fragment = fragment)
  if (any(!grepl("^[A-Za-z0-9._-]+$", cache_values))) {
    stop(
      "'type', 'collection', and 'fragment' may contain only letters, numbers, dots, underscores, and hyphens.",
      call. = FALSE
    )
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  validate_manifest <- function(json_text) {
    parsed <- tryCatch(
      jsonlite::fromJSON(json_text, simplifyVector = FALSE),
      error = function(error) error
    )
    if (inherits(parsed, "error")) {
      return(list(ok = FALSE, message = conditionMessage(parsed)))
    }
    if (is.list(parsed) && "error" %in% names(parsed) &&
        length(parsed$error) == 1L && nzchar(as.character(parsed$error))) {
      return(list(ok = FALSE, message = as.character(parsed$error)))
    }

    manifest <- if (is.list(parsed) && "result" %in% names(parsed)) {
      parsed$result
    } else {
      parsed
    }
    if (!is.list(manifest) || is.null(manifest$collection) ||
        !identical(as.character(manifest$collection), collection)) {
      return(list(ok = FALSE, message = "manifest collection does not match the request"))
    }
    if (is.null(manifest$fragment) ||
        !identical(as.character(manifest$fragment), fragment)) {
      return(list(ok = FALSE, message = "manifest fragment does not match the request"))
    }

    list(ok = TRUE)
  }

  cache_ttl_days <- 7
  cache_root <- tools::R_user_dir("biomastats", which = "cache")
  cache_dir <- file.path(cache_root, "manifests")
  cache_ready <- dir.exists(cache_dir) ||
    isTRUE(dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE))
  cache_file <- file.path(
    cache_dir,
    paste0(
      "tile-manifest-v3-type-", type,
      "-collection-", collection,
      "-fragment-", fragment,
      ".json"
    )
  )

  if (cache_ready) {
    cached_files <- list.files(
      cache_dir,
      pattern = "^tile-manifest-v3-.*[.]json$",
      full.names = TRUE
    )
    if (length(cached_files) > 0L) {
      cache_info <- file.info(cached_files)
      cache_age <- as.numeric(
        difftime(Sys.time(), cache_info$mtime, units = "days")
      )
      expired <- is.na(cache_age) | cache_age > cache_ttl_days
      unlink(cached_files[expired], force = TRUE)
    }
  }

  if (use_cache && cache_ready && file.exists(cache_file)) {
    cache_size <- file.info(cache_file)$size
    if (!is.na(cache_size) && cache_size > 0 && cache_size <= 5000000) {
      cached_raw <- readBin(cache_file, what = "raw", n = cache_size)
      cached_json <- rawToChar(cached_raw)
      cached_validation <- validate_manifest(cached_json)
      if (isTRUE(cached_validation$ok)) return(cached_json)
    }
    unlink(cache_file, force = TRUE)
  }

  write_cache <- function(json_raw) {
    if (!cache_ready) return(invisible(FALSE))

    temporary_file <- tempfile(
      pattern = "tile-manifest-",
      tmpdir = cache_dir,
      fileext = ".tmp"
    )
    on.exit(unlink(temporary_file, force = TRUE), add = TRUE)
    writeBin(json_raw, temporary_file)
    copied <- file.copy(temporary_file, cache_file, overwrite = TRUE)
    invisible(isTRUE(copied))
  }

  valid_url <- function(url, allow_http = FALSE) {
    if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
      return(FALSE)
    }
    scheme_ok <- grepl("^https://", url, ignore.case = TRUE) ||
      (allow_http && grepl("^http://", url, ignore.case = TRUE))
    if (!scheme_ok) return(FALSE)
    parsed <- tryCatch(httr::parse_url(url), error = function(error) NULL)
    !is.null(parsed) && nzchar(parsed$hostname %||% "") &&
      is.null(parsed$username) && is.null(parsed$password)
  }

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
    discovery <- "https://raw.githubusercontent.com/iep-ferreira/biomastats/main/inst/api-servers.json"
  }
  if (!valid_url(discovery, allow_http = FALSE)) {
    discovery <- NA_character_
  }

  request_manifest <- function(url) {
    response <- tryCatch(
      httr::POST(
        url,
        httr::add_headers(Accept = "application/json"),
        body = list(collection = collection, fragment = fragment),
        encode = "json",
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

    json_raw <- httr::content(response, as = "raw")
    json_text <- tryCatch(rawToChar(json_raw), error = function(error) error)
    if (inherits(json_text, "error")) {
      return(list(ok = FALSE, retryable = FALSE, message = "response is not valid text"))
    }
    validation <- validate_manifest(json_text)
    if (!isTRUE(validation$ok)) {
      return(list(
        ok = FALSE,
        retryable = FALSE,
        message = paste0("invalid manifest: ", validation$message)
      ))
    }

    list(ok = TRUE, json = json_text, raw = json_raw)
  }

  tried <- character()
  for (url in urls) {
    tried <- c(tried, url)
    result <- request_manifest(url)
    if (isTRUE(result$ok)) {
      write_cache(result$raw)
      return(result$json)
    }
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
          aliases <- c(aliases, version_aliases)
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
      result <- request_manifest(url)
      if (isTRUE(result$ok)) {
        write_cache(result$raw)
        return(result$json)
      }
    }
  }

  stop(
    "Could not fetch the manifest for fragment ", fragment,
    ". Servers attempted: ", paste(tried, collapse = ", "),
    ". The API or discovery document may be unavailable.",
    call. = FALSE
  )
}
