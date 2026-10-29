# Null-default operator for every R version supported by the package
# (base R only provides it from 4.4.0 on).
`%||%` <- function(x, y) if (is.null(x)) y else x

# Shared dictionary contract for native analytical objects.
# Land-cover schemes other than MapBiomas collections are identified by name.
.lulc_schemes <- function() "esa_worldcover"

.biomastats_collection <- function(collection) {
  if (is.character(collection) && length(collection) == 1L && collection %in% .lulc_schemes()) return(collection)
  if (is.character(collection) && length(collection) == 1L &&
      !is.na(collection) && grepl("^[0-9]+$", collection)) collection <- as.numeric(collection)
  # Local collections (load_local_rasters) are named; their dictionary travels
  # with the maps object as class_dictionary.
  if (is.character(collection) && length(collection) == 1L && !is.na(collection) &&
      nzchar(collection)) return(collection)
  if (!is.numeric(collection) || length(collection) != 1L ||
      !is.finite(collection) || collection != trunc(collection)) {
    stop("collection deve ser um codigo inteiro reconhecido por dict_build ou o nome de uma colecao local.")
  }
  collection
}

# Full dictionary (dict_build() format) of a collection: the one supplied
# (class_dictionary of local maps), dict_build() for known collections, or
# NULL for a collection without dictionary.
.collection_dictionary <- function(collection, dictionary = NULL) {
  if (!is.null(dictionary)) return(dictionary)
  if (is.null(collection)) return(NULL)
  tryCatch(dict_build(.biomastats_collection(collection)), error = function(e) NULL)
}

# Dictionary entries of `codes`. Codes absent from the dictionary (or every
# code, without a dictionary) get generic names and grey, with known = FALSE.
.biomastats_dictionary <- function(collection, codes, dictionary = NULL) {
  dic <- .collection_dictionary(collection, dictionary)
  if (is.null(dic)) {
    dic <- data.frame(grupo = character(), group = character(), classe = character(),
                      class = character(), code = numeric(), color = character())
  }
  codes <- sort(unique(codes))
  pos <- match(as.character(codes), as.character(dic$code))
  data.frame(code = codes,
    classe = ifelse(is.na(pos), paste("Classe", codes), as.character(dic$classe[pos])),
    class = ifelse(is.na(pos), paste("Class", codes), as.character(dic$class[pos])),
    grupo = as.character(dic$grupo[pos]), group = as.character(dic$group[pos]),
    color = ifelse(is.na(pos), "#999999", as.character(dic$color[pos])), known = !is.na(pos))
}
