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
  if (!is.numeric(collection) || length(collection) != 1L ||
      !is.finite(collection) || collection != trunc(collection)) {
    stop("collection deve ser um codigo inteiro reconhecido por dict_build.")
  }
  collection
}

.biomastats_dictionary <- function(collection, codes) {
  dic <- dict_build(.biomastats_collection(collection))
  codes <- sort(unique(codes))
  pos <- match(as.character(codes), as.character(dic$code))
  data.frame(code = codes,
    classe = ifelse(is.na(pos), paste("Classe", codes), as.character(dic$classe[pos])),
    class = ifelse(is.na(pos), paste("Class", codes), as.character(dic$class[pos])),
    grupo = as.character(dic$grupo[pos]), group = as.character(dic$group[pos]),
    color = ifelse(is.na(pos), "#999999", as.character(dic$color[pos])), known = !is.na(pos))
}
