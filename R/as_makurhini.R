#' Conversao de resultados do biomastats para o Makurhini
#'
#' Monta os argumentos de `Makurhini::MK_dPCIIC()`, `MK_dECA()` ou
#' `MK_ProtConn()` a partir de [land_connectivity()] ou [connectivity_change()],
#' com os mesmos fragmentos, areas, limite, distancias e calibracao de k, de modo
#' que os dois pacotes produzam resultados comparaveis. Com run = TRUE e o
#' Makurhini instalado, executa as funcoes e devolve tambem os resultados.
#' @param x Objeto biomastats_connectivity (dPCIIC, ProtConn) ou
#'   biomastats_connectivity_change (dECA; tambem aceita uma lista nomeada por ano
#'   de objetos biomastats_connectivity).
#' @param target "dPCIIC" (padrao), "dECA" ou "ProtConn".
#' @param metric "PC" (padrao) ou "IIC"; ProtConn usa sempre o PC.
#' @param max_dist Limiares a converter, entre os de x; NULL usa todos.
#' @param protected Areas protegidas (sf) para o ProtConn; NULL usa os
#'   fragmentos de habitat de x, o que reproduz os indicadores Prot, ProtConn,
#'   ProtUnconn e RelConn de land_connectivity.
#' @param region Regiao (sf/sfc) do ProtConn; NULL usa o limite de x.
#' @param transboundary Distancia de busca de areas protegidas fora da regiao
#'   (ProtConn), na unidade do CRS; NULL desliga.
#' @param resistance Superficie de [resistance_surface()] ou RasterLayer, para
#'   que dECA e ProtConn usem distancia de menor custo
#'   (`list(type = "least-cost")`) no Makurhini. dPCIIC recebe sempre a matriz de x.
#' @param run Executar o Makurhini (FALSE por padrao).
#' @return Lista biomastats_makurhini com fun (nome da funcao do Makurhini), args
#'   (lista de argumentos por limiar, nomeada como "<max_dist>"; use
#'   `do.call(Makurhini::<fun>, args[[1]])`), mapping (data.frame com k,
#'   probability e distance_threshold de cada limiar), result (resultados por
#'   limiar quando run = TRUE, ou NULL) e metadata.
#' @details A probabilidade do PC no Makurhini e p_ij = exp(d log(probability) /
#'   distance_thresholds), igual a exp(-k d) do biomastats com probability = p_ref
#'   e distance_thresholds = d_ref (ou 0,5 e log(2) / k quando k foi informado).
#'   O IIC usa distance_thresholds = max_dist. dPCIIC recebe a matriz de distancias
#'   de x (borda, externa ou de [cost_distance()]), LA = area do limite em m2,
#'   pij_min = 0 (sem o corte aproximado padrao de 0,01) e overall = TRUE, que
#'   devolve as fracoes intra, direct e step. dECA e ProtConn recalculam as
#'   distancias no Makurhini (borda, ou menor custo entre centroides com
#'   resistance). Areas e LA vao em m2 (area_unit = "m2").
#'
#'   No ProtConn, o Makurhini da as areas protegidas transfronteiricas produto de
#'   atributos 1 (quase nulo) em vez de 0; a diferenca e desprezivel em m2.
#'   Fragmentos diferentes de areas protegidas dao fracoes Within, Contig, Trans e
#'   Unprot que o biomastats nao calcula.
#' @references Saura, S. et al. (2017) Ecological Indicators 76: 144-158.
#'   Godinez-Gomez, O. & Correa Ayram, C. A. (2020) Makurhini,
#'   <https://github.com/connectscape/Makurhini>.
#' @md
#' @examples
#' \dontrun{
#' con <- land_connectivity(poligonos, max_dist = 1000, metric_patch = "PC")
#' mk <- as_makurhini(con, "dPCIIC")
#' res <- do.call(Makurhini::MK_dPCIIC, mk$args[[1]])
#' pc <- as_makurhini(con, "ProtConn", protected = ucs, run = TRUE)
#' pc$result
#' }
#' @export
as_makurhini <- function(x, target = c("dPCIIC", "dECA", "ProtConn"), metric = c("PC", "IIC"),
                         max_dist = NULL, protected = NULL, region = NULL, transboundary = NULL,
                         resistance = NULL, run = FALSE) {
  target <- match.arg(target)
  metric <- if (target == "ProtConn") "PC" else match.arg(metric)
  .connectivity_check_flag(run, "run")
  series <- if (target == "dECA") .makurhini_series(x) else NULL
  ref <- if (is.null(series)) x else series[[1]]
  if (!inherits(ref, "biomastats_connectivity")) {
    stop("x deve vir de land_connectivity (ou connectivity_change para dECA).")
  }
  meta <- ref$metadata
  available <- meta$max_dist_m
  if (is.null(max_dist)) max_dist <- available
  if (!is.numeric(max_dist) || !length(max_dist) || any(!max_dist %in% available)) {
    stop("max_dist deve estar entre os limiares de x.")
  }
  mapping <- do.call(rbind, lapply(max_dist, function(md) {
    k <- ref$metrics$k[match(md, ref$metrics$max_dist_m)]
    calib <- meta$k_calibration
    if (metric == "IIC") {
      data.frame(max_dist = md, k = NA_real_, probability = NA_real_, distance_threshold = md)
    } else if (!is.null(calib$k)) {
      data.frame(max_dist = md, k = k, probability = 0.5, distance_threshold = log(2) / k)
    } else {
      data.frame(max_dist = md, k = k, probability = calib$p_ref,
                 distance_threshold = -log(calib$p_ref) / k)
    }
  }))
  la <- meta$landscape_area_m2
  distance_spec <- if (is.null(resistance)) list(type = "edge") else {
    if (inherits(resistance, "biomastats_resistance")) resistance <- resistance$raster
    list(type = "least-cost", resistance = resistance)
  }
  args <- lapply(seq_len(nrow(mapping)), function(i) {
    m <- mapping[i, ]
    prob <- if (metric == "PC") m$probability
    common <- list(probability = prob, distance_thresholds = m$distance_threshold,
                   area_unit = "m2", intern = FALSE)
    switch(target,
      dPCIIC = c(list(nodes = .makurhini_nodes(ref$analysis_polygons), attribute = "area_m2",
        distance = unname(ref$distance), metric = metric, pij_min = 0, LA = la,
        overall = TRUE), common),
      dECA = c(list(nodes = lapply(series, function(s) .makurhini_nodes(s$analysis_polygons)),
        attribute = "area_m2", LA = la, distance = distance_spec, metric = metric,
        pij_min = 0, plot = FALSE), common),
      ProtConn = {
        crs <- sf::st_crs(ref$analysis_polygons)
        nodes <- if (is.null(protected)) .makurhini_nodes(ref$analysis_polygons)
          else .makurhini_nodes(sf::st_transform(protected, crs))
        reg <- if (is.null(region)) ref$boundary else region
        reg <- sf::st_sf(id = 1L, geometry = sf::st_union(sf::st_geometry(sf::st_transform(reg, crs))))
        c(list(nodes = nodes, region = reg, distance = distance_spec,
          transboundary = transboundary), common)
      })
  })
  names(args) <- as.character(mapping$max_dist)
  fun <- c(dPCIIC = "MK_dPCIIC", dECA = "MK_dECA", ProtConn = "MK_ProtConn")[[target]]
  result <- if (run) {
    f <- .makurhini_fun(fun)
    out <- lapply(args, function(a) .makurhini_plain(suppressMessages(do.call(f, a))))
    # MK_dECA numbers the periods; the years label them here (its own plot
    # labelling draws a figure, so it stays off).
    if (target == "dECA") {
      out <- lapply(out, function(r) {
        if (is.data.frame(r) && "Time" %in% names(r)) r$Time <- names(series)
        r
      })
    }
    out
  }
  structure(list(fun = fun, args = args, mapping = mapping, result = result,
    metadata = list(target = target, metric = metric, area_unit = "m2",
      distance_unit = meta$distance_unit, distance_source = meta$distance_source,
      makurhini_distance = if (target == "dPCIIC") "matrix from biomastats" else distance_spec$type,
      years = names(series), protected = if (target == "ProtConn") {
        if (is.null(protected)) "habitat patches" else "supplied"
      })), class = c("biomastats_makurhini", "list"))
}

# Nodes as Makurhini expects them: an sf with an id and an area attribute (m2).
.makurhini_nodes <- function(p) {
  sf::st_sf(fragment_id = if ("fragment_id" %in% names(p)) p$fragment_id else seq_len(nrow(p)),
    area_m2 = as.numeric(units::set_units(sf::st_area(p), "m^2")),
    geometry = sf::st_geometry(p))
}

# Makurhini tables are formattable objects; plain data.frames print and
# serialize without that package.
.makurhini_plain <- function(x) {
  if (inherits(x, "formattable")) {
    attr(x, "formattable") <- NULL
    class(x) <- "data.frame"
    return(x)
  }
  if (is.list(x) && !is.data.frame(x)) return(lapply(x, .makurhini_plain))
  x
}

.makurhini_series <- function(x) {
  if (inherits(x, "biomastats_connectivity_change")) return(x$connectivity)
  if (is.list(x) && length(x) > 1L && !is.null(names(x)) &&
      all(vapply(x, inherits, logical(1), what = "biomastats_connectivity"))) return(x)
  stop("dECA exige connectivity_change ou lista nomeada por ano de land_connectivity.")
}
