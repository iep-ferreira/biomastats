#' Distancia de menor custo entre fragmentos
#'
#' Calcula a matriz de distancias efetivas (menor custo acumulado) entre os
#' fragmentos de habitat sobre uma superficie de [resistance_surface()], pronta
#' para `land_connectivity(distance = )` e para o Makurhini. O metodo nativo mede
#' de borda a borda; o metodo "makurhini" delega a
#' `Makurhini::distancefile(type = "least-cost")`, que mede entre centroides.
#' @param polygons Objeto sf de [reclass_to_polygons()] ou sua lista temporal,
#'   como em [land_connectivity()].
#' @param resistance Objeto biomastats_resistance, RasterLayer ou SpatRaster com
#'   custos positivos por unidade de distancia (NA = barreira), em CRS projetado
#'   em metros.
#' @param year Ano a analisar, como em [land_connectivity()].
#' @param method "biomastats" (padrao, borda a borda, nativo) ou "makurhini"
#'   (centroides, exige o pacote Makurhini instalado do GitHub).
#' @param parallel FALSE (padrao), TRUE (plano future do usuario) ou numero de
#'   processos; distribui os fragmentos de origem do metodo nativo.
#' @return Matriz simetrica n x n com nomes iguais a fragment_id, na unidade de
#'   custo x metro. Inf indica pares separados por barreiras. Atributos unit,
#'   method, year e resolution acompanham a matriz e sao lidos por
#'   [land_connectivity()].
#' @details O metodo nativo monta o grafo das celulas validas com vizinhanca de 8
#'   (passo diagonal bloqueado quando as duas celulas ortogonais sao barreiras) e
#'   custo de passo igual ao comprimento do passo vezes a media dos custos das duas
#'   celulas, como gdistance e Makurhini. Cada fragmento e uma origem virtual
#'   ligada as celulas vizinhas fora dele, e cada destino um sumidouro ligado as
#'   celulas vizinhas fora dele, ambos com meio passo: assim a distancia vai de
#'   borda a borda e, com custo 1 em toda parte, reproduz a distancia entre bordas
#'   na grade (com o desvio usual de 8 direcoes em angulos intermediarios). O
#'   interior dos demais fragmentos tem o seu custo, e nao e atalho gratuito: o
#'   papel de trampolim fica no grafo de conectividade. Fragmentos com celulas
#'   vizinhas tem distancia zero. Fragmentos menores que uma celula ocupam a celula
#'   do seu ponto interior. Memoria e tempo crescem com o numero de celulas; recorte
#'   a superficie a paisagem estudada.
#' @md
#' @examples
#' \dontrun{
#' custo <- cost_distance(poligonos, r, year = 2024)
#' conectividade <- land_connectivity(poligonos, year = 2024, distance = custo,
#'   max_dist = 2000, metric_patch = c("IIC", "PC"))
#' }
#' @export
cost_distance <- function(polygons, resistance, year = NULL,
                          method = c("biomastats", "makurhini"), parallel = FALSE) {
  method <- match.arg(method)
  .connectivity_check_parallel(parallel)
  if (inherits(resistance, "biomastats_resistance")) resistance <- resistance$raster
  if (!inherits(resistance, c("RasterLayer", "SpatRaster"))) {
    stop("resistance deve ser biomastats_resistance, RasterLayer ou SpatRaster.")
  }
  r <- if (inherits(resistance, "SpatRaster")) resistance else terra::rast(resistance)
  crs <- sf::st_crs(terra::crs(r))
  if (is.na(crs) || sf::st_is_longlat(crs) || !.connectivity_metres(crs)) {
    stop("A superficie de resistencia deve estar em CRS projetado em metros.")
  }
  f <- terra::values(r, mat = FALSE)
  if (any(!is.na(f) & f <= 0)) stop("Custos devem ser positivos (NA = barreira).")
  f[!is.finite(f)] <- NA_real_
  p <- .connectivity_select_year(polygons, year)
  if ("value" %in% names(p)) p <- p[!is.na(p$value) & p$value == 1, , drop = FALSE]
  if (nrow(p) < 2L) stop("Sao necessarios ao menos dois fragmentos.")
  if (!"fragment_id" %in% names(p)) {
    p$fragment_id <- if ("frag_id" %in% names(p)) p$frag_id else seq_len(nrow(p))
  }
  if (anyNA(p$fragment_id) || anyDuplicated(p$fragment_id)) stop("fragment_id deve ser unico e sem NA.")
  p <- sf::st_transform(p, crs)
  ids <- as.character(p$fragment_id)
  if (method == "makurhini") {
    D <- .cost_distance_makurhini(p, resistance)
  } else {
    if (is.numeric(parallel)) {
      old_plan <- future::plan(future::multisession, workers = parallel)
      on.exit(future::plan(old_plan), add = TRUE)
    }
    cells <- .cost_patch_cells(p, r)
    if (any(is.na(f[!is.na(cells)]))) stop("Resistencia ausente (NA) em celulas de habitat.")
    D <- .cost_distance_native(cells, f, terra::nrow(r), terra::ncol(r), terra::res(r),
                               nrow(p), !isFALSE(parallel))
  }
  dimnames(D) <- list(ids, ids)
  structure(D, unit = "cost-weighted m",
    method = if (method == "makurhini") "least-cost centroid (Makurhini::distancefile)"
      else "least-cost edge-to-edge (biomastats, 8 neighbours)",
    year = if ("year" %in% names(p)) unique(p$year) else year, resolution = terra::res(r))
}

# Patch index of every cell (NA outside habitat). Patches without a cell
# centre inside take the cell of their interior point.
.cost_patch_cells <- function(p, r) {
  v <- terra::vect(sf::st_sf(idx = seq_len(nrow(p)), geometry = sf::st_geometry(p)))
  cells <- terra::values(terra::rasterize(v, r, field = "idx"), mat = FALSE)
  missing <- setdiff(seq_len(nrow(p)), cells)
  if (length(missing)) {
    pts <- sf::st_coordinates(suppressWarnings(sf::st_point_on_surface(p[missing, ])))
    at <- terra::cellFromXY(r, pts[, 1:2, drop = FALSE])
    if (anyNA(at) || any(!is.na(cells[at])) || anyDuplicated(at)) {
      stop("Fragmentos fora da superficie de resistencia ou menores que a celula sem celula livre.")
    }
    cells[at] <- missing
  }
  cells
}

# Edge-to-edge least cost between patches on the 8-neighbour cell graph.
# Vertices: cells 1..N, then one source S_i and one sink T_i per patch.
# Cell steps cost length * mean(f_a, f_b) both ways; S_i -> b (b next to i,
# outside it) costs half a step in b; a -> T_j (a next to j, outside it) costs
# half a step in a. Directed source and sink edges keep patches from being
# free shortcuts.
.cost_distance_native <- function(cells, f, nr, nc, res, n, parallel = FALSE) {
  N <- nr * nc
  row <- rep(seq_len(nr), each = nc)
  col <- rep(seq_len(nc), times = nr)
  valid <- !is.na(f)
  offsets <- list(c(0, 1), c(1, 0), c(1, 1), c(1, -1))
  pairs <- do.call(rbind, lapply(offsets, function(o) {
    a <- which(row + o[1] <= nr & col + o[2] >= 1 & col + o[2] <= nc)
    b <- a + o[1] * nc + o[2]
    ok <- valid[a] & valid[b]
    if (o[1] == 1 && o[2] != 0) {
      # No corner cutting between two barrier cells.
      ok <- ok & (valid[a + o[2]] | valid[a + nc])
    }
    a <- a[ok]; b <- b[ok]
    cbind(a, b, rep(sqrt((o[1] * res[2])^2 + (o[2] * res[1])^2), length(a)))
  }))
  a <- pairs[, 1]; b <- pairs[, 2]; step <- pairs[, 3]
  pa <- cells[a]; pb <- cells[b]
  w <- step * (f[a] + f[b]) / 2
  from <- c(a, b); to <- c(b, a); weight <- c(w, w)
  # Leaving patch pa into b, and leaving pb into a.
  out_a <- !is.na(pa) & (is.na(pb) | pb != pa)
  out_b <- !is.na(pb) & (is.na(pa) | pa != pb)
  from <- c(from, N + pa[out_a], N + pb[out_b], b[out_a], a[out_b])
  to <- c(to, b[out_a], a[out_b], N + n + pa[out_a], N + n + pb[out_b])
  half_b <- step[out_a] * f[b[out_a]] / 2
  half_a <- step[out_b] * f[a[out_b]] / 2
  weight <- c(weight, half_b, half_a, half_b, half_a)
  g <- igraph::make_graph(as.vector(rbind(from, to)), n = N + 2L * n, directed = TRUE)
  block <- function(i) {
    list(igraph::distances(g, v = N + i, to = N + n + seq_len(n), mode = "out",
                           weights = weight, algorithm = "dijkstra"))
  }
  D <- do.call(rbind, .map_chunks(n, block, parallel, simplify = FALSE))
  D <- pmin(D, t(D))
  contact <- !is.na(pa) & !is.na(pb) & pa != pb
  D[cbind(pa[contact], pb[contact])] <- 0
  D[cbind(pb[contact], pa[contact])] <- 0
  diag(D) <- 0
  unname(D)
}

# Makurhini is installed from GitHub and is not declared in Suggests, so it is
# reached through a variable name.
.makurhini_fun <- function(name) {
  pkg <- "Makurhini"
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Instale o Makurhini: remotes::install_github(\"connectscape/Makurhini\").")
  }
  getExportedValue(pkg, name)
}

.cost_distance_makurhini <- function(p, resistance) {
  nodes <- sf::st_sf(fragment_id = p$fragment_id, geometry = sf::st_geometry(p))
  r <- if (inherits(resistance, "SpatRaster")) {
    raster::setValues(raster::raster(resistance), terra::values(resistance, mat = FALSE))
  } else resistance
  D <- .makurhini_fun("distancefile")(nodes = nodes, id = "fragment_id", type = "least-cost",
    resistance = r, ngh = 8)
  # A From/To/Distance table (pairwise = TRUE) or a matrix named by id.
  D <- .connectivity_distance(if (is.data.frame(D)) D else as.matrix(D), p$fragment_id)
  attr(D, "unit") <- NULL
  unname(D)
}
