#' Priorizacao otimizada de fragmentos por IIC, PC ou AWF
#'
#' Reutiliza a matriz de distancias de um objeto lconnect, sem reconstruir
#' geometrias ou recalcular distancias a cada remocao de fragmento, e
#' decompoe a importancia em fracoes intra, fluxo e conector.
#' @param landscape Objeto de classe lconnect, produzido por
#'   [land_connectivity()] ou lconnect::upload_land(). Deve usar CRS em metros.
#' @param metric Indice de priorizacao: "IIC" (padrao), "PC" ou "AWF".
#' @param parallel Paraleliza o teste de candidatos, o conector e a poda (IIC e
#'   PC por decomposicao), que dominam o tempo. FALSE (padrao) roda em
#'   sequencia; TRUE usa o plano future ja definido pelo usuario; um numero
#'   abre um plano multisession temporario com esse numero de processos e
#'   restaura o anterior. Requer future.apply. Compensa a partir de algumas
#'   centenas de fragmentos (PC da Lagoa do Sino, 461 fragmentos: cerca de 4
#'   vezes mais rapido com 8 processos); em paisagens pequenas, abrir e
#'   alimentar os processos custa mais que o ganho.
#' @param prune Poda exata das arestas do PC: "auto" (padrao) poda redes densas
#'   (ao menos 50 fragmentos e 25% dos pares com ligacao finita); TRUE e FALSE
#'   forcam a escolha. O resultado e o mesmo com ou sem poda; so o tempo muda
#'   (ate 20 vezes com 400 fragmentos). Sem efeito em IIC e AWF.
#' @param method "decomposition" (padrao), pela identidade de Saura & Rubio
#'   (2010), ou "removal", que recalcula o indice apos cada remocao e serve
#'   de referencia. Sem efeito para AWF, calculado em forma fechada.
#' @param iic_rule "definition" (padrao) segue Pascual-Hortal & Saura (2006)
#'   e inclui fragmentos isolados; "lconnect" reproduz o lconnect 0.1.2, que
#'   omite isolados e contatos a distancia zero, apenas para comparacao.
#' @param k Constante de decaimento (1/m) de p_ij = exp(-k d_ij), usada por
#'   PC e AWF. Tem prioridade sobre p_ref e d_ref.
#' @param p_ref,d_ref Calibracao de k pela probabilidade de dispersao p_ref na
#'   distancia d_ref (m): k = -log(p_ref) / d_ref. Padrao: p = 0,5 a
#'   max_dist / 2, como no lconnect.
#' @return Objeto de classe pimp com landscape (sf com coluna attributes),
#'   prioritization (reducao percentual do indice apos remover cada fragmento)
#'   e fractions (data.frame com intra, flux e connector, em % do indice, que
#'   somam prioritization; NULL na regra lconnect e no metodo removal).
#'   Atributos: metric, status, index (valor do indice na paisagem), k,
#'   method, iic_rule, connector_candidates, pruned (se houve poda) e edges
#'   (arestas do grafo de caminhos). Nao escreve arquivos.
#' @details IIC e PC tem a forma I = sum_ij a_i a_j M_ij / A_L^2, somada em
#'   todos os fragmentos, inclusive i = j e os isolados (M_ii = 1). No IIC,
#'   M_ij = 1 / (1 + nl_ij), com ligacao quando d_ij < max_dist, inclusive
#'   contatos (d = 0). No PC (Saura & Pascual-Hortal 2007), M_ij = p*_ij, a
#'   maior probabilidade entre todos os caminhos, diretos ou indiretos, de
#'   modo que um trampolim (stepping stone) recebe importancia de conector.
#'   AWF (Jordan et al. 2003) soma apenas o fluxo direto a_i a_j exp(-k d_ij),
#'   i != j, mesmo alem do limiar; sua importancia e toda fluxo.
#'
#'   A decomposicao calcula intra (a_r^2) e fluxo em forma fechada e recalcula
#'   o conector apenas para fragmentos que estao em algum melhor caminho entre
#'   outros dois, e so entre esses pares. No PC, arestas com dois ou mais
#'   desvios estritamente mais curtos podem ser podadas sem alterar o
#'   resultado, tambem no metodo removal, que monta o grafo uma vez e apaga
#'   um vertice por remocao.
#'   A area do limite permanece fixa em todas as remocoes.
#'
#'   Quando o indice inicial e zero ou ha um unico fragmento, a prioridade e
#'   indefinida (NaN) e status = "undefined_baseline". Memoria quadratica no
#'   numero de fragmentos; a poda do PC custa n^3 operacoes.
#' @references Pascual-Hortal, L. & Saura, S. (2006) Landscape Ecology 21:
#'   959-967. Saura, S. & Pascual-Hortal, L. (2007) Landscape and Urban
#'   Planning 83: 91-103. Saura, S. & Rubio, L. (2010) Ecography 33: 523-537.
#'   [lconnect](https://cran.r-project.org/package=lconnect).
#' @md
#' @examples
#' \dontrun{
#' land <- land_connectivity(poligonos, max_dist = 300)$scenarios[[1]]$land
#' pc <- patch_imp_fast(land, metric = "PC", p_ref = 0.05, d_ref = 300)
#' head(pc$fractions)
#' # paisagens grandes: 4 processos temporarios
#' pc4 <- patch_imp_fast(land, metric = "PC", parallel = 4)
#' }
#' @export
patch_imp_fast <- function(landscape, metric = c("IIC", "PC", "AWF"), parallel = FALSE,
                           method = c("decomposition", "removal"),
                           iic_rule = c("definition", "lconnect"),
                           k = NULL, p_ref = 0.5, d_ref = NULL, prune = "auto") {
  metric <- match.arg(metric)
  method <- match.arg(method)
  iic_rule <- match.arg(iic_rule)
  .connectivity_check_parallel(parallel)
  checked <- .connectivity_check_land(landscape)
  kk <- if (metric == "IIC") NULL else .calibrate_k(landscape$max_dist, k, p_ref, d_ref)
  # A number of processes opens a temporary plan; the user's plan is restored.
  if (is.numeric(parallel)) {
    old_plan <- future::plan(future::multisession, workers = parallel)
    on.exit(future::plan(old_plan), add = TRUE)
  }
  res <- .patch_imp_engine(checked$D, checked$areas, landscape$max_dist,
    as.numeric(landscape$area_l), metric, !isFALSE(parallel), method, iic_rule, kk, prune)
  .as_pimp(landscape, res, metric, iic_rule)
}

# lconnect object (from land_connectivity or lconnect::upload_land) checked
# for metric CRS and coherent distances, areas, area_l and max_dist.
.connectivity_check_land <- function(landscape) {
  if (!inherits(landscape, "lconnect") || !inherits(landscape$landscape, "sf")) {
    stop("landscape deve ser um objeto lconnect com landscape sf.")
  }
  p <- landscape$landscape
  if (!nrow(p) || is.na(sf::st_crs(p)) || sf::st_is_longlat(p) ||
      !.connectivity_metres(sf::st_crs(p))) stop("landscape deve ter fragmentos em CRS projetado em metros.")
  d <- unname(as.matrix(landscape$distance))
  a <- as.numeric(units::set_units(sf::st_area(p), "m^2"))
  md <- landscape$max_dist
  al <- as.numeric(landscape$area_l)
  if (!is.numeric(d) || !identical(dim(d), c(nrow(p), nrow(p))) ||
      any(!is.finite(d) | d < 0) || !isTRUE(all.equal(d, t(d))) ||
      any(diag(d) != 0) || any(!is.finite(a) | a <= 0) ||
      !is.numeric(md) || length(md) != 1L || !is.finite(md) || md <= 0 ||
      length(al) != 1L || !is.finite(al) || al <= 0) {
    stop("Distancias, areas, area_l e max_dist devem ser validos e compativeis.")
  }
  list(D = d, areas = a)
}
