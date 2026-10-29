# Figuras e tabelas do capitulo de contexto ampliado do tutorial (Lagoa do Sino,
# entorno ate 10 km), em portugues e ingles. Roda uma vez neste computador; o
# livro so exibe os arquivos gravados em tutorial/imagens/, e o build (local ou
# GitHub) nao repete a analise.
# Uso, na raiz do pacote: Rscript development/gerar_figuras_contexto.R
# Requer internet (leitura por HTTP do MapBiomas, cerca de 2 a 3 min).
# Instalado numa biblioteca temporaria: os processos paralelos carregam o pacote.
biblioteca <- file.path(tempdir(), "biomastats-contexto")
dir.create(biblioteca, showWarnings = FALSE)
Sys.setenv(R_LIBS = paste(c(biblioteca, .libPaths()), collapse = .Platform$path.sep))
stopifnot(system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", ".", paste0("--library=",
  shQuote(biblioteca)), "--no-test-load"), stdout = FALSE, stderr = FALSE) == 0)
.libPaths(c(biblioteca, .libPaths()))
library(biomastats, lib.loc = biblioteca)
interno <- function(f) getFromNamespace(f, "biomastats")
saida <- file.path("tutorial", "imagens")
raios <- c(0, 1000, 2500, 5000, 10000)
max_dist <- 300

ufscar <- system.file("shp", "UFSCar.shp", package = "biomastats")
mapas <- load_rasters(ufscar, start = 2024, end = 2024, method = "http",
                      product = "mapbiomas_brasil_c10")
contexto <- connectivity_context(mapas, classes = 3, max_dist = max_dist, context = raios,
  metric_patch = c("IIC", "PC"), d_ref = max_dist / 2, parallel = 4)

# Os tres fragmentos mais importantes da fazenda (PC, maior raio), juntos e isolados.
ps <- contexto$patch_sensitivity
ref <- ps[ps$metric == "PC" & ps$radius_m == max(raios), ]
top <- utils::head(ref$fragment_id[order(ref$rank)], 3)
alvos <- connectivity_context(mapas, classes = 3, max_dist = max_dist, context = raios,
  metric_patch = "PC", d_ref = max_dist / 2, parallel = 4, build_plot = FALSE,
  targets = c(list(conjunto = top), stats::setNames(as.list(top), paste0("f", top))))

# Efeito de borda por fragmento: ganho de importancia com o contexto contra a
# distancia ao limite da fazenda.
poligonos <- contexto$polygons[["2024"]]
poligonos <- poligonos[poligonos$value == 1, ]
utm <- sf::st_crs(32722)
limite <- sf::st_boundary(sf::st_union(sf::st_transform(contexto$focal, utm)))
borda <- do.call(rbind, lapply(c("IIC", "PC"), function(metric) {
  sem <- ps[ps$metric == metric & ps$radius_m == 0, ]
  com <- ps[ps$metric == metric & ps$radius_m == max(raios), ]
  ids <- intersect(sem$fragment_id, com$fragment_id)
  geo <- sf::st_transform(poligonos[match(ids, poligonos$fragment_id), ], utm)
  data.frame(metric = metric, fragment_id = ids,
    area_ha = as.numeric(sf::st_area(geo)) / 1e4,
    distancia_m = as.numeric(sf::st_distance(geo, limite)),
    papel = com$focal_role[match(ids, com$fragment_id)],
    ganho = com$importance[match(ids, com$fragment_id)] -
      sem$importance[match(ids, sem$fragment_id)])
}))

grafico_borda <- function(language) {
  tr <- function(pt, en) if (language == "en") en else pt
  b <- borda
  b$papel <- factor(ifelse(b$papel == "edge", tr("Cruza o limite", "Crosses the boundary"),
                           tr("Interno", "Inside")),
                    levels = c(tr("Interno", "Inside"), tr("Cruza o limite", "Crosses the boundary")))
  ggplot2::ggplot(b, ggplot2::aes(x = distancia_m, y = ganho, size = area_ha, colour = papel)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555") +
    ggplot2::geom_point(alpha = 0.75) +
    ggplot2::scale_colour_manual(values = c("#125c40", "#d08c2d"), name = NULL) +
    ggplot2::scale_size_continuous(range = c(1.5, 7), name = tr("Área (ha)", "Area (ha)")) +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::labs(x = tr("Distância ao limite da fazenda (m)", "Distance to the farm boundary (m)"),
                  y = tr(sprintf("Ganho de importância com %d km de contexto (p.p.)", max(raios) / 1000),
                         sprintf("Importance gain with %d km of context (p.p.)", max(raios) / 1000))) +
    ggplot2::theme_minimal(base_size = 12) + ggplot2::theme(legend.position = "bottom")
}

maior <- contexto$connectivity[["2024"]][[as.character(max(raios))]]
figuras <- function(language) list(
  mapa = interno(".context_map_plot")(maior, contexto$focal, contexto$contexts, language),
  sensibilidade = interno(".context_sensitivity_plot")(contexto$sensitivity, max_dist, language),
  ranking = interno(".context_rank_plot")(contexto$rank_stability, language),
  borda = grafico_borda(language),
  alvos = interno(".context_target_plot")(alvos$target_sensitivity, max_dist, language))
nomes <- list(pt = c(mapa = "contexto-mapa", sensibilidade = "contexto-sensibilidade",
                     ranking = "contexto-ranking", borda = "contexto-borda", alvos = "contexto-alvos"),
              en = c(mapa = "en-context-map", sensibilidade = "en-context-sensitivity",
                     ranking = "en-context-ranking", borda = "en-context-edge", alvos = "en-context-targets"))
alturas <- c(mapa = 7, sensibilidade = 4.5, ranking = 4.5, borda = 5, alvos = 4.5)
for (language in c("pt", "en")) {
  g <- figuras(language)
  for (n in names(g)) {
    ggplot2::ggsave(file.path(saida, paste0(nomes[[language]][[n]], ".png")), g[[n]],
                    width = 9, height = alturas[[n]], dpi = 150)
  }
}

# Tabelas pequenas para o texto do capitulo.
trampolins <- contexto$stepping_stones
trampolins <- trampolins[trampolins$radius_m == max(raios), ]
saveRDS(list(raios = raios, max_dist = max_dist, ano = 2024,
             sensitivity = contexto$sensitivity, rank_stability = contexto$rank_stability,
             stepping = do.call(rbind, lapply(split(trampolins, trampolins$metric), function(t) utils::head(t[order(-t$connector), ], 10))),
             targets = alvos$target_sensitivity, borda = borda,
             fragmentos = c(fazenda = sum(contexto$sensitivity$patches[contexto$sensitivity$radius_m == 0 &
                                                                     contexto$sensitivity$metric == "PC"]),
                            contexto = max(contexto$sensitivity$patches))),
        file.path(saida, "contexto-lagoa.rds"))
message("Figuras e tabelas gravadas em ", saida)
