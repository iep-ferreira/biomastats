#!/usr/bin/env Rscript

# Este arquivo é um roteiro manual. Execute os blocos selecionados no RStudio
# com Ctrl+Enter. Não é necessário executar o arquivo inteiro de uma vez.

# ---- FLUXO 1: testes do código em desenvolvimento --------------------------

# Execute somente este bloco para rodar a suíte testthat no código local.
if ("biomastats" %in% loadedNamespaces()) {
  stop(
    paste(
      "O namespace biomastats já está carregado nesta sessão.",
      "Reinicie o R antes de executar o Fluxo 1."
    ),
    call. = FALSE
  )
}
library(devtools)
devtools::load_all(".")
stopifnot(is.function(load_features))
devtools::test()


# ---- FLUXO 2: instalação limpa e uso como usuário --------------------------

# 1. Identificar a branch, o último commit e a árvore de trabalho
branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
commit <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)
alteracoes_locais <- system2("git", c("status", "--porcelain"), stdout = TRUE)
message(
  "Branch: ", branch,
  " | commit: ", commit,
  " | alterações locais: ", length(alteracoes_locais)
)


# 2. Descarregar qualquer instalação usada anteriormente nesta sessão.
# Isso deve ocorrer antes de remover bibliotecas temporárias, pois o namespace
# pode estar usando os arquivos .rdb/.rdx da instalação anterior.
if ("package:biomastats" %in% search()) {
  detach_resultado <- tryCatch(
    {
      detach("package:biomastats", unload = TRUE, character.only = TRUE)
      NULL
    },
    error = identity
  )
  if (inherits(detach_resultado, "error")) {
    stop(
      "Não foi possível descarregar biomastats. Reinicie o R e execute este fluxo novamente.",
      call. = FALSE
    )
  }
}
if ("biomastats" %in% loadedNamespaces()) {
  unload_resultado <- tryCatch(unloadNamespace("biomastats"), error = identity)
  if (inherits(unload_resultado, "error")) {
    stop(
      "O namespace biomastats está corrompido. Reinicie o R antes de continuar.",
      call. = FALSE
    )
  }
}
invisible(gc())

# 3. Preparar uma biblioteca exclusiva para esta execução e usar nela a
# árvore de trabalho atual. tempfile() evita reutilizar uma base lazy-load.
biblioteca <- tempfile("biomastats-local-lib-")
origem <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
dir.create(biblioteca, recursive = TRUE, showWarnings = FALSE)

bibliotecas_anteriores <- .libPaths()
.libPaths(c(biblioteca, bibliotecas_anteriores))

# 4. Instalar o pacote
status_instalacao <- system2(
  file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", paste0("--library=", biblioteca), "--no-multiarch", origem)
)
stopifnot(identical(status_instalacao, 0L))


# 5. Carregar o pacote instalado
library(biomastats, lib.loc = biblioteca)
stopifnot(packageVersion("biomastats") == package_version(read.dcf("DESCRIPTION")[1, "Version"]))
packageVersion("biomastats")
find.package("biomastats")

# 6. Ler os shapefiles instalados
ufscar_shp <- system.file("shp", "UFSCar.shp", package = "biomastats")
polygon_shp <- system.file("shp", "polygon.shp", package = "biomastats")
ufscar <- sf::read_sf(ufscar_shp)
polygon_estudo <- sf::read_sf(polygon_shp)
stopifnot(nrow(ufscar) > 0, nrow(polygon_estudo) > 0)
ufscar
polygon_estudo

# 7. Carregar os mapas de exemplo do pacote
arquivo_exemplo <- system.file("examples", "mapa_exemplo.Rdata", package = "biomastats")
ambiente_exemplo <- new.env(parent = emptyenv())
load(arquivo_exemplo, envir = ambiente_exemplo)
mapas <- ambiente_exemplo[["mapas"]]
mapas[["collection"]] <- 10
names(mapas)
mapas[["time_range"]]

# 8. Calcular áreas
areas <- biomastats::get_area(mapas)
areas[["aggregate_data"]]

# 9. Visualizar uso e cobertura da terra
primeiro_ano <- mapas[["time_range"]][[1]]
ultimo_ano <- mapas[["time_range"]][[2]]
ano_visualizacao <- min(1990, ultimo_ano)

mapa_uso <- biomastats::land_vis(mapas, year = ano_visualizacao)
mapa_uso

mapa_uso_final <- biomastats::land_vis(mapas, year = ultimo_ano)
mapa_uso_final

# 10. Distribuir classes
distribuicao_barra <- biomastats::land_dist(areas, year = ultimo_ano, type = "barplot")
distribuicao_barra

distribuicao_pizza <- biomastats::land_dist(areas, year = ultimo_ano, type = "pie")
distribuicao_pizza

# 11. Calcular métrica e gerar mapa reclassificado
metricas <- biomastats::biomastats_metrics(
  mapas,
  start = primeiro_ano,
  end = primeiro_ano + 1,
  zone = "22",
  hemisphere = "south",
  classes = c(3, 4),
  metrics = "aggregation_index"
)
metricas[["metrics_table"]]

mapa_reclassificado <- biomastats::reclass_map(metricas, year = primeiro_ano)
mapa_reclassificado

# 12. Opcional: mapas interativos no RStudio
# mapview::mapview(ufscar)
# mapview::mapview(polygon_estudo)

# 13. Restaurar a biblioteca original ao terminar o fluxo 2
.libPaths(bibliotecas_anteriores)
