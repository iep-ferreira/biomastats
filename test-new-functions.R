# Teste manual de funções em desenvolvimento.
# Execute a partir da raiz do projeto:
# source("test-new-functions.R")

if (!file.exists("DESCRIPTION")) {
  stop("Execute este script a partir da raiz do projeto biomastats.")
}

if (!requireNamespace("devtools", quietly = TRUE)) {
  stop("Instale o pacote 'devtools' para carregar o código local.")
}

devtools::load_all(quiet = TRUE)

example_path <- system.file(
  "examples", "mapa_exemplo.Rdata", package = "biomastats"
)
fixture <- new.env(parent = emptyenv())
load(example_path, envir = fixture)
mapas <- fixture[["mapas"]]
if (is.null(mapas[["collection"]])) {
  mapas[["collection"]] <- 10
}

dev_file <- file.path("development", "functions", "get_area_mod.R")
if (!file.exists(dev_file)) {
  stop("Função experimental não encontrada: ", dev_file)
}

# O ambiente separado mantém a implementação original disponível para o
# protótipo, sem substituir permanentemente o arquivo em R/.
fn_env <- new.env(parent = globalenv())
fn_env[["get_area_original"]] <- biomastats::get_area
sys.source(dev_file, envir = fn_env)
get_area_mod <- fn_env[["get_area_mod"]]

resultado <- testthat::with_mocked_bindings(
  {
    areas <- biomastats::get_area(mapas, plot_type = "areaplot")
    if (interactive()) {
      print(areas[["time_series"]])
    }

    metrics <- biomastats::biomastats_metrics(
      mapas,
      start = 1985,
      end = 1986,
      zone = "22",
      hemisphere = "south",
      classes = c(3, 4),
      metrics = "aggregation_index"
    )
    print(metrics[["metrics_table"]])

    list(areas = areas, metrics = metrics)
  },
  get_area = get_area_mod,
  .package = "biomastats"
)

message("Teste manual concluído. O mock foi restaurado ao final do bloco.")
invisible(resultado)
