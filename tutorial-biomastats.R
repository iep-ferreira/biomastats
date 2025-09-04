# Instalação e Carregamento do Pacote
remove.packages("biomastats") # Opcional: para reinstalação
# install.packages("devtools")
library(devtools)
devtools::install_github("iep-ferreira/biomastats")
library(biomastats)

# Definição do Recorte Espacial
## Opção 1: Importar shapefile existente
path_package <- system.file(package = "biomastats")
ufscar_shp <- file.path(path_package, "shp/UFSCar.shp")

## Opção 2: Criar polígono a partir de coordenadas
make_polygon(lat = -23.605, lon = -48.529, size = 2.5, shape = "hexagon")
shp_path <- file.path(path_package, "shp/polygon.shp")

# Carregamento dos Dados
## Método 1: Download automático
mapas <- load_rasters(
  shape_path = ufscar_shp,
  start = 1985,
  end = 2021,
  method = "download",
  export_folder_path = "./meus-mapas/"
)

## Método 2: Importar coleção local
# mapas <- load_rasters(
#   shape_path = shp_path,
#   start = 1985,
#   end = 2021,
#   method = "library",
#   import_folder_path = "./minha-colecao/"
# )

# Exportação/Load de Objetos
save(mapas, file = "./meus-recortes/biomastats_ufscar_exemplo.Rdata")
load("./meus-recortes/biomastats_ufscar_exemplo.Rdata") # Para uso futuro

# Cálculo de Áreas por Classe
results <- get_area(mapas, plot_type = "areaplot")
head(results$aggregate_data) # Visualizar tabela de áreas
results$time # Gráfico temporal

# Visualização de Mapas Temáticos
land_vis(mapas, year = 1990) # Mapa para 1990
land_vis(mapas, year = 2020) # Mapa para 2020

# Distribuição de Classes
land_dist(results, year = 2020, type = "barplot") # Gráfico de barras
land_dist(results, year = 2020, type = "pie")     # Gráfico de pizza

# Métricas da Paisagem (interativo)
plot_teste <- biomastats_metrics(
  mapas,
  start = 1985,
  end = 2020,
  zone = "20",
  hemisphere = "south",
  metrics = "keep.all"
) # Abre janela interativa para seleção de classes

head(plot_teste$metrics_table) # Tabela de métricas
plot_teste$area_plot # Gráfico de áreas
plot_teste$ai_plot   # Gráfico de índice de agregação

# Personalização com ggplot2
library(ggplot2)
plot_teste$ai_plot +
  geom_point(color = "red") +
  theme_minimal() +
  theme(panel.grid = element_blank())

# Mapas Reclassificados
reclass_map(obj = plot_teste, year = 1985) # 1985
reclass_map(obj = plot_teste, year = 2020) # 2020