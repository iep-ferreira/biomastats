\name{reclass_to_polygons}
\alias{reclass_to_polygons}
\title{Vetoriza os rasters reclassificados em poligonos sf}
\usage{
reclass_to_polygons(obj, years = NULL, dissolve = TRUE,
  only_class = TRUE, crs = 4326, save_shp = TRUE,
  output_dir = "./source_codes/shp", file_prefix = "reclass_polygons")
}
\arguments{
\item{obj}{Lista de biomastats_metrics com rec_map, start, end e classes.
Use export.raster = TRUE ao calcular as metricas.}
\item{years}{Anos inteiros desejados; NULL seleciona todos.}
\item{dissolve}{Dissolver pixels adjacentes de mesma classe.}
\item{only_class}{Retornar somente a classe de interesse.}
\item{crs}{CRS de saida, por padrao EPSG 4326.}
\item{save_shp}{Salvar um shapefile por ano.}
\item{output_dir}{Pasta dos shapefiles.}
\item{file_prefix}{Prefixo dos arquivos.}
}
\value{Lista nomeada por ano ou, para um unico ano, objeto sf com value
(0/1), area_ha, year, fragment_id e geometry. O atributo shp_paths contem
os caminhos salvos. Anos sem pixels selecionados retornam sf vazio.}
\description{Converte os rasters de biomastats_metrics em poligonos,
opcionalmente dissolvendo os pixels adjacentes de mesma classe.}
\details{Codigos preservados pela reclassificacao de uma unica classe sao
convertidos para 1. NA e excluido. A conectividade usa arestas; fragmentos
que tocam somente em vertices permanecem separados, diferentemente das
metricas de paisagem com oito vizinhos. Areas sao convertidas explicitamente
para hectares. O CRS de entrada e respeitado antes da transformacao.
No shapefile, fragment_id e abreviado para frag_id pelo limite de 10 caracteres.}
