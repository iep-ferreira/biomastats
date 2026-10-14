# Arquitetura e vetorizacao

O projeto e um pacote R: `R/` implementa as funcoes, `NAMESPACE` publica a
API, `man/` documenta os contratos e `tests/testthat/` valida o comportamento.
`inst/` distribui mapas de exemplo, dicionarios e shapefiles.

O fluxo principal baixa/carrega mapas (`download_*`, `load_rasters*`), calcula
areas e visualizacoes (`get_area`, `land_vis`, `land_dist`) e analisa a paisagem
com `biomastats_metrics`. Esta ultima organiza os anos e chama `lulc_metrics`
para cada raster. `lulc_metrics` seleciona classes, projeta uma copia para UTM
para as metricas e devolve o raster reclassificado no CRS original.
`biomastats_metrics` agrupa esses rasters em `rec_map`, na ordem start:end.
`reclass_map` apenas desenha esse resultado; nao realiza a reclassificacao.

Para uma classe, `lulc_metrics` preserva seu codigo (por exemplo, floresta = 3);
para varias classes, substitui os codigos selecionados por 1. A nova funcao
`reclass_to_polygons` aceita ambos os contratos e normaliza a presenca para 1
sem alterar o objeto original. Usa terra para poligonizar e sf para geometria,
reprojecao e escrita. units garante hectares mesmo quando o CRS usa pes.
Os fragmentos usam conectividade por aresta; as metricas existentes usam oito
vizinhos, portanto a contagem pode diferir para pixels em contato diagonal.

O ramo OSM (`load_osm_data`, `rasterize_osm`, `distance_to_feature`,
`density_of_feature`, `integrate_feature`) e independente dessa vetorizacao.
Os testes desse ramo foram comentados a pedido, incluindo os testes com mocks,
assim como a consulta real no roteiro manual. Nenhuma consulta externa e
necessaria para testar ou executar o exemplo de floresta.

Execute `devtools::test(filter = "reclass-to-polygons")` para testar.
O exemplo executavel de build, analise e visualizacao esta em
`ignorados/testes-locais/teste-local-poligonos.Rmd`; ele e mantido fora do
pacote porque serve como verificacao local e nao como interface distribuida.

Validacao em 14/10/2026: `testthat::test_local(".")` passou com 90 verificacoes,
sem falhas, avisos ou testes pulados (o grupo OSM esta comentado). Dessas,
38 verificam vetorizacao, topologia, area, selecao temporal, entradas invalidas,
shapefiles e recuperacao dos pixels originais de floresta por rasterizacao.
O exemplo executado gerou 26 fragmentos e 99,81147 hectares em 1985.
