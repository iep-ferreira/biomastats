# biomastats (desenvolvimento)

## Mudanças de comportamento

* `reclass_to_polygons()` ganha `directions` (padrão 8): com `dissolve = TRUE`,
  pixels que se tocam só por um vértice formam um único fragmento, seguindo a
  recomendação do Conefor de dissolver polígonos contíguos. Fragmentos são
  sempre `MULTIPOLYGON`; `directions = 4` reproduz a vetorização anterior.
* IIC e CPL (`land_connectivity()`, `patch_imp_fast()`) passam a ligar
  fragmentos à distância zero. Antes, como no lconnect 0.1.2, fragmentos que
  se tocavam ficavam desconectados. `land_connectivity()` aceita `MULTIPOLYGON`.
* O IIC segue a definição de Pascual-Hortal & Saura (2006) e inclui os
  fragmentos isolados, que o lconnect 0.1.2 omite (`R/con_metric.R`, linhas
  188–194). `metrics$IIC` e `dIIC` mudam sempre que há isolados; o valor do
  lconnect fica em `metrics$IIC_lconnect`, e `iic_rule = "lconnect"` reproduz
  a priorização antiga.

## Novidades

* **PC** (Saura & Pascual-Hortal 2007) em `patch_imp_fast(metric = "PC")` e
  `land_connectivity(metric_patch = "PC")`: probabilidade máxima de caminho,
  que capta o papel de trampolim por caminhos indiretos. Arestas dominadas
  são podadas sem alterar o resultado.
* **Decomposição de Saura & Rubio (2010)**, método padrão de IIC e PC: a
  importância é dividida em intra, fluxo e conector, e o conector só é
  recalculado para os fragmentos que estão em algum melhor caminho. Na Lagoa
  do Sino (642 fragmentos), o IIC caiu de 5,4 para 0,18 s e o PC, de cerca de
  72 min para 54 s. `method = "removal"` mantém a remoção explícita.
* **Calibração de k** para PC e AWF: `k` ou `p_ref` e `d_ref`
  (`k = -log(p_ref) / d_ref`); o padrão continua p = 0,5 a `max_dist / 2`.
* Novas saídas de `land_connectivity()`: colunas `dX_intra`, `dX_flux` e
  `dX_connector` em `patch_metrics`, `fraction_metrics` (formato longo),
  `metrics$PC`, `metrics$IIC_lconnect` e `metrics$k`, e os gráficos
  `fractions_plot` (frações dos 15 fragmentos mais importantes) e
  `connector_plot` (mapa da fração de conector). `patch_imp_fast()` devolve
  `fractions` e os atributos `index`, `k`, `method` e `iic_rule`.

## Desempenho

* **Poda opcional do PC** (`prune = "auto"`, `TRUE` ou `FALSE`) em
  `patch_imp_fast()` e `land_connectivity()`. `"auto"` poda redes densas
  (ao menos 50 fragmentos e 25% dos pares com ligação finita); o resultado é o
  mesmo com ou sem poda. PC com 200 fragmentos: 2,8 s com poda e 12,3 s sem.
* **Paralelismo por blocos** (`parallel = FALSE`, `TRUE` ou número de
  processos): o teste de candidatos, o conector e a poda são divididos entre
  processos `future`. Um número abre um plano temporário e restaura o do
  usuário; `land_connectivity()` o abre uma vez para todos os limiares. PC com
  8 processos: Lagoa do Sino (461 fragmentos) de 26,8 para 5,8 s e grade de
  400 fragmentos de 21,2 para 4,8 s, com resultados idênticos. Em paisagens
  pequenas e no IIC, abrir os processos custa mais que o ganho, por isso o
  padrão continua sequencial.
* `method = "removal"` monta o grafo, podado ou não, uma única vez e apaga um
  vértice por remoção: o PC com 200 fragmentos caiu de 41 para 3 s.
* `metrics` ganha `pc_pruned` e `pc_edges`; `metadata`, `pc_prune` e
  `parallel_workers` (`schema_version` 1.2). Os objetos `pimp` ganham os
  atributos `pruned` e `edges`. `future` entra em Suggests.
* Testes de `tests/testthat/test-connectivity-engine.R` cobrem a poda
  (exata, opcional e automática) e o paralelismo (igual ao sequencial, plano
  do usuário restaurado, `land_connectivity()` em paralelo), com no máximo 2
  processos, e rodam no `R CMD check`.

# biomastats 2.1.0

Versão de funcionalidades: 18 funções exportadas novas, todas compatíveis com o
objeto de mapas de `load_rasters()`. Há duas mudanças incompatíveis com a
2.0.0: a remoção das funções de consulta ao Overpass e o novo significado de
`size` em `make_polygon()` (veja "Remoções" e "Correções").

## Novos módulos de análise

* **Séries temporais.** `land_time_series()` organiza as áreas anuais de
  `get_area()`. Sobre elas:
  * `series_correlation()` mede a associação entre duas séries em níveis,
    diferenças ou resíduos de tendência, com p-valor convencional e com
    correção para autocorrelação AR(1);
  * `series_fit_regime()` ajusta regimes segmentados, linear, bent-cable e
    bent-cable probabilístico (uniforme e logístico), com escolha automática
    por AICc/AIC/BIC, pontos de mudança e IC por bootstrap (resíduos ou blocos);
  * `series_diagnostics()` traz resíduos, autocorrelação e qualidade do ajuste;
  * `series_forecast()` faz a projeção com intervalo.
* **Diversidade das classes.** `class_diversity()` calcula, por ano, riqueza,
  Shannon e equitabilidade de Pielou, números de Hill (q = 0, 1, 2), Simpson,
  Berger-Parker e dominância. A entropia é decomposta por classe e pode ser
  expressa em nats, bits ou dits.
* **Fragmentos na grade nativa.** `fragment_metrics()` traz métricas por
  fragmento: área, perímetro, área núcleo, forma, dimensão fractal, centroide,
  raio de giração, alongamento, orientação e distância geodésica ao vizinho
  mais próximo. Por classe e ano, traz estatísticas distribucionais. Não
  reprojeta rasters e detecta zona UTM e hemisfério automaticamente.
* **Mudanças entre datas.** `land_change()` compara dois ou três momentos, com
  matrizes de transição, saldos por classe e diagramas bipartite e tripartite.
* **Adjacências.** `land_to_graph()` monta a matriz de adjacência entre classes
  e um grafo da paisagem, estático ou interativo.
* **Vetorização e conectividade.** `reclass_to_polygons()` converte os mapas
  reclassificados em polígonos `sf`, herdando o dicionário de classes.
  `land_connectivity()` calcula a conectividade dos fragmentos em vários
  limiares de distância. `patch_imp_fast()` reproduz `lconnect::patch_imp()`
  com cálculo vetorizado.

## Camadas externas (Brasil e mundo)

* `layer_catalog()` lista as fontes, a versão e o ano de catálogo de cada
  camada.
* `load_lulc()` lê o ESA WorldCover 10 m (2020 e 2021) em qualquer lugar do
  mundo e devolve um objeto igual ao de `load_rasters()`. O dicionário
  `dict_build("esa_worldcover")` inclui as cores oficiais.
* `load_features()` traz estradas, ferrovias, rios, corpos d'água, edificações
  e localidades. As fontes são os extratos OSM (Geofabrik ou openstreetmap.fr,
  com cache local) ou o Overture Maps, via índice STAC e GeoParquet.
* `load_physical()` traz altitude, declividade e orientação do Copernicus DEM
  30 m e propriedades do solo do SoilGrids 250 m, alinhadas à grade do mapa.
* `feature_raster()` gera superfícies de distância, contagem, comprimento,
  cobertura e densidade de feições na grade de análise.
* `distance_to_nearest()` mede a distância geodésica de coordenadas até a feição
  ou classe mais próxima.
* As fontes remotas falham rápido: uma fonte que não responde em 5 s (opção
  `biomastats.timeout`) ou recusa a requisição é ignorada com status
  `"skipped"`, e a análise continua.

## Alterações

* O exemplo `inst/examples/mapa_exemplo.Rdata` passou à Coleção 10 do
  MapBiomas (1985–2024). `raster_index()` usa a coleção 10 por padrão.
* `lulc_metrics()` e `biomastats_metrics()` calculam na grade nativa, sem
  reprojeção. Os argumentos `zone` e `hemisphere` ficam obsoletos e são
  ignorados.
* `biomastats_metrics()` documenta `export.raster`, usado por `reclass_map()`
  e `reclass_to_polygons()`.
* Resultados herdam dicionário de classes, cores e metadados entre as etapas e
  sobrevivem a `saveRDS()`/`readRDS()`.
* Gráficos novos têm legendas em português e inglês (`language = "pt"`/`"en"`).
* `dict_build()` não imprime mais a mensagem "Collection 10 subtitles are
  loaded as default" a cada chamada.

## Remoções

* Removido o fluxo de consultas ao OpenStreetMap pela API Overpass:
  `load_osm_data()`, `rasterize_osm()`, `distance_to_feature()`,
  `density_of_feature()` e `integrate_feature()`, além das opções
  `biomastats.osm_*`. Feições do OpenStreetMap continuam disponíveis por `load_features(source = "osm")`, com
  extratos regionais em cache. As superfícies de distância e densidade ficam
  com `feature_raster()`, e as distâncias de pontos com `distance_to_nearest()`.

## Correções

* `land_change()` aceita anos na mesma grade mesmo quando o `raster` gravou
  parte deles em disco durante `load_rasters()`. Isso ocorre em áreas de duas
  ou mais tiles com períodos longos e gera diferenças de ~1e-14 grau na
  extensão. Antes, a comparação exata recusava esses anos. A
  tolerância agora é de um milionésimo de célula.
* `land_to_graph()` deixa de emitir o aviso "Ignoring unknown labels" do
  ggplot2 em paisagens sem arestas.
* `land_change()`: a coluna `valid_area_ha` de `coverage` trazia o número de
  pixels válidos, e não a área em hectares, porque o `!` se aplicava ao produto
  `is.na(values) * area`. No mapa de exemplo (1985 e 2024), ela valia 15130 em
  vez de ~1242 ha.
* `land_change()` calcula a área de cada pixel com `terra::cellSize()`, como
  `get_area()`. Antes, polígonos de célula medidos com `sf::st_area()` davam
  áreas um pouco diferentes: no mapa de exemplo, formação florestal em 1985
  tinha 200,21 ha em `land_change()` e 199,74 ha em `get_area()`; agora as duas
  dão 199,74 ha. Em CRS projetado, a área da célula deixa de ser a nominal da
  grade e passa a corrigir a distorção da projeção, como em `get_area()` (uma
  célula de 10 m no meridiano central UTM tem 0,010008 ha). `metadata$area_method`
  registra o novo método, e o campo `metadata$geographic_s2` foi removido.
* `make_polygon()` gerava formas com tamanho e proporções dependentes da
  latitude: o tamanho era multiplicado por cos(lat) e usado como o mesmo raio
  em graus de latitude e de longitude. Um círculo com `size = 2` em
  Liechtenstein (latitude 47,16) media ~2,7 km N–S por ~1,8 km L–O. Agora os
  vértices são posicionados no elipsoide WGS84 com `geosphere::destPoint()`, e
  `size` é a largura total da forma, em km: diâmetro do círculo, lado do
  quadrado ou distância entre vértices opostos do hexágono, em qualquer
  latitude.
* **Mudança incompatível.** A mesma chamada de `make_polygon()` gera áreas
  diferentes das versões anteriores. Por exemplo, um hexágono com `size = 2.5`
  a 23,6° S tinha 12,5 km² e agora tem 4,1 km². Em `circle_coords()`,
  `square_coords()` e `hexagon_coords()`, o argumento `adj_size` (raio em
  graus) foi substituído por `size`, com o mesmo significado e unidade de
  `make_polygon()`.
* O exemplo `inst/examples/mapa_exemplo.Rdata` não muda. Ele foi criado com
  `make_polygon(lat = -23.605, lon = -48.529, size = 2.5, shape = "hexagon")`
  nas versões anteriores e continua com 12,465 km².

## Dependências

* Novas em Imports: `igraph (>= 1.6.0)`, `terra`, `units`, `curl`, `s2` e
  `methods`.
* Novas em Suggests: `lconnect`, `future.apply`, `osmextract`, `arrow`,
  `ggiraph`, `htmlwidgets`, `rmarkdown` e `withr`.
* Removidas de Imports, por não serem usadas: `graphics` e `readxl`.
* Removida de Imports: `osmdata`, junto com o fluxo de consultas ao Overpass.

## Documentação e testes

* Documentação gerada pelo roxygen2 com markdown. `man/` e `NAMESPACE` foram
  regenerados a partir dos comentários do código-fonte, e o CI confere essa
  correspondência.
* O GitHub Actions roda `R CMD check` em Linux (R release, oldrel-1 e
  oldrel-2), Windows e macOS, e falha em ERROR ou WARNING. Os testes
  automatizados (680 verificações) não usam internet.
* Testes manuais versionados em `testes-manuais/`: integrado, aquisição,
  método library, camadas externas e um relatório por módulo em
  `testes-manuais/modulos/`. Os relatórios de análise usam o território da
  Lagoa do Sino.
* Novo livro do usuário em bookdown, em português (`tutorial/`, publicado em
  <https://iep-ferreira.github.io/biomastats/>) e em inglês (`tutorial/en/`,
  em <https://iep-ferreira.github.io/biomastats/en/>), com botão para trocar de
  idioma em cada página. Traz instalação, um sumário
  das funções, as bases de dados e conexões, as duas vias de aquisição do
  MapBiomas, o fluxo completo e discutido da Lagoa do Sino e as camadas
  externas, inclusive um exemplo fora do Brasil. O tutorial anterior foi
  arquivado em `arquivo/tutorial-2.0/`.
* Licença unificada como GNU AGPL versão 3 ou posterior (`AGPL (>= 3)`) no
  DESCRIPTION, no README e no arquivo `LICENSE`. O `LICENSE` continha o texto
  da GPL-3 e passou a ter o texto oficial da AGPL-3.
* Testes sem prefixos redundantes (`biomastats::`, `testthat::`). A interface
  pública (41 funções exportadas) está listada em
  `tests/testthat/test-installation.R`.

# biomastats 2.0.0

* Integração de feições OSM: `load_osm_data()`, `rasterize_osm()`,
  `distance_to_feature()`, `density_of_feature()` e `integrate_feature()`.
* Aquisição seletiva de tiles da Coleção 10 por `load_rasters()`, com cache de
  manifestos (`fetch_tile_manifest()`).
