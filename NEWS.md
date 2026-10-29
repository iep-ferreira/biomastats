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

## Integração com o Makurhini e análise temporal

* `land_connectivity(distance = )` aceita distâncias externas: matriz (com
  nomes iguais a `fragment_id` ou na ordem dos fragmentos), `dist` ou tabela
  `From`/`To`/`Distance`, como a de `Makurhini::distancefile()`. `NA` e `Inf`
  são barreiras. Unidade e origem ficam em `metadata`.
* **ECA** nativa em `metrics`: `ECA_IIC` e `ECA_PC` (m²), `ECAnorm_IIC` e
  `ECAnorm_PC` (% do habitat), e as **frações da paisagem** do Makurhini,
  `X_intra`, `X_direct` e `X_step` (% do numerador, para IIC e PC). Com o PC,
  `metrics` traz `Prot`, `ProtConn`, `ProtUnconn` e `RelConn` (Saura et al.
  2017), tomando o habitat como área protegida e o limite como região.
  ECA, frações, dPC e dIIC coincidem com `MK_dPCIIC(overall = TRUE)`, e os
  indicadores, com `MK_ProtConn()`, nos mesmos dados.
* **Importância das ligações** (`link_importance = TRUE`): `link_metrics` dá a
  queda do IIC e do PC quando cada ligação é removida, recalculando só os pares
  que passam por ela, em blocos paralelizáveis; `link_plot` a mapeia. Na Lagoa
  do Sino (461 fragmentos, 300 m), IIC e PC com ligações levam 47 s.
* `resistance_surface()`: superfície de custo a partir do uso do solo de um
  ano, com custo por classe, custo padrão e barreiras `sf` (intransponíveis ou
  com custo próprio). Devolve um `RasterLayer` serializável, aceito pelo
  Makurhini e pelo gdistance.
* `cost_distance()`: distância de menor custo **de borda a borda** em grafo de
  células com vizinhança de 8, sem atalhos por fragmentos nem por cantos de
  barreiras; com custo 1, reproduz a distância entre bordas na grade.
  `method = "makurhini"` delega a `Makurhini::distancefile()` (centroides). Na
  Lagoa do Sino (315 mil células, 461 fragmentos), 64 s, ou 42 s com 4
  processos.
* `connectivity_change()`: série temporal de habitat, ECA, dA, dECA, rECA e
  tipo de mudança (classes do `MK_dECA()`), frações da paisagem por ano,
  resumo da participação das ligações e os gráficos `eca_plot`, `change_plot`,
  `fractions_plot`, `map_plot` e `link_plot`. Com `animate = TRUE`, devolve uma
  animação gganimate do mapa de importância ano a ano. `gganimate` entra em
  Suggests.
* `as_makurhini()`: argumentos prontos para `MK_dPCIIC()`, `MK_dECA()` e
  `MK_ProtConn()` (nós com área em m², limite como `LA` ou região, matriz de
  distâncias, `probability` e `distance_thresholds` equivalentes ao k,
  `pij_min = 0`) e, com `run = TRUE`, os resultados. O Makurhini, instalado do
  GitHub, continua opcional e fora de Suggests.
* `metadata$schema_version` passa a 1.3.

## Contexto espacial ampliado e sensibilidade ao efeito de borda

* `load_rasters()` aceita a área de estudo como `sf`/`sfc`, além do caminho
  do shapefile, e ganha `context_m`: um buffer (em metros, na UTM local) que
  amplia recortes, corte e máscara. A área original fica em `focal`, e `source`
  registra método, pastas, tipo e coleção. `compare_shapefiles()` também
  aceita `sf`. `biomastats_metrics()` e `reclass_to_polygons()` repassam
  `focal` junto com `boundary`.
* `land_connectivity(focal = )` (ou o atributo `focal` herdado): conectividade
  do habitat da área focal com o contexto como trampolim, como as áreas
  transfronteiriças do ProtConn. Cada fragmento entra com sua área dentro da
  área focal (`focal_area_m2`; zero fora dela), as distâncias usam o fragmento
  inteiro e A_L passa a ser a área focal. `patch_metrics$focal_role` separa
  fragmentos focais, de borda e de contexto; os do contexto só têm importância
  de conector. Sem `focal`, nada muda. `schema_version` 1.4.
* `connectivity_context()`: sensibilidade dos índices e da importância ao raio
  do contexto. A partir dos mapas da área de estudo e das classes, aplica o
  buffer do maior raio, recarrega o território maior com a mesma origem (ou o
  recorta de `source`, ou reaproveita mapas já carregados com contexto),
  reclassifica e vetoriza uma única vez, sem gravar arquivos, e reaproveita a
  matriz de distâncias (ou a de menor custo) do maior contexto nos menores.
  O raio 0 reproduz a análise sem contexto. Devolve `sensitivity` (ECA,
  ProtConn focal e variação em relação ao maior raio, com indicador de
  convergência), `patch_sensitivity`, `rank_stability` (Spearman e
  sobreposição dos 10 maiores), `stepping_stones` (trampolins de fora da área)
  e os gráficos `sensitivity_plot`, `rank_plot` e `map_plot`. O fluxo é
  opcional.
* `connectivity_context(targets = )`: importância de um fragmento-alvo ou de
  um conjunto frente ao contexto, por **remoção conjunta**. Alvos são
  `fragment_id`, listas nomeadas de ids ou geometrias `sf` (agrupadas pela
  coluna `target`), inclusive fora da área focal, para medir trampolins
  externos. `target_sensitivity` traz, por raio, a importância conjunta, a soma
  das importâncias individuais e a interação (positiva quando os fragmentos se
  substituem, negativa quando dividem a mesma contribuição), com
  `target_plot`. `stepping_stones` passa a ser relatado por raio.
* O contexto funciona com **qualquer coleção portada**, inclusive fora do
  Brasil: `load_lulc()` (ESA WorldCover) ganha `context_m`, `focal` e `source`,
  e a recarga automática usa o carregador registrado. `source` também aceita
  uma função `function(area, years)` que devolve mapas no formato de
  `load_rasters()`, para outras fontes. Coleções sem dicionário em
  `dict_build()` funcionam com seus códigos numéricos, também em
  `resistance_surface()`.
* Testes com mapas sintéticos (`tests/testthat/test-connectivity-context.R`)
  cobrem o buffer de `load_rasters()` (modo `library`, sem download), a
  herança de `focal`, os pesos focais conferidos à mão, o ganho de um
  trampolim fora da área, a monotonia dos índices, a igualdade entre
  distâncias reaproveitadas e análise direta, a recarga automática e a
  distância de menor custo.

## Tutorial e rótulos

* Tutorial (português e inglês) reorganizado em quatro vias de aquisição
  (recortes, coleção nacional, leitura por HTTP com o catálogo e pasta local),
  com área de estudo em `.gpkg` ou `.shp`, dicionário de classes, contexto
  ampliado na aquisição e, no capítulo de conectividade, a análise de
  sensibilidade ao contexto na Lagoa do Sino até 10 km.
* `biomastats_metrics()` ganha `language` ("pt", padrão, ou "en"): os seis
  gráficos tinham rótulos fixos em inglês. Os nomes das colunas de
  `metrics_table` continuam em inglês.
* Tutorial: conectividade dividida em quatro capítulos (vetorização e índices;
  resistência e ligações; conectividade ao longo do tempo; efeito de borda e
  contexto ampliado, com figuras pré-calculadas por
  `development/gerar_figuras_contexto.R`). Capítulos de diversidade, mudanças e
  paisagem passam a mostrar números de Hill e dominância, saldo por classe e
  trajetórias, matriz de adjacências, interfaces entre classes, métricas da
  rede e das classes, grafo interativo, os gráficos de área e borda de
  `biomastats_metrics()`, distribuição de tamanhos, curva posição-tamanho,
  forma e tendências dos fragmentos e as métricas do `landscapemetrics`.
* `land_connectivity()` não falha mais ao construir os gráficos quando nenhum
  par de fragmentos está ligado. Em `range_plot`, os painéis de componentes e
  fragmentos isolados têm títulos em português e inglês.

## Aquisição seletiva por HTTP e catálogo de mapas rasterizados

* `load_rasters(method = "http")`: lê, pelo sistema de arquivos virtual do
  GDAL (`/vsicurl/`), apenas a janela da área de estudo em GeoTIFFs públicos,
  sem login, a partir de um produto de `raster_catalog()` (`product`) ou de
  qualquer modelo de URL com `{year}` (`url`). Usa a mesma rotina de
  `load_local_rasters()` (conferência, harmonização, recorte e máscara) e, com
  `export_folder_path`, grava as janelas como `<produto>_<ano>.tif` e a área
  como `area.gpkg`, prontas para `load_local_rasters()`. A recarga do contexto
  em `connectivity_context()` usa a mesma origem.
* `raster_catalog()`: 18 coleções anuais de uso do solo do bucket público do
  MapBiomas, verificadas em 29/10/2026: Brasil (Coleções 9, 10 e 11, 30 m, e
  10 m Coleção 4 beta), Argentina, Bolívia, Chile, Colômbia, Equador,
  Paraguai, Peru, Uruguai, Venezuela, Indonésia, Amazônia, Chaco, Pampa e
  Bosque Atlântico, com resolução, anos, modelo de URL e dicionário
  compatível. Todos os arquivos são divididos em blocos (256 ou 512 pixels).
* A área de estudo é detectada como GeoPackage, shapefile ou GeoJSON, com ou
  sem extensão, ou pela pasta (um único arquivo de área); com `.gpkg` e `.shp`
  de mesmo nome, vale o `.gpkg`. Um shapefile sem `.prj` gera erro explícito,
  em vez de CRS ausente. Vale para `load_rasters()`, `load_lulc()`,
  `load_local_rasters()` e `compare_shapefiles()`.
* `load_local_rasters()` (e a via HTTP) recusam mapas que cubram menos de
  99% da área de estudo, como janelas baixadas para outra área; o buffer de
  contexto pode ultrapassar o limite dos dados.
* Novo teste manual `testes-manuais/teste-manual-http.Rmd` (HTML): aquisição
  por HTTP para Itapetininga - SP, conferida célula a célula com o recorte
  direto do GeoTIFF nacional, e portabilidade para Campina do Monte Alegre -
  SP (recusa da janela parcial, gravação, releitura local e coincidência com
  as janelas antigas na sobreposição).

## Coleções locais de qualquer origem

* `load_local_rasters()`: carrega GeoTIFFs anuais de uma pasta de caminho
  conhecido, nomeados como `prefixo_ano.tif`, de qualquer coleção ou país.
  Confere compatibilidade (uma camada, CRS, códigos inteiros, cobertura da
  área, anos consecutivos), harmoniza todos os anos pela grade do primeiro (ou
  por `crs` e `resolution`) com o vizinho mais próximo, recorta só a janela da
  área, mascara pelo centro das células, trata `nodata` (0 por padrão) e
  devolve o objeto de `load_rasters()`, com a ação de harmonização de cada ano
  em `metadata$harmonization`. Aceita `context_m` e registra a origem, de modo
  que `connectivity_context()` recarrega a área ampliada da mesma pasta.
* `class_dictionary()`: assistente opcional do dicionário de classes. Sem
  arquivo, devolve um modelo com os códigos presentes nos mapas (ou a base de
  uma coleção conhecida) para preencher; com CSV ou data.frame, reconhece
  colunas em português ou inglês, completa nomes e cores, converte cores para
  hexadecimal e avisa sobre códigos sem entrada.
* O dicionário viaja no objeto de mapas (`class_dictionary`) e é usado por
  `get_area()`, `land_vis()`, `land_dist()`, `land_change()`,
  `land_to_graph()`, `fragment_metrics()`, `biomastats_metrics()`,
  `resistance_surface()` e `connectivity_context()`. Coleções sem dicionário
  recebem nomes genéricos ("Classe 3") em vez de erro. `land_vis()` aceita os
  anos do próprio objeto, não só 1985–2024.
* Dados sintéticos leves para testes em `inst/extdata/local_maps` (cerca de
  4 KB: três anos com grades diferentes, área e legenda), gerados por
  `development/make_local_fixtures.R`. Um teste local,
  `tests/testthat/test-local-mapbiomas-10m.R` (fora do pacote construído), usa
  uma coleção MapBiomas 10 m em `~/Documents/mapbiomas_10m_col4`: janelas da
  Coleção 4 beta (2017–2025) lidas por `/vsicurl` do bucket público, sem login.

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
