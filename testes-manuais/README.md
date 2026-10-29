# Testes manuais do biomastats

Quatro relatórios R Markdown que conferem o pacote de ponta a ponta e sete
relatórios por módulo (pasta `modulos/`), todos gerando um HTML para inspeção
visual. Complementam os testes automatizados (`tests/testthat`), que rodam no
GitHub Actions em Linux, Windows e macOS; estes rodam só na sua máquina, porque
dependem de downloads, da coleção nacional ou de inspeção visual.

Áreas de teste: as funcionalidades são conferidas em áreas pequenas, a Lagoa do
Sino (exemplo do pacote) e Liechtenstein (camadas fora do Brasil). Itapetininga
(SP) aparece só no teste de aquisição, porque é o caso que exige duas tiles do
MapBiomas.

| Teste | O que confere | Internet | Tempo aproximado |
|:--|:--|:--|:--|
| `teste-manual-integrado.Rmd` | Todas as funções de análise sobre o exemplo da Lagoa do Sino (Coleção 10, 1985–2024): mapas, áreas, diversidade, séries temporais, mudanças, adjacências, métricas da paisagem, fragmentos e conectividade, com tabelas dinâmicas e o tempo de cada função | não | 1 a 2 min |
| `teste-manual-aquisicao.Rmd` | `load_rasters()` com aquisição seletiva de tiles, sem cache: Lagoa do Sino (1 tile) e Itapetininga - SP (2 tiles) | sim | 3 a 5 min |
| `teste-manual-biblioteca.Rmd` | `load_rasters(method = "library")`: recorte a partir da coleção nacional já gravada no computador (`collection_10`, 1985–2024), com o tempo de cada etapa, para a Lagoa do Sino; confere que o recorte coincide com o exemplo do pacote | não (exige a coleção local, ~28 GB) | menos de 1 min |
| `teste-manual-camadas.Rmd` | Camadas externas gratuitas no mesmo limite das análises, no Brasil (Lagoa do Sino) e no mundo (Liechtenstein): ESA WorldCover por `load_lulc()`, feições OSM e Overture por `load_features()`, relevo e solos por `load_physical()`, superfícies por `feature_raster()`, distâncias por `distance_to_nearest()`, ano de catálogo de cada fonte e simulação de fontes fora do ar | sim | 2 a 3 min |
| `teste-manual-http.Rmd` | `load_rasters(method = "http")` e `raster_catalog()`: aquisição seletiva da janela de Itapetininga - SP (Coleção 10, 30 m) e releitura das janelas gravadas; portabilidade para Campina do Monte Alegre - SP (recusa dos mapas de 10 m que não cobrem o município, gravação por HTTP e releitura por `load_local_rasters()`); Lagoa do Sino igual pela pasta antiga e por HTTP; erro de shapefile sem `.prj` | sim | 1 a 2 min |

### Testes por módulo (`modulos/`)

Cada relatório aprofunda um módulo sobre o exemplo da Lagoa do Sino, sem
internet, em 25 a 50 s:

| Teste | Módulo |
|:--|:--|
| `teste-modulo-poligonos.Rmd` | `biomastats_metrics()` → `reclass_to_polygons()`: polígonos de floresta, visualização e objetos reutilizáveis |
| `teste-modulo-adjacencias.Rmd` | `land_to_graph()`: matriz de adjacências, grafo interativo, métricas exploratórias e fronteiras |
| `teste-modulo-mudancas.Rmd` | `land_change()`: transições entre dois e três momentos, métricas e saldos por classe |
| `teste-modulo-conectividade.Rmd` | `land_connectivity()` e `patch_imp_fast()`: cenários de distância, componentes, índices e priorização, comparados ao `lconnect` |
| `teste-modulo-series-temporais.Rmd` | `land_time_series()`, `series_correlation()`, `series_fit_regime()`, `series_diagnostics()` e `series_forecast()` |
| `teste-modulo-diversidade.Rmd` | `class_diversity()`: índices ao longo do tempo, números de Hill e decomposição da entropia por classe |
| `teste-modulo-fragmentos.Rmd` | `fragment_metrics()`: fragmentos individuais e distribuições por classe na grade nativa |

Os módulos são as versões atualizadas dos antigos testes locais em
`ignorados/testes-locais/`. Os demais testes locais antigos foram absorvidos:
`teste-local-integrado` e `teste-local-rapido` pelo integrado,
`teste-local-load-rasters` pelo de aquisição. Os relatórios de análise usam o
território da Lagoa do Sino (exemplo do pacote), sem downloads.

## Antes de rodar

1. Abra o R na raiz do repositório (pasta com `DESCRIPTION`).
2. Instale as dependências do pacote e, para estes relatórios, `rmarkdown`,
   `knitr` e `DT` (tabelas dinâmicas). O teste de aquisição também usa `geobr`.

```r
install.packages(c("rmarkdown", "knitr", "DT", "geobr", "osmextract", "arrow"))
```

Não é preciso instalar o `biomastats`: cada relatório constrói o pacote a partir
do código-fonte atual em uma biblioteca temporária.

## Como rodar

```r
rmarkdown::render("testes-manuais/teste-manual-integrado.Rmd")
rmarkdown::render("testes-manuais/teste-manual-aquisicao.Rmd")
rmarkdown::render("testes-manuais/teste-manual-biblioteca.Rmd")
rmarkdown::render("testes-manuais/teste-manual-camadas.Rmd")
rmarkdown::render("testes-manuais/teste-manual-http.Rmd")

# todos os módulos
for (f in list.files("testes-manuais/modulos", "[.]Rmd$", full.names = TRUE))
  rmarkdown::render(f)
```

O HTML é criado ao lado do `.Rmd`, com o mesmo nome, e fica fora do Git.
Arquivos intermediários vão para uma pasta temporária, apagada ao final.

No teste de aquisição, o período padrão é 2015–2024. Para outro período:

```r
rmarkdown::render("testes-manuais/teste-manual-aquisicao.Rmd",
                  params = list(inicio = 1985, fim = 2024))
```

## Como ler o resultado

**Integrado** — a renderização termina sem erro e:

- os gráficos usam as cores do dicionário de classes e legendas legíveis;
- as tabelas dinâmicas de fragmentos e conectividade aceitam filtro por classe,
  ano e faixa de valores;
- a seção "Tempo de computação" mostra cada função; nenhuma deve passar de
  alguns segundos, exceto o ajuste de regime com bootstrap.

**Aquisição** — a renderização para com erro se qualquer verificação falhar
(`stopifnot`). Se terminar, confira na tabela "Verificações":

- tiles: 1 na Lagoa do Sino (93) e 2 em Itapetininga (93 e 94);
- `arquivos` igual a `esperado` (tiles × anos) e todas as tiles baixadas;
- `grade_unica_entre_anos` e `identico_a_referencia` iguais a 1;
- área classificada próxima à área do limite (diferença < 1% em Itapetininga).

## Quando rodar

- Integrado: depois de alterar qualquer função de análise ou gráfico.
- Aquisição: depois de alterar `load_rasters()`, `check_maps()`,
  `download_maps()`, `compare_shapefiles()` ou a API de tiles.
- Camadas: depois de alterar `load_lulc()`, `load_features()`,
  `load_physical()`, `feature_raster()` ou `distance_to_nearest()`.
- HTTP: depois de alterar `load_rasters(method = "http")`, `raster_catalog()`,
  `load_local_rasters()` ou a leitura da área de estudo.
- Módulos: depois de alterar a função do módulo correspondente.

## Problemas comuns

- **Erro ao baixar tiles ou manifestos:** a API pode estar temporariamente
  indisponível; tente de novo mais tarde.
- **Erro em `geobr::read_municipality`:** o serviço do IBGE/geobr não
  respondeu; tente de novo.
- **Acentos trocados no HTML (Windows):** rode antes
  `Sys.setlocale("LC_CTYPE", ".UTF-8")`.
- **Falha na construção do pacote:** rode `devtools::document()` e os testes
  automatizados (`devtools::test()`) para localizar o erro.

## Teste do método library

O relatório procura a pasta `collection_10` (arquivos
`brasil_coverage_<ano>.tif`) na pasta pessoal e em `Documentos`. Para outro
local ou período:

```r
rmarkdown::render("testes-manuais/teste-manual-biblioteca.Rmd",
                  params = list(pasta_colecao = "D:/mapbiomas/collection_10",
                                inicio = 2000, fim = 2024))
```

A renderização para com erro se o recorte do método library divergir do exemplo
do pacote (gerado por download de tiles com o mesmo limite). A tabela "Tempo de
cada etapa" separa leitura do limite, abertura dos mapas nacionais, recorte,
máscara, chamada completa de `load_rasters()` e `get_area()`.

## Teste de camadas externas

Confere as fontes de `layer_catalog()` com dados reais. Na tabela de status de
cada bloco, `ok` indica dados carregados; `skipped`, uma fonte que não
respondeu em 5 s ou falhou (o relatório continua sem ela). A seção "Fontes
indisponíveis" simula servidores fora do ar e deve mostrar todos os blocos
como `skipped`.

Parâmetros:

- `cache_novo` (padrão `TRUE`): baixa extratos OSM e índices do Overture em
  uma pasta temporária; use `FALSE` para reaproveitar o cache do pacote.
- `osm_brasil` (padrão `FALSE`): inclui o extrato OSM do estado de São Paulo
  para a Lagoa do Sino (cerca de 365 MB no primeiro uso).

```r
rmarkdown::render("testes-manuais/teste-manual-camadas.Rmd",
                  params = list(cache_novo = FALSE, osm_brasil = TRUE))
```

## Teste da via HTTP

Usa a malha municipal do IBGE (sem login) e o bucket público do MapBiomas.
A parte de portabilidade lê a pasta de 10 m em `~/Documents/mapbiomas_10m_col4`
(janelas da Lagoa do Sino + 5 km). Para outra pasta, produto ou período:

```r
rmarkdown::render("testes-manuais/teste-manual-http.Rmd",
  params = list(pasta_10m = "D:/mapas/mapbiomas_10m", anos_campina = 2023:2025))
```

A renderização para com erro se qualquer verificação falhar. Fora do RStudio, o
`rmarkdown` precisa do pandoc: defina `RSTUDIO_PANDOC` com a pasta do pandoc
(no Windows, `C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools`).
