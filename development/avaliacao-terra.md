# Avaliação: substituição integral de `raster` por `terra`

Data: 2026-10-16. Escopo: código em `R/`, objetos públicos, exemplos e testes.

## Situação atual

- `raster` aparece em 15 arquivos (≈ 90 chamadas). As mais frequentes são
  `getValues`/`setValues` (24), `res`, `projection`, `extent`, `compareRaster`,
  `xFromCol`/`yFromRow` e `isLonLat`.
- Operações pesadas: `merge` (`check_maps`), `crop`/`mask` (`load_rasters`),
  `focal` (`density_of_feature`), `distance` (`distance_to_feature`),
  `rasterize`/`cover` (`rasterize_osm`) e `rasterToPoints` (gráficos).
- `terra` já é o motor dos módulos novos (`get_area`, `fragment_metrics`, a
  geometria nativa e a vetorização compartilhada). Os módulos novos convertem a
  entrada com `.as_terra()` e aceitam `RasterLayer` e `SpatRaster`.
- O objeto público de mapas (`load_rasters()$raster`, `rec_map` de
  `biomastats_metrics()`, `mapa_exemplo.Rdata`) contém `RasterLayer`.
- `NAMESPACE` ainda importa `raster::area` e `raster::projectRaster`, que não são
  mais usados depois da migração de `lulc_metrics()` para a grade nativa.

## Benefícios da migração integral

1. **Desempenho e memória.** `terra` é escrito em C++, processa em blocos e é,
   em geral, várias vezes mais rápido em `crop`, `mask`, `merge`, `focal`,
   `distance` e `rasterize`. Hoje o pacote paga conversões `raster ↔ terra` em
   cada função nova.
2. **Manutenção.** `raster` está em modo de manutenção, e suas versões recentes
   já delegam operações ao próprio `terra`. Remover a dependência reduz a árvore
   de pacotes e o tempo de instalação no CI.
3. **Coerência com a arquitetura.** Um único tipo de raster simplifica a regra
   "herdar primeiro": CRS, resolução e grade passam a ser lidos sempre da mesma
   forma, sem `.as_terra()`.
4. **Ecossistema.** `landscapemetrics`, `sf` e `ggplot2` (via
   `terra::as.data.frame(xy = TRUE)`) já aceitam `SpatRaster`.

## Riscos e pontos de atenção

1. **Serialização (crítico).** Um `SpatRaster` é um ponteiro para um objeto C++:
   `saveRDS()`/`load()` não preservam os dados. Isso afeta diretamente a regra
   de análises serializáveis e o exemplo `mapa_exemplo.Rdata`. Solução:
   guardar os rasters dos objetos públicos como `PackedSpatRaster`
   (`terra::wrap()`), com desempacotamento automático na entrada de cada função
   (`terra::unwrap()`/`terra::rast()`), e converter o `.Rdata` de exemplo.
2. **Compatibilidade da API.** Usuários que aplicam funções do `raster` sobre
   `mapas$raster[[i]]` deixariam de funcionar. Mitigação: ciclo de transição em
   que as funções aceitam ambos os tipos (já é o caso dos módulos novos) e um
   aviso de depreciação documentado.
3. **Diferenças sutis de semântica.** `terra::merge` (ordem de prioridade),
   `terra::mask` (valores de atualização), `terra::focal` (bordas e `na.rm`) e
   `terra::distance` (unidades em grades geográficas, já geodésicas no `terra`)
   precisam de testes de equivalência numérica antes da troca.
4. **Funções OSM.** `density_of_feature()` e `distance_to_feature()` usam
   reprojeção e operações focais; a migração é a oportunidade de aplicar também a
   regra da grade nativa (distâncias geodésicas do `terra`, sem reprojetar).

## Mapa de substituição

| `raster` | `terra` |
|:--|:--|
| `raster()` | `rast()` |
| `getValues()` / `setValues()` | `values()` / `values<-` ou `setValues()` |
| `res`, `extent`, `xmin`… | `res`, `ext`, `xmin`… |
| `projection()` / `crs()` | `crs()` |
| `isLonLat()` | `is.lonlat()` |
| `compareRaster()` | `compareGeom()` |
| `merge()` | `merge()` / `mosaic()` |
| `crop()` / `mask()` | `crop(mask = TRUE)` / `mask()` |
| `focal()` | `focal()` |
| `distance()` | `distance()` |
| `rasterize()` / `cover()` | `rasterize()` / `cover()` |
| `rasterToPoints()` | `as.data.frame(xy = TRUE)` |
| `projectRaster()` | `project()` (evitar: regra da grade nativa) |

## Esforço estimado

| Grupo | Arquivos | Esforço |
|:--|:--|:--|
| Carga de dados | `check_maps`, `load_rasters`, `load_rasters_from_folder` | baixo |
| Visualização | `reclass_map`, `land_vis` | baixo |
| Métricas e análise | `biomastats_metrics`, `lulc_metrics`, `land_change`, `land_to_graph`, `reclass_to_polygons` | baixo a médio |
| OSM | `load_osm_data`, `rasterize_osm`, `density_of_feature`, `distance_to_feature`, `integrate_feature` | médio |
| Serialização | `wrap/unwrap` nos objetos públicos, `.Rdata` de exemplo, testes de `saveRDS` | médio |

## Recomendação

Migrar integralmente, em três etapas, sem quebrar usuários:

1. **Interna.** Trocar todas as chamadas `raster::` por `terra::` dentro das
   funções, aceitando `RasterLayer` e `SpatRaster` na entrada (`.as_terra()`).
   Remover importações mortas (`raster::area`, `raster::projectRaster`).
2. **Objetos públicos.** `load_rasters()` e `biomastats_metrics()` passam a
   devolver rasters empacotados (`terra::wrap`), garantindo `saveRDS/readRDS`.
   Converter `mapa_exemplo.Rdata`. Testes de equivalência numérica com a versão
   anterior em cada função.
3. **Remoção.** Retirar `raster` de `Imports` após um ciclo de versão com aviso
   de depreciação para quem usa funções do `raster` sobre os objetos do pacote.

A etapa 1 não muda a API e pode ser feita imediatamente; a etapa 2 é a que
exige decisão, porque altera o tipo dos objetos devolvidos.
