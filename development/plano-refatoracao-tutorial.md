# Plano de refatoração do tutorial

> **Executado em 2026-10-18.** O tutorial foi reconstruído como livro bookdown
> em `tutorial/`, publicado em `docs/`, com a identidade visual descrita em
> `development/identidade-visual-livro.md`. O tutorial anterior está
> preservado na tag `v2.0.0` (`tutorial.Rmd` na raiz). Este plano fica como registro.

Data: 2026-10-16. Base: `tutorial.Rmd` (615 linhas, bilíngue pt/en via `b()`,
`bh()` e `bli()`, publicado em `docs/` por `build_docs.R`) e o teste local
integrado (hoje `testes-manuais/teste-manual-integrado.Rmd`), que executa todas
as funções analíticas sobre os dados de exemplo da Lagoa do Sino em ~45 s.

## Diagnóstico do tutorial atual

1. **Não é reproduzível fora do computador do autor.** O carregamento usa
   `load_rasters(method = "library")` com `import_folder_path` apontando para
   `C:/Users/Pichau/Documents/collection_10`, executado (`eval = TRUE`). Sem a
   coleção nacional nesse caminho, a renderização falha.
2. **Conteúdo desatualizado.**
   - `biomastats_metrics()` ainda é apresentada com `zone` e `hemisphere`
     obrigatórios; hoje as métricas são calculadas na grade nativa e esses
     argumentos são ignorados.
   - O texto afirma que `classes = NULL` abre uma janela seletora
     (`raster_index`), recurso interativo que não combina com renderização.
   - `dict_build` é chamado com `biomastats:::`, mas já é exportado.
   - A aquisição seletiva por tiles (`load_rasters(method = "download")`),
     validada no teste de Lagoa do Sino e Itapetininga, não aparece; só o
     download da coleção completa.
3. **Módulos novos ausentes:** `class_diversity`, `land_time_series` e
   `series_*`, `land_change`, `land_to_graph`, `fragment_metrics`,
   `reclass_to_polygons`, `land_connectivity` e `patch_imp_fast`.
4. **`make_polygon` grava dentro da biblioteca instalada**
   (`<biblioteca>/biomastats/shp/polygon.shp`), sobrescrevendo o shapefile de
   exemplo. Convém documentar ou oferecer destino configurável.
5. **O insumo de exemplo existe e não é usado na renderização.**
   `inst/examples/mapa_exemplo.Rdata` contém o objeto `mapas` da Lagoa do Sino,
   carregável com `load()`, exatamente o fluxo da seção "Exportação e leitura de
   objetos do Bioma Stats". Hoje essa seção é apenas ilustrativa
   (`eval = FALSE`, arquivo local `meu_recorte_ufscar.Rdata`).
   *Atualizado em 2026-10-16:* o exemplo foi refeito com `load_rasters()`
   (versão com `raster`) a partir da Coleção 10, 1985–2024, mesmo limite
   hexagonal e mesma grade (132 × 152), já com `collection = "10"`. O exemplo
   anterior era da Coleção 7 (1985–2021).
6. **Rótulos só em inglês** nos gráficos de `get_area()`, `land_vis()` e
   `land_dist()` ("Land use", "Area (km2)"), ao contrário dos módulos novos,
   que aceitam `language = c("pt", "en")`.

## Princípios da nova versão

- **Reproduzível em qualquer máquina:** toda a parte analítica avaliada sobre
  dados de exemplo distribuídos com o pacote; etapas de rede (download) mostradas
  com `eval = FALSE` ou executadas apenas se uma opção de renderização permitir.
- **Fluxo único e encadeado:** cada seção recebe o objeto da anterior, como no
  teste integrado, ilustrando a interoperabilidade (dicionário, cores, unidades
  e metadados herdados).
- **Foco em gráficos e métricas:** mensagens ocultas, código curto, uma figura
  ou tabela por conceito, legendas pt/en via `language`.
- **Bilíngue preservado:** manter `b()`/`bh()`; cada parágrafo novo ganha as
  duas versões.
- **Tempo de renderização controlado:** meta de até ~1 min de execução (o teste
  integrado já roda em ~45 s, incluindo o build do pacote).

## Estrutura proposta

| # | Seção | Funções | Origem do conteúdo |
|:--|:--|:--|:--|
| 1 | Sobre o Bioma Stats e funcionalidades | — | manter, atualizar os cartões |
| 2 | Instalação | — | manter |
| 3 | Definição do recorte | `make_polygon`, shapefiles do pacote | manter, nota sobre onde o shp é gravado |
| 4 | Aquisição dos dados | `load_rasters` (download seletivo e biblioteca local), `compare_shapefiles` | reescrever; `eval = FALSE` + mapa estático das tiles |
| 5 | Exportação e leitura de objetos | `save`/`load` | manter o fluxo atual, agora executado com `mapa_exemplo.Rdata` |
| 6 | Áreas e mapas | `get_area`, `land_vis`, `land_dist` | manter, com o RDS |
| 7 | Diversidade e entropia | `class_diversity` | novo (teste integrado) |
| 8 | Séries temporais | `land_time_series`, `series_correlation`, `series_fit_regime`, `series_diagnostics`, `series_forecast` | novo |
| 9 | Mudanças entre datas | `land_change` | novo |
| 10 | Adjacências entre classes | `land_to_graph` | novo |
| 11 | Métricas da paisagem e reclassificação | `biomastats_metrics`, `lulc_metrics`, `reclass_map`, `dict_build` | atualizar (sem `zone`/`hemisphere`) |
| 12 | Fragmentos | `fragment_metrics` | novo |
| 13 | Vetorização e conectividade | `reclass_to_polygons`, `land_connectivity`, `patch_imp_fast` | novo |
| 14 | Integração com OpenStreetMap | `integrate_feature` | manter estático (imagem), sem chamada à API |
| 15 | Customização e idioma dos gráficos | `ggplot2`, `language = "en"` | ampliar a seção atual |
| 16 | Contribua, agradecimentos, termos, uso de IA | — | manter |

## Etapas

1. **Dados de exemplo.** Executar a seção "Exportação e leitura de objetos" com
   `load(system.file("examples", "mapa_exemplo.Rdata", package = "biomastats"))`
   e usar o objeto `mapas` em todo o restante do tutorial, como já faz o teste
   integrado. O mesmo `.Rdata` alimenta os testes automatizados.
2. **Correções do conteúdo existente** (seções 4, 11 e chamadas internas):
   remover `zone`/`hemisphere`, trocar `biomastats:::dict_build` por
   `dict_build`, passar `classes` explicitamente, substituir o caminho local da
   coleção.
3. **Novas seções 7–13**, reaproveitando o código do teste integrado, com um
   parágrafo explicativo pt/en por figura e as tabelas principais.
4. **Revisão do texto bilíngue** e dos cartões de funcionalidades
   (diversidade, séries, mudanças, fragmentos, conectividade).
5. **Renderização e verificação:** `build_docs.R` em sessão limpa, sem acesso à
   coleção nacional; conferir ausência de avisos, tempo total e links.
6. **Publicação** em `docs/` (GitHub Pages) e atualização da data no cabeçalho.

## Decisões pendentes

- ~~`collection` e período do exemplo~~: resolvido; exemplo refeito na
  Coleção 10, 1985–2024, com `collection = "10"`.
- Idioma dos gráficos de `get_area()`, `land_vis()` e `land_dist()`: acrescentar
  `language` (como nos módulos novos) antes de refatorar o tutorial, ou manter
  em inglês.
- Download no tutorial: apenas ilustrativo (`eval = FALSE`, recomendado) ou
  executado opcionalmente por parâmetro de renderização.
- Formato: página única (atual) ou dividir em artigos temáticos
  (aquisição, composição e séries, paisagem e fragmentos, conectividade).
