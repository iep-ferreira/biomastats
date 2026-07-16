# Checklist de ajustes futuros do pacote biomastats

Este checklist foi criado a partir da execucao de `scripts/test-sandbox.ps1` em Docker Linux. O fluxo principal funcionou: a imagem foi montada, o pacote foi instalado, o smoke test passou, o build gerou `biomastats_1.0.0.tar.gz` e os testes `testthat` rodaram com sucesso.

O ponto pendente e reduzir o resultado do `R CMD check`, que terminou com `4 WARNINGs, 4 NOTEs`.

## Prioridade alta

- [ ] Padronizar a licenca no `DESCRIPTION`.
  - Problema atual: `License: Affero GPL 3.0` e uma especificacao nao padronizada para pacotes R.
  - Opcao provavel: trocar para `License: AGPL-3`.
  - Verificar se o arquivo `LICENSE` deve ser mantido, removido do build ou referenciado explicitamente, para eliminar a NOTE: `LICENSE is not mentioned in the DESCRIPTION file`.

- [ ] Corrigir os arquivos de documentacao gerados por roxygen.
  - `dict_build()` tem argumento `collection` no codigo, mas nao documentado.
  - `load_rasters()` tem argumento `collection` no codigo, mas nao documentado.
  - `raster_index()` usa `collection = 10` no codigo, mas a documentacao ainda indica `collection = 7`.
  - `biomastats_metrics()` tem `export.raster` sem documentacao.
  - `check_maps()` tem `export_folder_path` sem documentacao.
  - `download_maps()` tem `file_name` sem documentacao.
  - `load_rasters()` ainda documenta argumentos antigos (`data_from`, `folder_path`) que nao aparecem mais na assinatura.

- [ ] Rodar `devtools::document()` de forma intencional depois dos ajustes de roxygen.
  - Hoje o `DESCRIPTION` indica `RoxygenNote: 7.2.3`, enquanto a maquina local tem roxygen2 7.3.3.
  - Decidir entre atualizar a documentacao com roxygen2 7.3.3 ou fixar a versao antiga em ambiente controlado.

- [ ] Resolver os imports ausentes apontados pelo `R CMD check`.
  - Adicionar `methods` em `Imports`, se `methods::as()` continuar sendo usado.
  - Declarar `importFrom(methods, as)` no roxygen/NAMESPACE.
  - Declarar `importFrom(utils, download.file, head)` no roxygen/NAMESPACE.

## Prioridade media

- [ ] Corrigir variaveis globais aparentes em codigo com `dplyr`/avaliacao nao padrao.
  - Variaveis apontadas: `Year`, `Avg_frag_area`, `Edge_length`, `Edge_density`, `Class_area`, `Aggregation_Index`, `Number_of_fragments`, `year`, `code`, `x`, `y`, `value`.
  - Preferir `.data$coluna` onde fizer sentido.
  - Usar `utils::globalVariables()` apenas quando for a opcao mais limpa para objetos criados por avaliacao nao padrao.

- [ ] Remover chamadas internas com `biomastats:::`.
  - O check apontou uso de `biomastats:::dict_build` dentro do proprio pacote.
  - Dentro do pacote, chamar `dict_build()` diretamente.

- [ ] Tratar caracteres nao ASCII em arquivos de codigo R.
  - Arquivos apontados: `R/download_mapbiomas.R`, `R/get_api_values.R`, `R/raster_index.R`.
  - Usar `tools::showNonASCIIfile()` para localizar os caracteres.
  - Substituir por ASCII ou usar escapes `\uXXXX` quando o caractere for necessario em string.

- [ ] Remover arquivos ocultos desnecessarios de `tests/testthat`.
  - O check apontou `tests/testthat/.gitkeep` e `tests/testthat/.keep`.
  - Como ja existem testes reais no diretorio, esses marcadores provavelmente nao sao mais necessarios.

- [ ] Revisar o aviso sobre objetos serializados em `inst/examples/mapa_exemplo.Rdata`.
  - O build avisou que objetos serializados em versao 3 exigem `R >= 3.5.0`.
  - Decidir entre adicionar `Depends: R (>= 3.5.0)` ou recriar o `.Rdata` em formato compativel com versoes antigas.
  - Como o pacote ja esta sendo testado em R moderno, adicionar a dependencia minima tende a ser a solucao mais simples.

- [ ] Remover ou revisar `LazyData: false` no `DESCRIPTION`.
  - O build informou: `Omitted LazyData from DESCRIPTION`.
  - Como o exemplo esta em `inst/examples`, e nao em `data/`, esse campo pode ser desnecessario.

## Prioridade baixa

- [ ] Reduzir mensagens repetidas durante smoke tests e checks.
  - O log repetiu: `Collection 10 subtitles are loaded as default...`.
  - Avaliar se isso deve virar `message()` controlavel, aviso unico, ou ser silenciado nos testes.

- [ ] Melhorar legibilidade do log no PowerShell.
  - O log exibiu caracteres corrompidos em trechos de output do R, por exemplo aspas tipograficas.
  - Avaliar ajuste de encoding no script PowerShell, como UTF-8 para a saida do console.

- [ ] Atualizar a imagem base do Docker ou parametrizar a versao do R.
  - O sandbox atual usa `rocker/r-ver:4.4.2`.
  - O GitHub Actions ja testa uma matriz mais ampla.
  - Opcao futura: usar `ARG R_VERSION=4.6.1` no Dockerfile ou manter o Docker como ambiente Linux estavel separado da matriz do GitHub.

- [ ] Considerar upload dos logs do `R CMD check` no GitHub Actions.
  - Em falhas futuras, salvar `*.Rcheck/00check.log` como artifact facilita diagnostico sem repetir o job localmente.

## Criterio de conclusao

- [ ] `scripts/test-sandbox.ps1` termina com `Status: OK`, sem WARNINGs e sem NOTEs relevantes.
- [ ] GitHub Actions passa em Linux, Windows e macOS.
- [ ] O fluxo local continua reservado para `devtools::test()` e testes manuais de desenvolvimento.
- [ ] O build/check oficial continua acontecendo em Docker Linux e GitHub Actions.
