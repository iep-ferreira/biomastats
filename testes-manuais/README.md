# Testes manuais do biomastats

Dois relatórios R Markdown que conferem o pacote de ponta a ponta e geram um
HTML para inspeção visual. Complementam os testes automatizados
(`tests/testthat`), que rodam no GitHub Actions; estes rodam só na sua máquina.

| Teste | O que confere | Internet | Tempo aproximado |
|:--|:--|:--|:--|
| `teste-manual-integrado.Rmd` | Todas as funções de análise sobre o exemplo da Lagoa do Sino (Coleção 10, 1985–2024): mapas, áreas, diversidade, séries temporais, mudanças, adjacências, métricas da paisagem, fragmentos e conectividade, com tabelas dinâmicas e o tempo de cada função | não | 1 a 2 min |
| `teste-manual-aquisicao.Rmd` | `load_rasters()` com aquisição seletiva de tiles, sem cache: Lagoa do Sino (1 tile) e Itapetininga - SP (2 tiles) | sim | 3 a 5 min |

## Antes de rodar

1. Abra o R na raiz do repositório (pasta com `DESCRIPTION`).
2. Instale as dependências do pacote e, para estes relatórios, `rmarkdown`,
   `knitr` e `DT` (tabelas dinâmicas). O teste de aquisição também usa `geobr`.

```r
install.packages(c("rmarkdown", "knitr", "DT", "geobr"))
```

Não é preciso instalar o `biomastats`: cada relatório constrói o pacote a partir
do código-fonte atual em uma biblioteca temporária.

## Como rodar

```r
rmarkdown::render("testes-manuais/teste-manual-integrado.Rmd")
rmarkdown::render("testes-manuais/teste-manual-aquisicao.Rmd")
```

O HTML é criado ao lado do `.Rmd`, com o mesmo nome, e fica fora do Git.

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

## Problemas comuns

- **Erro ao baixar tiles ou manifestos:** a API pode estar temporariamente
  indisponível; tente de novo mais tarde.
- **Erro em `geobr::read_municipality`:** o serviço do IBGE/geobr não
  respondeu; tente de novo.
- **Acentos trocados no HTML (Windows):** rode antes
  `Sys.setlocale("LC_CTYPE", ".UTF-8")`.
- **Falha na construção do pacote:** rode `devtools::document()` e os testes
  automatizados (`devtools::test()`) para localizar o erro.
