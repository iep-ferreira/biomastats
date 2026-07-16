# Testes automatizados

## Testes locais no R

O script principal para validação local é:

```r
source("test-package-locally.R")
```

Ele é dividido em seções `# ----`, para execução manual no RStudio. Não há uma
função controladora nem seleção automática de fluxo: execute somente a seção
que deseja testar, na ordem indicada.

### Fluxo 1: codigo em desenvolvimento

Executa `devtools::test()` diretamente no diretório de trabalho. Use-o para
feedback rápido enquanto edita funções, testes e documentação.

```r
Abra `test-package-locally.R` e execute o bloco `FLUXO 1`:

```r
devtools::test()
```
```

### Fluxo 2: instalacao a partir do ultimo commit

Instala o pacote a partir do `HEAD` da branch atual em uma biblioteca temporaria
e executa um fluxo de uso semelhante ao tutorial:

- leitura dos shapefiles instalados (`UFSCar.shp` e `polygon.shp`);
- carregamento dos mapas salvos em `inst/examples/mapa_exemplo.Rdata`;
- calculo de areas com `get_area()`;
- visualizacao com `land_vis()`;
- distribuicao de classes com `land_dist()`;
- metricas com `biomastats_metrics()`;
- mapa reclassificado com `reclass_map()`.

```r
Abra `test-package-locally.R` e execute, em sequência, os blocos `FLUXO 2`,
começando por branch/commit, instalação, carregamento, shapefiles e tutorial.
```

Esse é o fluxo recomendado para verificar manualmente instalação, carregamento
e uso. Ele mostra os objetos intermediários e executa as principais funções do
tutorial em sequência. A instalação é feita em uma biblioteca temporária; o
pacote instalado na sua biblioteca pessoal não é removido.

O fluxo 2 usa `git archive HEAD`. Portanto, alteracoes nao commitadas sao
ignoradas nesse fluxo. Isso e intencional: ele testa o pacote como ele seria
instalado a partir do ultimo commit da branch, sem depender de uma instalacao
antiga do Windows. A instalacao ocorre em biblioteca temporaria, preservando a
biblioteca pessoal do R.

O `devtools::check()` nao deve ser executado diretamente no Windows deste
projeto, pois o `R CMD build` pode copiar checkpoints longos do diretorio
`.git` antes de aplicar o `.Rbuildignore`.

Os testes automatizados carregam `inst/examples/mapa_exemplo.Rdata` e
reproduzem etapas de analise do tutorial. A funcao `load_rasters()` nao e
chamada pelos testes automatizados, para evitar download e dependencia externa.

## Sandbox Linux com Docker

O container instala as dependencias, instala o pacote, carrega `biomastats`,
executa um smoke test do fluxo e roda `R CMD check`:

No PowerShell:

```powershell
.\scripts\test-sandbox.ps1
```

No Linux ou macOS:

```bash
bash ./scripts/test-sandbox.sh
```

O Docker precisa estar instalado e com o daemon em execucao. A imagem usa Linux
e R 4.4.2 para tornar a instalacao reproduzivel.

## GitHub Actions

Cada push e pull request executa o build e o check em Linux, Windows e macOS.
O Linux testa as versoes 4.4.3, 4.5.3 e 4.6.1; Windows e macOS testam a versao
4.6.1. A matriz cobre a versao atual, uma versao anterior amplamente utilizada
e a serie anterior ainda relevante.

## Mock temporario durante o desenvolvimento

O arquivo `tests/testthat/helper-mock-template.R` contem um exemplo comentado.
Para substituir uma funcao durante um teste inteiro:

```r
test_that("pipeline com get_area experimental", {
  local_mocked_bindings(
    get_area = get_area_mod,
    .package = "biomastats"
  )

  resultado <- alguma_funcao_que_chama_get_area(mapas)
  expect_true(is.list(resultado))
})
```

Tambem e possivel usar o mock manualmente no console, sem alterar
permanentemente o pacote:

```r
devtools::load_all(quiet = TRUE)

fn_env <- new.env(parent = globalenv())
fn_env[["get_area_original"]] <- biomastats::get_area
sys.source("development/functions/get_area_mod.R", envir = fn_env)
get_area_mod <- fn_env[["get_area_mod"]]

testthat::with_mocked_bindings(
  {
    resultado <- alguma_funcao_que_chama_get_area(mapas)
    print(resultado)
  },
  get_area = get_area_mod,
  .package = "biomastats"
)
```

O `with_mocked_bindings()` restaura o `get_area()` original ao final do bloco.
Ele pode ser usado em testes manuais, desde que o pacote esteja carregado e a
funcao experimental esteja definida na sessao.

O teste manual pronto para esse fluxo esta em `test-new-functions.R`, na raiz
do projeto. A implementacao experimental e carregada de
`development/functions/get_area_mod.R`. Execute-o com:

```r
source("test-new-functions.R")
```

Esse script usa `devtools::load_all()` para carregar temporariamente o codigo
local, sem reinstalar o pacote. O diretorio de desenvolvimento e versionado,
mas excluido do pacote publicado; ele serve para prototipos em branches de
desenvolvimento.
