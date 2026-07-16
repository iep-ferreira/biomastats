# Testes automatizados

## Testes locais no R

Com o pacote e as dependências instalados, execute:

```r
devtools::test()
```

O arquivo `test-package-locally.R` executa somente `devtools::test()`. O
`devtools::check()` não deve ser executado diretamente no Windows deste
projeto, pois o `R CMD build` pode copiar checkpoints longos do diretório
`.git` antes de aplicar o `.Rbuildignore`.

Os testes carregam `inst/examples/mapa_exemplo.Rdata` e reproduzem as etapas de análise do tutorial: leitura da área, cálculo de áreas, visualização, distribuição de classes, métricas e mapas reclassificados. A função `load_rasters()` não é chamada pelos testes.

## Sandbox Linux com Docker

O container instala as dependências, instala o pacote, carrega `biomastats`, executa um smoke test do fluxo e roda `R CMD check`:

No PowerShell:

```powershell
.\scripts\test-sandbox.ps1
```

No Linux ou macOS:

```bash
bash ./scripts/test-sandbox.sh
```

O Docker precisa estar instalado e com o daemon em execução. A imagem usa Linux e R 4.4.2 para tornar a instalação reproduzível.

## GitHub Actions

Cada push e pull request executa o build e o check em Linux, Windows e macOS.
O Linux testa as versões 4.4.3, 4.5.3 e 4.6.1; Windows e macOS testam a
versão 4.6.1. A matriz cobre a versão atual, uma versão anterior amplamente
utilizada e a série anterior ainda relevante.

## Mock temporário durante o desenvolvimento

O arquivo `tests/testthat/helper-mock-template.R` contém um exemplo comentado. Para substituir uma função durante um teste inteiro:

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

Também é possível usar o mock manualmente no console, sem alterar permanentemente o pacote:

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

O `with_mocked_bindings()` restaura o `get_area()` original ao final do bloco. Ele pode ser usado em testes manuais, desde que o pacote esteja carregado e a função experimental esteja definida na sessão.

O teste manual pronto para esse fluxo está em `test-new-functions.R`, na raiz do projeto. A implementação experimental é carregada de `development/functions/get_area_mod.R`. Execute-o com:

```r
source("test-new-functions.R")
```

Esse script usa `devtools::load_all()` para carregar temporariamente o código local, sem reinstalar o pacote. O diretório de desenvolvimento é versionado, mas excluído do pacote publicado; ele serve para protótipos em branches de desenvolvimento.
