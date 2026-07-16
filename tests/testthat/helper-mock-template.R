# Template opcional para substituir temporariamente uma função do pacote.
# Este arquivo não executa nada por padrão.
#
# 1. Defina a implementação experimental:
#
# get_area_mod <- function(data, dec_places = 3, plot_type = "areaplot") {
#   # cole aqui a implementação experimental de get_area()
# }
#
# 2. Em um teste de integração, ative o mock apenas durante aquele teste:
#
# test_that("pipeline com get_area experimental", {
#   local_mocked_bindings(
#     get_area = get_area_mod,
#     .package = "biomastats"
#   )
#
#   resultado <- alguma_funcao_que_chama_get_area(mapas)
#   expect_true(is.list(resultado))
# })
#
# Ao terminar o teste, testthat restaura automaticamente o get_area() original.
