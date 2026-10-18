# Executado antes de cada capítulo (bookdown: before_chapter_script).
# O livro em inglês define `lang <- "en"` antes deste arquivo (en/_lang.R).
if (!exists("lang")) lang <- "pt"

knitr::opts_chunk$set(
  message = FALSE, warning = FALSE, comment = "",
  fig.align = "center", fig.width = 8, fig.height = 5,
  out.width = "100%", dpi = 110, fig.retina = 2
)
suppressPackageStartupMessages(library(biomastats))

# Tabelas: vírgula decimal em português, ponto em inglês; anos sem separador.
tabela <- function(x, digits = 1, ...) {
  knitr::kable(x, digits = digits, row.names = FALSE,
               format.args = list(decimal.mark = if (lang == "en") "." else ",", big.mark = ""), ...)
}

# Números no texto, no padrão de cada idioma.
num <- function(x, digits = 1) {
  if (lang == "en") formatC(x, format = "f", digits = digits, big.mark = ",")
  else formatC(x, format = "f", digits = digits, decimal.mark = ",", big.mark = ".")
}
