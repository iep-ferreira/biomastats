# Identidade visual do livro do Bioma Stats

Referência para o livro em `tutorial/` (bookdown, formato `bs4_book`) e para
materiais derivados (apresentações, cartazes, páginas do projeto). Evolui a
identidade do tutorial anterior (`arquivo/tutorial-2.0/`): mantém o azul
institucional e adota o fundo, o verde e a água da arte da capa.

## Princípios

1. **O dado é o protagonista.** As cores do MapBiomas nos mapas e gráficos não
   são alteradas; a identidade fica na moldura (tipografia, títulos, caixas,
   tabelas).
2. **Leitura longa e confortável.** Texto serifado sobre fundo de papel, linhas
   de cerca de 70 caracteres, código e tabelas compactos.
3. **Pouco e consistente.** Uma figura ou tabela por ideia, sempre com a mesma
   largura; destaques só nas caixas padronizadas.

## Paleta

| Papel | Cor | Uso |
|:--|:--|:--|
| Azul institucional | `#2c5f8a` | links, cor primária |
| Azul escuro | `#1c4e6e` | títulos |
| Verde floresta | `#3b7d3a` | filete dos títulos, borda do código, partes do sumário, caixa "Dica" |
| Verde claro | `#e6efdc` | fundo da caixa "Dica" e cabeçalho das tabelas |
| Água | `#2a7f8a` | topo dos cartões de funcionalidades |
| Âmbar | `#b7791f` | caixa "Atenção" |
| Papel | `#fcf8ee` | fundo da página (o mesmo da arte da capa) |
| Tinta | `#22302a` | texto |
| Cinza | `#5c6670` | legendas e textos secundários |

O vermelho fica reservado para erros. Todas as combinações de texto e fundo têm
contraste AA ou superior.

## Tipografia (Google Fonts, incorporadas no site)

- **Corpo:** Source Serif 4. Serifada, continua a escolha do tutorial anterior.
- **Títulos e interface:** Source Sans 3, peso 600.
- **Código:** JetBrains Mono.

## Componentes

- **Capa:** arte `imagens/capa.jpg`, selo da versão do pacote (verde) e logos da
  UFSCar e do CNPq.
- **Cartões de funcionalidades:** grade responsiva na apresentação
  (`::: {.cartoes}` com `::: {.cartao}`).
- **Caixas de destaque:** `::: {.dica}`, `::: {.nota}` e `::: {.atencao}`, com
  rótulo automático e cor própria.
- **Código:** fundo creme, borda verde à esquerda; só funções do pacote e o
  mínimo de R básico.
- **Tabelas:** vírgula decimal, anos sem separador de milhar, cabeçalho verde
  claro, linhas alternadas.
- **Figuras:** largura total da coluna (mapas lado a lado com 50%); legendas
  curtas em cinza.

## Regras de conteúdo

- Saídas voltadas ao usuário: nada de mensagens de log, avisos técnicos ou
  impressão crua de objetos.
- Números no texto calculados nos próprios chunks (`num()`), nunca digitados.
- Etapas que dependem de internet aparecem com `eval = FALSE`; suas figuras são
  geradas uma vez, guardadas em `tutorial/imagens/` e reutilizadas.
- Duas edições com o mesmo conteúdo: português (`tutorial/`, publicada em
  `docs/`) e inglês (`tutorial/en/`, em `docs/en/`). A edição em inglês usa
  `language = "en"` nas funções que o aceitam. Um botão de idioma
  (`tutorial/idioma.html`) leva à página correspondente na outra edição; ao
  criar ou renomear um capítulo, atualize a tabela de pares nesse arquivo.

## Arquivos

- `tutorial/_output.yml`: tema `bslib` (cores e fontes).
- `tutorial/estilo.css`: componentes e ajustes finos.
- `tutorial/_common.R`: opções dos chunks e as funções `tabela()` e `num()`,
  com formato numérico de cada idioma (`tutorial/en/_lang.R` define o inglês).
- `tutorial/idioma.html`: botão de troca de idioma.
- `build_docs.R`: gera o site em `docs/` (GitHub Pages).
