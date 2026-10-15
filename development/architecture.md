# Arquitetura e diretrizes de analise

## Regra obrigatoria para toda funcao nova ou refatorada

**Herdar primeiro; pedir somente decisoes que o objeto de entrada nao contem.**
Esta regra vale para todas as novas funcoes e etapas intermediarias, incluindo
vetorizacao, redes e objetos destinados a exportacao; nao e uma particularidade de land_to_graph.

> Removemos `plot_options`: a funcao obtem colecao, classes, CRS e resolucao
> do objeto de mapas e cruza os codigos com `dict_build()` para nomes e cores.

- Cada etapa recebe diretamente os objetos publicos da etapa anterior, sem
  exigir scripts de adaptacao, duplicacao de metadados ou reconstrucao manual
  de estatisticas, tabelas, legendas e geometrias. O contrato deve documentar
  entradas aceitas, campos herdados, perdas de informacao e saidas reutilizaveis.
- Colecao vem de `load_rasters()` e aceita texto ou numero. Classes sao
  identificadas por codigo numerico. Nomes pt/en, grupos e paleta vem do
  dicionario da colecao; nunca de uma nova lista de opcoes de apresentacao.
  Os mesmos contratos internos de colecao/dicionario sao compartilhados pelas
  novas funcoes, evitando implementacoes divergentes.
- CRS, resolucao e anos vem dos rasters/metadados recebidos. A saida espacial
  conserva o CRS de origem por padrao; reprojecao exige uma escolha explicita.
  Se um unico ano torna a selecao inequivoca, nao e preciso informa-lo.
  Uma etapa que aceita intervalos herda os limites quando nao sao informados.
- Parametros adicionais representam decisoes analiticas (vizinhanca, peso,
  dissolucao, selecao temporal). Titulos, temas e escalas sao
  alterados nos objetos ggplot retornados. Nao criar `plot_options` ou APIs
  paralelas para informacoes ja presentes no objeto ou no dicionario.
- Cada transformacao preserva colecao, selecao de classes, dicionario e
  proveniencia (anos, CRS e resolucao de origem, unidades e convencoes), mesmo
  quando nao conserva os rasters originais. A selecao temporal deve indexar
  os anos do objeto de entrada, nunca presumir que o primeiro raster corresponde
  ao primeiro ano solicitado.
- Reclassificacao binaria de varias classes perde a identidade original por
  pixel. A vetorizacao registra o conjunto selecionado, mas nao inventa codigo,
  nome ou cor unicos para um fragmento misto. Fundo nao recebe a identidade da
  classe selecionada. Objetos legados sem colecao sao explicitamente incompletos;
  nao presumir uma colecao para preencher nomes ou cores.
- Preservar tipos padrao (`sf`, `ggplot`, `igraph`, matrizes e data.frames).
  Serializar e recuperar deve conservar o contrato e permitir exportacao sem
  refazer a analise. Formatos externos que nao conservam atributos R devem
  transportar as chaves em colunas e ter as limitacoes documentadas.
- Testes devem cobrir o encadeamento real entre funcoes, heranca de metadados,
  cores equivalentes ao dicionario, CRS herdado/reprojetado, selecao temporal e
  recuperacao RDS/exportacao. Exemplos locais devem demonstrar esse mesmo fluxo
  usando apenas funcoes publicas e operacoes padrao dos objetos retornados.

## Contratos nativos, dicionario e interoperabilidade

- Toda etapa necessaria ao fluxo analitico deve estar em funcoes do pacote.
  Exemplos e testes locais apenas chamam a API publica: nao devem corrigir
  resultados, recalcular estatisticas nem reconstruir visualizacoes que
  constituem funcionalidades oferecidas pelo pacote.
- `dict_build(collection)` e a fonte unica para identidades, nomes em portugues
  (`classe`, `grupo`), nomes em ingles (`class`, `group`) e cores (`color`).
  O codigo numerico e a chave de ligacao entre rasters, matrizes, tabelas e
  grafos; rotulos traduzidos nunca devem substituir essa chave.
- Tabelas e objetos analiticos preservam ambos os idiomas. A escolha de idioma
  altera a apresentacao. Cores sao herdadas do dicionario da colecao informada
  por load_rasters; a API analitica nao exige paleta ou rotulos adicionais.
  A paleta original do dicionario continua registrada. Codigos desconhecidos
  permanecem identificaveis e recebem fallback explicito, sem inferir classes.
- Resultados devem ser objetos prontos para reuso (data.frame, matrix, sf,
  igraph, ggplot e htmlwidget), com metadados de parametros, colecao, CRS,
  resolucao, unidades e versao do contrato. Serializacao RDS deve permitir
  recuperar e exportar a analise sem acessar novamente os rasters originais.
- As funcoes analiticas novas retornam os resultados prontos em memoria;
  nao criar uma funcao propria de exportacao nem argumentos de formato, caminho
  e sobrescrita na analise. A escrita usa os metodos padrao dos objetos:
  `ggplot2::ggsave()` para graficos personalizaveis pela gramatica ggplot2,
  `saveRDS()/readRDS()` para a analise completa, `utils::write.csv()` para
  tabelas, `igraph::write_graph()` para redes, `htmlwidgets::saveWidget()` para
  widgets e `sf::st_write()` para vetores. CSV deve conservar chaves e nomes
  UTF-8; RDS conserva o contrato completo. O suporte SHP preexistente de
  reclass_to_polygons continua documentado no seu contrato.
- Graficos devem aceitar opcoes documentadas de apresentacao e continuar
  editaveis na gramatica ggplot2. Grafico estatico e widget sao objetos
  independentes: alteracoes posteriores em um nao alteram automaticamente o
  outro. Titulos devem ser editaveis com `ggplot2::labs()` apos a analise; nao
  entram em opcoes especificas da funcao. Todo grafico estatico inclui uma
  legenda de classes com codigo e nome localizado, para conservar a leitura
  em impressao quando os nos usam apenas codigos. Amostras de cor devem ter
  tamanho fixo e visivel, sem imprimir codigos hexadecimais. A exportacao
  reutiliza os objetos fornecidos.

`land_to_graph()` implementa essas diretrizes com `language`, sem
`plot_options`: colecao (texto ou numero), classes presentes, CRS e resolucao
vem do objeto de load_rasters; nomes e cores vem de dict_build. Um unico ano
e herdado automaticamente; varios anos exigem selecao. Titulos, temas e
escalas sao personalizaveis nos ggplots retornados.
Retorna `biomastats_land_graph`, incluindo `class_dictionary`,
`metadata`, tabelas, matrizes, igraph e quatro visualizacoes prontas. Os
resultados podem ser personalizados e salvos diretamente pelas ferramentas
padrao do R, inclusive apos readRDS. A analise nao escreve arquivos.
O ranking de fronteiras e retornado como `edge_plot`, sem montagem externa.
Contagens de adjacencia e comprimentos conservam suas convencoes documentadas;
personalizacao e exportacao nao mudam os valores calculados.

## Fluxo espacial existente

### Conectividade de fragmentos: land_connectivity e patch_imp_fast

`land_connectivity()` recebe os sf ou listas temporais de reclass_to_polygons,
inclusive apos readRDS. Nao procura o ultimo arquivo nem depende de scripts,
coluna de habitat por posicao ou escrita/releitura de shapefiles. Herda
identidades, paleta, ano, selecao, CRS e proveniencia. Somente value=1 entra
quando a coluna existe; varios anos exigem escolha explicita. Fragmentos
invalidos ou com sobreposicao de interiores sao recusados, sem reparo/dissolucao
implicitos. IDs sao preservados em tabelas, sf, matrizes e vertices do grafo.

O limite do estudo segue de load_rasters$shape para biomastats_metrics$boundary
e para o atributo boundary dos poligonos. A conectividade o herda para seus
denominadores. Sem limite, usa fecho convexo do habitat, identificado como
aproximacao nos metadados. boundary explicito pode substituir o herdado.

Distancias minimas entre bordas sao calculadas uma vez e reutilizadas nos
limiares e remocoes. A analise precisa de metros: conserva projecoes metricas
ou transforma uma copia para UTM local, com limites espaciais documentados.
source_polygons conserva as geometrias originais; metadados registram CRS
de origem/analise, area do limite e unidades. Esta copia metrica nao altera o
contrato da vetorizacao, que continua herdando seu CRS por padrao.

As rotinas internas otimizadas reproduzem os dez indices do lconnect 0.1.2.
IIC recalcula caminhos em submatrizes, sem recalcular distancias; AWF usa
operacoes matriciais e diagonal zero, inclusive na contribuicao marginal.
patch_imp_fast e API publica para objetos lconnect ja existentes e retorna
pimp sem escrita. Paralelizacao IIC e optativa e herda o plano do usuario.

As convencoes originais devem ficar explicitas: NC usa agrupamento <= limiar;
LNK e grafo exploratorio usam < limiar; IIC/CPL usam 0 < distancia < limiar e
descartam isolados. O grafo exploratorio preserva todos os fragmentos e inclui
contatos de distancia zero. Campos cluster_lconnect, component, iic_included,
graph_components e isolates distinguem essas interpretacoes em empates/contatos.
AWF usa todos os pares e seu limiar controla o decaimento. Se o indice inicial
for zero, a priorizacao por remocao e matematicamente indefinida: patch_imp_fast
retorna NaN por fragmento e anexa status "undefined_baseline" ao objeto pimp.
O mesmo status se aplica a uma paisagem de um fragmento, que nao tem ranking
relativo. O indice basal continua disponivel (IIC=0/AWF=0) para distinguir esse
caso; CPL sem pares conectados e NA. A matriz de distancias e densa, portanto
memoria cresce quadraticamente e paisagens muito grandes requerem outro desenho.

O resultado biomastats_connectivity oferece scenarios com objetos lconnect,
pimp e igraph, sf enriquecido por limiar, matrizes, tabelas e quatro ggplots:
grafo espacial, componentes, importancia e resposta ao range de distancias.
Cores de habitat vem do dicionario; componentes usam paleta categorica propria
e importancias usam escala continua. Nenhuma identidade individual e inventada
para habitat composto de classes fundidas. O desenho de ligacoes entre pontos
interiores e apenas ilustrativo, sem inferir corredores ou distancia centroide.
Os objetos sao serializaveis e salvos por ferramentas padrao do R.

O exemplo executavel esta em
`ignorados/testes-locais/teste-local-conectividade.Rmd`, com HTML na mesma
pasta ignorada pelo Git. Testes incluem equivalencia com con_metric/patch_imp
originais e upload_land, diagonal AWF, isolados, contatos/empates de limiar,
unidades, heranca de limite, recuperacao RDS e fluxo completo com o recorte.

### Mudancas temporais: land_change

`land_change(data, years)` recebe diretamente o objeto de load_rasters e
compara dois ou tres momentos. Reutiliza os contratos internos de colecao e
dicionario; nao recebe paletas, rotulos, CRS ou resolucao duplicados. Dois ou
tres anos totais sao herdados quando years e NULL; intervalos maiores exigem
escolha explicita. Grades diferentes sao recusadas, sem reamostragem implicita.

Os resultados usam uma unica mascara de pixels validos em todos os anos
selecionados. zero e classe, NA e ausencia. Areas geograficas sao estimadas por
celula e faixa de latitude com sf; areas projetadas representam a grade com
conversao explicita das unidades do CRS. A convencao de area, o motor geografico,
o CRS, resolucao, anos e cobertura constam no resultado serializavel.

O contrato `biomastats_land_change` retorna trajectories (caminhos completos
observados), transitions, matrices (pixels, area_ha, probability), class_metrics,
interval_metrics, trajectory_metrics, coverage, node_metrics, graph, plot_data,
plot e change_plot, alem de years, class_dictionary e metadata. Com tres anos,
inclui comparacoes consecutivas e primeiro-ultimo na mesma mascara. Categorias
de trajetoria distinguem persistencia, mudanca unica, retorno e duas mudancas.
Nao inferir trajetorias multiplicando matrizes de pares.

Persistencia, ganhos, perdas, saldo, troca e componentes de quantidade e
alocacao sao medidas descritivas, nao testes de causalidade, previsoes ou
estimativas de erro de classificacao. Linhas de probability sem origem sao NA.
Saldo anual e uma media aritmetica sobre o intervalo, sem inferir eventos anuais.

O grafico bipartite/tripartite usa faixas proporcionais a area de cada trajetoria,
coloridas pela classe inicial, e blocos pela classe de cada ano. Cores e nomes
vem do dicionario; legendas usam codigos e nomes sem hexadecimais impressos.
Os ggplots aceitam labs, temas e escalas. A rede temporal e um igraph dirigido,
com atributos de classes/anos e pesos em hectares; plot_data torna os dados do
desenho acessiveis. Nenhum arquivo e escrito pela funcao. saveRDS/readRDS,
ggsave, write.csv e write_graph atuam nos objetos retornados.

O exemplo de build e analise esta em
`ignorados/testes-locais/teste-local-mudancas.Rmd`, com HTML na mesma pasta
ignorada pelo Git. Os testes automatizados cobrem matrizes conhecidas,
conservacao de areas e fluxos, trajetorias de retorno, mascara comum, grade,
unidades projetadas/geograficas, paleta, serializacao e recorte real do pacote.
Validacao em 14/10/2026: 328 verificacoes na suite completa, incluindo 87 para
land_change, sem falhas, erros, avisos ou testes pulados. O Rmd foi executado
com build/instalacao temporaria e gerou HTML, RDS, PNG, CSV e GraphML pelos
metodos padrao dos objetos. A rede registra partition por ano; com dois anos,
inclui tambem o atributo type usado pelas rotinas bipartites do igraph.

### Demais etapas

O projeto e um pacote R: `R/` implementa as funcoes, `NAMESPACE` publica a
API, `man/` documenta os contratos e `tests/testthat/` valida o comportamento.
`inst/` distribui mapas de exemplo, dicionarios e shapefiles.

O fluxo principal baixa/carrega mapas (`download_*`, `load_rasters*`), calcula
areas e visualizacoes (`get_area`, `land_vis`, `land_dist`) e analisa a paisagem
com `biomastats_metrics`. Esta ultima organiza os anos e chama `lulc_metrics`
para cada raster. `lulc_metrics` seleciona classes, projeta uma copia para UTM
para as metricas e devolve o raster reclassificado no CRS original.
`biomastats_metrics` agrupa esses rasters em `rec_map`, na ordem start:end.
Herda start/end de time_range quando omitidos e repassa collection,
class_dictionary e metadata, com CRS/resolucao de origem por ano. Quando ha
recorte temporal, seleciona os rasters pelos indices do intervalo original.
`reclass_map` apenas desenha esse resultado; nao realiza a reclassificacao.

Para uma classe, `lulc_metrics` preserva seu codigo (por exemplo, floresta = 3);
para varias classes, substitui os codigos selecionados por 1. A nova funcao
`reclass_to_polygons` aceita ambos os contratos e normaliza a presenca para 1
sem alterar o objeto original. Usa terra para poligonizar e sf para geometria,
reprojecao e escrita. units garante hectares mesmo quando o CRS usa pes.
O padrao `crs = NULL` conserva o CRS de cada raster. Os objetos sf incluem
code, classe, class, color, collection e classes alem das medidas geometricas;
class_dictionary e metadata ficam em atributos do sf e da lista temporal.
Uma unica classe selecionada identifica a presenca; varias classes fundidas
conservam seus codigos em classes/dicionario e deixam a identidade individual
como NA. SHP conserva as colunas; RDS conserva tambem os atributos completos.
Os fragmentos usam conectividade por aresta; as metricas existentes usam oito
vizinhos, portanto a contagem pode diferir para pixels em contato diagonal.

O ramo OSM (`load_osm_data`, `rasterize_osm`, `distance_to_feature`,
`density_of_feature`, `integrate_feature`) e independente dessa vetorizacao.
Os testes desse ramo foram comentados a pedido, incluindo os testes com mocks,
assim como a consulta real no roteiro manual. Nenhuma consulta externa e
necessaria para testar ou executar o exemplo de floresta.

Execute `devtools::test(filter = "reclass-to-polygons")` para testar.
O exemplo executavel de build, analise e visualizacao esta em
`ignorados/testes-locais/teste-local-poligonos.Rmd`; ele e mantido fora do
pacote porque serve como verificacao local e nao como interface distribuida.

Validacao em 14/10/2026: `testthat::test_local(".")` passou com 90 verificacoes,
sem falhas, avisos ou testes pulados (o grupo OSM esta comentado). Dessas,
38 verificam vetorizacao, topologia, area, selecao temporal, entradas invalidas,
shapefiles e recuperacao dos pixels originais de floresta por rasterizacao.
O exemplo executado gerou 26 fragmentos e 99,81147 hectares em 1985.

Validacao dos contratos de heranca em 14/10/2026: suite completa com 254
verificacoes aprovadas, sem falhas, erros, avisos ou testes pulados. Inclui
selecao temporal correta nas metricas, paleta/dicionario compartilhados,
CRS herdado e reprojecao explicita, presenca/fundo, classes fundidas,
preservacao de partes geometricas e recuperacao por SHP/RDS. Os dois exemplos
locais (poligonos e adjacencias) sao renderizados pelo Rmd com build e
instalacao temporaria do pacote.
