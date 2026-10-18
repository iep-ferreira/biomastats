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

## Series anuais de uso e ocupacao: land_time_series

### Etapas separadas de inferencia serial

Os gráficos seriais avaliam a curva do modelo em uma malha interior densa,
sem alterar nem imputar observações. Diagnósticos e projeções mantêm um gráfico
de referência do ajuste. Densidade do limiar probabilístico, faixa de transição
(quantis 10%–90%) e IC bootstrap do centro são objetos distintos e identificados.
As tabelas `curve`, `kernels` e `transition_bands` são retornos nativos.
A correlação permite níveis, diferenças e resíduos de tendências lineares;
Pearson retorna separadamente o p-valor convencional e a aproximação serial
AR(1).

`land_time_series()` prepara exclusivamente o objeto temporal nativo, mantendo
`data`, `class_dictionary` e `metadata` herdados de `get_area()`. Nenhuma
seleção de modelo, correlação ou projeção deve ser executada implicitamente nessa
etapa. A API analítica é composta por objetos serializáveis e interoperáveis:

- `series_correlation()` recebe exatamente duas classes escolhidas, plota as duas
  séries e retorna uma medida de associação com significância.
- `series_fit_regime()` recebe uma classe e um modelo explícito. `segmented`,
  `bent_cable`, `prob_bent_uniform` e `prob_bent_logistic` mantêm continuidade;
  os dois últimos são bent-cables probabilísticos com limiar aleatório.
- `series_diagnostics()` recebe o ajuste e concentra qualidade, ACF/PACF
  residual e Ljung--Box.
- `series_forecast()` recebe o próprio ajuste, uma série por vez, sem promover
  seleção automática de modelo durante a projeção.

Cada etapa devolve tabelas, metadados, dicionário e ggplots editáveis. Arquivos
devem ser salvos pelo usuário a partir desses retornos, preservando a gramática
do ggplot2 e evitando scripts externos ao pacote.

`get_area()` preserva a API preexistente (Years, Occupied area, aggregate_data,
time_series, collection), acrescentando atributos metadata/class_dictionary no
resultado, na tabela e no ggplot. A tabela conserva area para apresentacao e
area_raw sem arredondamento para inferencia. Areas sao km2 calculadas por
terra::cellSize, incluindo correcao de projecao. Classes ausentes recebem zero
apenas em anos com pixels observados; anos inteiramente sem dados permanecem NA.
Metadados registram cobertura e se a grade/mascara espacial e identica entre anos.
Variacao de suporte espacial gera aviso na analise; nao deve ser confundida com
mudanca de uso. Colecao, nomes e cores continuam derivados de dict_build.

`land_time_series(get_area(mapas)$time_series)` aceita diretamente o grafico,
o resultado completo de get_area ou sua tabela. Atributos sao recuperados da
tabela se a manipulacao de um ggplot os remover. Tabelas externas sem metadados
usam unidade desconhecida e rotulos neutros: nao presumir colecao. Anos faltantes
viram NA; nao interpolar nem transformar ausencia de observacao em ausencia de
classe. Series incompletas ou com menos de oito anos nao recebem modelos, mas
mantem estatisticas descritivas e status explicitos. Series constantes recebem
projecoes constantes sem intervalos inferenciais.

O projeto de IC de Beatriz Munari (2022), secoes 4.2-4.3, fundamenta o modelo
quadratico bent-cable. A implementacao atual usa maxima verossimilhanca Gaussiana
estacionaria completa, erros independentes ou AR(1), selecao AICc/AIC/BIC e de zero
a cinco transicoes conforme suporte amostral (padrao maximo dois). Nao implementa
MCMC, Stan, AR(k) geral ou toda a familia de limiares aleatorios do projeto.
A otimizacao multistart e deterministica e sujeita a otimos locais. Cada regime
externo requer min_segment observacoes, transicoes nao se sobrepoem, gamma >=
0.05 ano e |rho| <= 0.98. Criterios contam coeficientes, centros, larguras, rho
e variancia; convergencia e solucoes na fronteira sao registradas.

As tabelas retornam resumo descritivo, inclinacao robusta de Sen, variacoes
anuais, candidatos, modelos completos, coeficientes/covariancia condicional,
transicoes, residuos/inovacoes, diagnosticos, correlacoes e projecoes. A faixa
q20-q80 e a regiao transiente do limiar uniforme (tau +/- 0.6*gamma), nao um
intervalo de confianca do centro. Pearson/Spearman incluem niveis, diferencas e
residuos; p-valores e ajuste BH sao exploratorios diante da autocorrelacao,
selecao e natureza composicional das areas. Diferencas de area nao substituem
as matrizes de transicao espacial de land_change.

Validacao reserva anos finais e refaz selecao/ajuste somente no prefixo de treino,
comparando RMSE/MAE com referencia ingenua. Projecoes extrapolam o regime final
com decaimento do residuo AR. Intervalos condicionais incluem inovacao e
covariancia dos coeficientes, mas excluem incerteza de transicoes, rho e selecao.
Nao truncar previsoes negativas nem impor conservacao por normalizacao silenciosa;
retornar flags e totais para inspecao. Ljung-Box e diagnostico indicativo.

Graficos de perfis, transicoes, variacoes, diagnostico e projecoes herdam cores
e legendas codigo/nome; a matriz de correlacoes usa escala numerica propria e
rotulos completos. Todas as saidas sao serializaveis por saveRDS/readRDS e os
graficos personalizaveis via ggplot2. Sem funcao de exportacao ou scripts auxiliares
no fluxo do usuario. Exemplo local: ignorados/testes-locais/teste-local-series-temporais.Rmd
com HTML executado na mesma pasta ignorada. Documentacao Rd deve ser gerada de
forma restrita aos arquivos alterados, evitando reformatar outras paginas.

### Regimes com múltiplas transições e correlação por objetivo

- `series_fit_regime()` ajusta de 0 a `max_breaks` transições (padrão 3) e
  seleciona por AICc/AIC/BIC; `breaks = k` fixa o número. Mínimos quadrados
  perfilados: coeficientes lineares exatos dados centros e escalas, refinados por
  L-BFGS-B a partir de uma grade, com separação mínima entre centros. Todos os
  candidatos ficam em `model_table`.
- Para U-BC e L-BC, `kernels` contém a densidade/CDF do limiar de cada transição e
  `transition_bands` os quantis 10%–90%. Os gráficos sempre mostram a curva
  ajustada avaliada em malha interior densa, sem criar observações.
- `change_points` traz estimativa pontual, IC bootstrap percentil (residual ou em
  blocos), escala, faixa e inclinações antes/depois de cada centro.
- Textos não ASCII nos gráficos usam escapes `\u` no código-fonte. Cores do
  dicionário ficam nos preenchimentos; cores muito claras são escurecidas apenas
  em linhas e contornos.

### Regras de organização e apresentação (obrigatórias)

- **Um arquivo por função exportada.** Cada função pública fica em
  `R/<nome_da_funcao>.R`, com seu bloco roxygen e os auxiliares internos que só
  ela usa; o Rd correspondente aponta para esse arquivo. Auxiliares
  compartilhados por várias funções ficam em arquivos internos nomeados pelo
  tema (por exemplo, `R/series_presentation.R`). No módulo temporal:
  `land_time_series.R`, `series_correlation.R`, `series_fit_regime.R`,
  `series_diagnostics.R` e `series_forecast.R`.
- **Legendas sempre em pt e en.** Toda função que gera gráficos recebe
  `language = c("pt", "en")`, e todos os textos (eixos, legendas, subtítulos,
  notas) passam pelo seletor interno `.tr()`. Funções que recebem um objeto
  ajustado herdam o idioma dele, mas aceitam `language` explícito.
- **Associação enxuta.** `series_correlation()` apenas representa as duas séries
  e uma medida de associação com significância (Pearson, Spearman ou Kendall
  sobre níveis, diferenças ou resíduos sem tendência; `p_serial` AR(1) para
  Pearson). Análises adicionais ficam a cargo do usuário, a partir de `data`.
- **Somente R.** Edições, testes, documentação e renderização usam R e
  ferramentas do pacote; não introduzir scripts em outras linguagens.

## Diversidade das classes: class_diversity

`class_diversity()` (arquivo `R/class_diversity.R`) recebe a saída de
`get_area()` — resultado completo, `time_series` ou `aggregate_data` — pelo
mesmo contrato de entrada das séries (`.series_input`), herdando dicionário,
cores, unidade e metadados. Classes são tratadas como espécies e áreas
(`area_raw`, sem arredondamento) como abundâncias. Por ano, retorna riqueza,
entropia de Shannon e seu máximo, equitabilidade de Pielou, entropia relativa ao
total de classes da série, números de Hill (q = 0, 1, 2), Simpson (D, 1 − D,
1/D e equitabilidade), Berger-Parker com a classe dominante e a dominância
log(S) − H'. A tabela `contributions` decompõe a entropia em −p log p por classe
e soma exatamente `shannon`. `base` define a unidade das entropias (nats, bits
ou dits); números de Hill são sempre em classes efetivas. `min_area` descarta
classes residuais e `classes` restringe o conjunto antes do cálculo das
proporções. Anos sem pixels válidos ficam NA. Gráficos (`indices`, `hill`,
`contribution`, `dominance`) seguem as cores do dicionário e têm legendas pt/en.
Exemplo local: `ignorados/testes-locais/teste-local-diversidade.Rmd`.

## Métricas de fragmentos na grade nativa: fragment_metrics

Regra obrigatória: **métricas espaciais são calculadas na grade de origem, sem
reprojetar rasters.** Reprojetar reamostra pixels e altera área e topologia dos
fragmentos (no recorte de exemplo, floresta 1985: 99,58 ha e 20 fragmentos na
grade nativa contra 97,71 ha e 21 fragmentos após reprojeção para UTM).

- Motor interno em `R/landscape_geometry.R`: área de cada pixel por
  `terra::cellSize` (geodésica em grades geográficas; área verdadeira em grades
  projetadas); bordas medidas no elipsoide WGS84 (arcos de paralelo e meridiano
  de cada linha) em grades geográficas e, em grades projetadas, pelo tamanho do
  pixel em metros corrigido pelo fator de escala local (exato em projeções
  conformes). Unidades lineares do CRS (pés, metros) são convertidas a metros.
- Zona UTM e hemisfério são detectados do centro da grade (`.utm_from_raster`),
  apenas para registro em `metadata`. Os argumentos `zone` e `hemisphere` de
  `lulc_metrics()` e `biomastats_metrics()` ficam obsoletos e são ignorados.
- `fragment_metrics()` (arquivo próprio) identifica fragmentos com
  `terra::patches` (8 vizinhos por padrão) e retorna, por fragmento, área,
  perímetro, área núcleo, índices de forma (incluindo o `shape` do
  landscapemetrics, 0,25 P/√A), dimensão fractal, centroide ponderado pela área
  (CRS herdado e lon/lat), raio de giração, alongamento e orientação do eixo
  principal, extensão e distância borda a borda ao vizinho mais próximo; e, por
  classe e ano, estatísticas distribucionais (quantis, assimetria, Gini, LPI,
  mesh, densidades). Métricas do landscapemetrics podem ser anexadas na mesma
  grade; as padrão (AI, coesão, clumpy) não dependem de unidade.
- A vetorização usa a rotina compartilhada `.vectorize_raster()` (a mesma de
  `reclass_to_polygons()`), apenas quando necessária: distância entre
  fragmentos (geodésica via s2 em grades geográficas) e geometrias de saída.
- `lulc_metrics()` usa o mesmo motor e mantém o formato de tabela do
  landscapemetrics; `biomastats_metrics()` herda as melhorias sem mudar a API.
- Transformações de coordenadas ficam restritas a pontos (centroides para
  lon/lat; centro da grade para a zona UTM).

## Camadas externas: lulc, feições e meio físico

Funções (um arquivo cada): `layer_catalog()`, `load_lulc()`, `load_features()`,
`load_physical()`, `feature_raster()` e `distance_to_nearest()`. Infraestrutura
interna compartilhada em `R/layer_sources.R`.

- **Mesma área de referência.** Todas recebem o objeto de mapas (ou o mesmo
  shapefile) e herdam limite, grade, CRS e ano. `load_lulc()` devolve o mesmo
  objeto de `load_rasters()` (`shape`, `time_range`, `raster` com
  `RasterLayer` em memória, `collection`), com `collection = "esa_worldcover"`
  reconhecida por `dict_build()` e pelo contrato de coleção; as funções de
  análise de uso e ocupação funcionam sem adaptação.
- **Fontes gratuitas, sem chave, mundiais:** ESA WorldCover e Copernicus DEM
  (COG com leitura só da janela), SoilGrids (WCS com recorte), OSM por extrato
  regional `.osm.pbf` (Geofabrik ou openstreetmap.fr, escolha automática do
  menor extrato que contém a área, cache local, sem Overpass) e Overture
  (índice STAC por release em cache, leitura só dos arquivos que cruzam a área).
- **Ano de catálogo acessível:** `layer_catalog()` lista versão e ano de cada
  fonte; cada carregamento registra a versão e o ano efetivamente lidos
  (`metadata$status` ou `status`): ano do WorldCover, data do extrato OSM
  (cabeçalho HTTP), release do Overture, versão do DEM e do SoilGrids.
- **Regra de 5 s (H).** Cada requisição que não responde em `timeout` segundos
  (padrão 5, opção `biomastats.timeout`), falha ou sofre limitação faz pular só
  o seu bloco: valor `NULL`, status `skipped` e mensagem; a análise continua.
  Leituras remotas pelo GDAL são precedidas por checagem rápida (`curl`, HEAD),
  pois o GDAL faz novas tentativas internas. Downloads grandes continuam
  enquanto há fluxo de dados e são abortados se pararem por `timeout` segundos;
  nenhum arquivo parcial fica no cache e as configurações do GDAL são
  restauradas. `load_features(max_extract_mb)` evita downloads de extratos
  muito grandes.
- **Covariáveis externas** são reamostradas para a grade dos mapas (bilinear;
  vizinho mais próximo para aspecto). Os mapas de uso não são reprojetados.
- **Superfícies por tipo de geometria** (`feature_raster`): pontos (distância,
  contagem, densidade), linhas (distância, comprimento, densidade) e polígonos
  (distância, cobertura, área, densidade), calculadas numa grade estendida até
  as feições do buffer e recortadas à grade de referência; densidade por janela
  quadrada com tabela de somas acumuladas e área geodésica da janela.
- **Distância a partir de coordenadas** (`distance_to_nearest`): índice
  espacial s2 até a feição ou até a área de uma classe (vetorizada com a rotina
  compartilhada), em metros geodésicos.
- Testes automatizados leves e offline (`test-external-layers.R`); teste manual
  com dados reais em `testes-manuais/teste-manual-camadas.Rmd`.
