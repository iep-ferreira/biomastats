# Funções em desenvolvimento

Este diretório contém implementações experimentais que ainda não fazem parte
do namespace do pacote. Elas são carregadas explicitamente pelos testes ou
manualmente durante o desenvolvimento.

## Política de versionamento

- Mantenha os arquivos versionados no Git para preservar histórico e revisão.
- Desenvolva cada alteração em uma branch própria, por exemplo
  `feature/get-area-experimental`.
- Não coloque funções experimentais em `R/` antes de consolidá-las.
- Ao promover uma função para o pacote, mova sua implementação para `R/`,
  atualize a documentação e remova o protótipo deste diretório.
- O diretório é excluído do tarball do pacote por `.Rbuildignore`.
