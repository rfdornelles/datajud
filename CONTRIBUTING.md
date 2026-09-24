# Contribuir com o datajud

O `datajud` é um cliente R não oficial da API Pública do Datajud. Contribuições
são revisadas pelo mantenedor `@rfdornelles`. Agentes podem preparar código,
testes, documentação e PRs; revisão final, merge, tags e publicação são atos
manuais do mantenedor. Não use merge automático.

## Escolher uma tarefa

Consulte as [issues](https://github.com/rfdornelles/datajud/issues) e o
[Project](https://github.com/users/rfdornelles/projects/4). O fluxo do
[roadmap](roadmap/README.md) usa `Decisão = Aprovada` e exige dependências
concluídas antes da implementação. Use uma branch e um PR por issue. A aprovação
para desenvolver não autoriza merge ou publicação. O manifesto do roadmap
registra o planejamento; o andamento fica no Project.

Relatos de erro devem incluir um exemplo mínimo, versão do pacote, `sessionInfo()`
e o comportamento esperado. Use dados sintéticos, sem credenciais ou processos
reais identificáveis. Para problemas da API, informe tribunal, classe do erro e
status HTTP; não publique cabeçalhos de autenticação nem o cliente serializado.

## Preparar o ambiente

Faça fork se não tiver permissão de escrita e clone sua cópia. Na raiz:

```sh
git switch -c minha-contribuicao
Rscript -e 'install.packages(c("devtools", "rcmdcheck", "covr", "knitr", "rmarkdown"))'
Rscript -e 'devtools::install_deps(dependencies = TRUE)'
Rscript -e 'devtools::load_all()'
```

A instalação de dependências precisa de rede. A validação comum usa fixtures e
não consulta a API. O pacote declara R >= 4.1; a CI obrigatória executa R 4.2.
Confira também no R atual antes de um release. Para construir vinhetas, instale
Pandoc, disponível também com RStudio ou Quarto, e confira
`rmarkdown::pandoc_available()`.

## Estilo e mensagens

- Use português brasileiro na documentação, nos comentários e nas mensagens;
  preserve nomes oficiais da API e dos tribunais.
- Use `snake_case`, nomes de argumentos explícitos, indentação de dois espaços,
  atribuição com `<-` e pipe nativo `|>`, seguindo o estilo tidyverse.
- Prefira funções pequenas e retornos previsíveis. Não atribua objetos ao ambiente
  global nem carregue coleções inteiras implicitamente.
- Nomeie chamadas externas com `pacote::funcao()` e declare dependências diretas.
  Dependências de exemplos/testes vão em `Suggests`; de execução, em `Imports`.
- Mensagens devem dizer qual argumento ou operação falhou e como corrigir.
  Use `cli::cli_abort()` e classes estáveis quando houver tratamento programático.
  Não exponha chaves, cabeçalhos ou conteúdo sensível em mensagens.
- Em strings de código R, use escapes Unicode (`\u00e7`, por exemplo) quando
  necessário para portabilidade; comentários e documentação ficam em UTF-8.
- Não reformate arquivos inteiros sem necessidade. Preserve o estilo local e
  evite mudanças sem relação com a issue.

## Testes e documentação

Cubra comportamento observável, incluindo falhas relevantes. Não consulte a API
nos testes comuns. Use as fixtures em `tests/testthat/fixtures/` e mocks do
transporte; mantenha as condições que evitam vazamento de credenciais.

Mudanças públicas devem atualizar roxygen, exemplos, `NEWS.md` e as vinhetas
afetadas. Mudanças de estrutura também atualizam
[`dicionario-retornos.csv`](inst/extdata/dicionario-retornos.csv) e os testes de
contrato. Gere os arquivos `man/` e `NAMESPACE` com `devtools::document()`.
Mantenha `README.Rmd` e `README.md` coerentes. Os blocos de rede das vinhetas usam
`eval=FALSE`; os executáveis usam exemplos sintéticos distribuídos.

Validação local, na raiz:

```sh
Rscript -e 'devtools::document()'
Rscript -e 'devtools::test()'
Rscript scripts/verificar_vinhetas.R
Rscript -e 'rcmdcheck::rcmdcheck(args="--no-manual", error_on="note")'
git diff --check
```

A construção das vinhetas instala o pacote em biblioteca temporária, inicia uma
sessão limpa, bloqueia o transporte de rede e verifica links internos. Consulte
[data-raw/README-exemplos.md](data-raw/README-exemplos.md) para regenerar os exemplos.
A cobertura deve permanecer em pelo menos 90%, com as mesmas exclusões da CI:

```r
excluidas <- c("datajud_requisition", "datajud_consultar_processo", "obter_chave_publica_cnj")
cobertura <- covr::package_coverage(function_exclusions = excluidas)
stopifnot(covr::percent_coverage(cobertura) >= 90)
```

Veja o comando completo e a versão de R nos workflows de
[check](.github/workflows/R-CMD-check.yml) e [cobertura](.github/workflows/coverage.yml).
Não atualize snapshots apenas para fazer um teste passar: revise a mudança de
comportamento. Investigue warnings e notes; exceções ambientais precisam estar
explicadas no PR, sem desabilitar verificações gerais.

## Dados, cache e credenciais

Atualizações de TPU são explícitas, seguindo
[data-raw/README-tpu.md](data-raw/README-tpu.md). Não baixe dados no carregamento do
pacote. Versione os artefatos distribuídos e a proveniência, não os downloads do
cache. Preserve as fontes locais necessárias à reprodução e confira os hashes.
Não regenere dados de produção durante uma mudança que não os afeta.

Antes do commit, confira `git status --short`, `git diff --stat` e
`git diff --cached`. Não versione `.Renviron`, `.env`, clientes serializados,
credenciais, dumps reais de processos ou caches. A chave pública publicada pelo
CNJ e mantida explicitamente no pacote não equivale a uma credencial pessoal;
mesmo assim, não a replique em logs ou exemplos. Nunca imprima valores de segredos
em uma auditoria; reporte somente o arquivo e o tipo do achado.

## Entregar para revisão

Abra um PR com problema, comportamento resultante, issue e evidências da
validação. Explique mudanças incompatíveis e migração conforme a
[política de compatibilidade](COMPATIBILIDADE.md). PR em rascunho indica que o
mantenedor ainda precisa revisar; não implica autorização para merge.

A branch deve estar atualizada com `main`, com os checks `R 4.2` e
`Medir cobertura no R 4.2` verdes e conversas resolvidas. O mantenedor revisa e
realiza o merge manual. A configuração e a exceção para PRs de autoria do próprio
mantenedor estão em [.github/rulesets/README.md](.github/rulesets/README.md).
O [checklist de release](RELEASE.md) é aplicado em uma branch de preparação;
agentes não criam tags nem publicam releases.
