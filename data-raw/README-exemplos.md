# Exemplos offline e dicionário

`Rscript data-raw/exemplos.R`, executado na raiz com `pkgload`, `testthat` e
`withr` instalados, gera `inst/extdata/exemplo-resultado.rds` e
`inst/extdata/exemplo-coleta/`. Usa as fixtures sintéticas dos testes e as funções
públicas de pesquisa/coleta, com o transporte substituído por respostas locais.
Datas do manifesto e cursores são fixos para reprodução. A chave usada é
fictícia e não é salva nos artefatos. Nenhum processo é consultado na API.

`inst/extdata/dicionario-retornos.csv` é a fonte editável do dicionário. Cada linha
define objeto, campo, tipo R, cardinalidade e significado. A vinheta renderiza
essas mesmas linhas; `tests/testthat/test-dicionario.R` compara nomes e tipos
com os retornos produzidos pelas fixtures. Ao mudar um retorno público, atualize
o CSV e os exemplos afetados.

Para validar em nova sessão e biblioteca temporária do pacote:

```sh
Rscript scripts/verificar_vinhetas.R
Rscript -e 'devtools::test()'
Rscript -e 'rcmdcheck::rcmdcheck(args="--no-manual", error_on="warning")'
```

A construção requer `knitr`, `rmarkdown` e Pandoc instalados, mas não requer rede.
O script de verificação usa `Rscript --vanilla`, bloqueia `httr2::req_perform()` e
`utils::download.file()`, renderiza os quatro HTML e verifica links internos.
As chamadas reais à API ficam em blocos `eval=FALSE`; os blocos executáveis leem
os artefatos instalados e as TPU locais. O script imprime o diretório temporário
com os HTML para inspeção.
