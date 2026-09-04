
<!-- README.md is generated from README.Rmd. Please edit that file -->

# datajud

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

O `datajud` é um pacote R não oficial para consultar a API Pública do
Datajud, disponibilizada pelo Conselho Nacional de Justiça (CNJ). A
interface prioriza funções simples, retornos explícitos e composição com
pipes, sem criar objetos automaticamente no ambiente global.

O pacote está em desenvolvimento e sua interface pode mudar. Relate
problemas e sugestões nas [issues do
projeto](https://github.com/rfdornelles/datajud/issues).

## Instalação

Instale a versão de desenvolvimento a partir do GitHub:

``` r
install.packages("pak")
pak::pak("rfdornelles/datajud")
```

``` r
library(datajud)
```

## Cliente

A chave publicada pelo CNJ é pública. O pacote tenta obtê-la da Wiki
oficial e mantém uma cópia da chave vigente como contingência. Também é
possível informar a chave diretamente ou pela variável de ambiente
`DATAJUD_API_KEY`. Nas chamadas simples, não é necessário criar um
cliente: as funções criam um cliente transitório automaticamente.

``` r
# Alternativas explícitas:
cliente <- datajud_cliente(api_key = "chave-publicada-pelo-cnj")
Sys.setenv(DATAJUD_API_KEY = "chave-publicada-pelo-cnj")
cliente <- datajud_cliente()
```

Crie um cliente explicitamente apenas quando quiser configurar chave,
e-mail, timeout ou tentativas. O e-mail, quando informado, compõe o
`User-Agent`:

``` r
cliente <- datajud_cliente(email = "seu.email@dominio.com")
```

## Consulta por número de processo

A consulta pública recebe o número CNJ, com ou sem pontuação. O tribunal
pode ser informado ou inferido do número. Para vários processos, use um
tribunal único, um tribunal por processo ou `NA` escalar para inferir
todos. Não misture tribunais informados e `NA` no mesmo vetor.

``` r
respostas <- datajud_consultar_processo(
  processo = "0000001-89.2020.8.26.0000",
  tribunal = "TJSP"
)
```

``` r
numeros <- c(
  "00008323520184013202",
  "07223914020178070001"
)

respostas <- datajud_consultar_processo(
  processo = numeros,
  tribunal = NA
)
```

O número CNJ é o parâmetro de consulta. O campo `id` devolvido pelo
Datajud é preservado como chave interna do pacote.

## Pesquisa por assunto, classe e órgão

A pesquisa geral aceita vários assuntos, uma classe e um ou mais órgãos.
Dentro de vetores de assunto ou órgão, a combinação padrão é OR;
categorias diferentes são combinadas com AND.

``` r
resultados <- datajud_pesquisar_processos(
  tribunal = "TJSP",
  assunto_codigo = c(899, 900),
  classe_codigo = 1116,
  orgao_codigo = 13597,
  size = 100
)
```

Quando for necessário configurar o transporte, informe `cliente` por
último:

``` r
resultados <- datajud_pesquisar_processos(
  tribunal = "TJSP",
  assunto_codigo = 899,
  cliente = datajud_cliente(timeout = 60)
)
```

### Paginação por cursor

Cada chamada retorna uma página e preserva o cursor do último hit. Para
buscar somente a página seguinte, use
`datajud_pesquisar_proxima_pagina()`. A função faz no máximo uma
requisição, de forma sequencial, e não acumula todas as páginas na
memória.

``` r
cliente <- datajud_cliente()
pagina_1 <- datajud_pesquisar_processos(
  tribunal = "TJSP",
  assunto_codigo = 899,
  size = 100,
  cliente = cliente
)
pagina_2 <- datajud_pesquisar_proxima_pagina(
  pagina_1,
  pausa = 0.5,
  cliente = cliente
)
```

Quando uma página não possui cursor seguinte, a função retorna `NULL`
sem acessar a rede. Cursores repetidos ou processos duplicados entre
páginas adjacentes interrompem a paginação com erro, evitando loops e
duplicação. Também é possível continuar manualmente passando
`cursor = pagina_1$metadados$proximo_cursor` para
`datajud_pesquisar_processos()`.

### Coleta incremental em disco

Para volumes maiores, `datajud_coletar_processos()` exige um diretório
exclusivo e grava uma página por vez. O retorno contém os caminhos e
metadados, mas não mantém todos os hits na memória.

``` r
coleta <- datajud_coletar_processos(
  tribunal = "TJSP",
  diretorio = "dados/tjsp-assunto-899",
  assunto_codigo = 899,
  size = 500,
  limite_registros = 10000,
  limite_paginas = 100,
  pausa = 0.5
)

coleta$arquivos
coleta$manifesto

# Em uma nova sessão, reabra somente o manifesto e os metadados:
coleta <- datajud_abrir_coleta("dados/tjsp-assunto-899")

# Somente este arquivo NDJSON é materializado:
pagina_1 <- datajud_ler_pagina(coleta, 1)
processos_1 <- datajud_ler_processo(pagina_1)
```

Cada página concluída é gravada primeiro em um arquivo temporário e
depois renomeada atomicamente. O manifesto registra a consulta
sanitizada, o hash da consulta, os cursores, as contagens e o checksum
MD5 de cada arquivo. Se uma requisição falhar, execute novamente a mesma
chamada e o mesmo diretório para continuar depois da última página
válida. Consultas incompatíveis e arquivos alterados são rejeitados, sem
sobrescrever a coleta existente.

As atualizações do manifesto também usam troca por renomeação no mesmo
diretório, com um backup transitório restaurável, em vez de copiar
parcialmente sobre o manifesto vigente.

#### Por que NDJSON?

NDJSON mantém um objeto JSON completo por linha. Isso permite gravar,
validar e processar uma página por vez, inclusive com ferramentas de
linha de comando, sem reconstruir na memória um único documento JSON com
toda a coleta. O formato também é simples, interoperável e não exige
Arrow como dependência do pacote. Formatos colunares continuam sendo uma
boa opção para a etapa posterior de análise; aqui, NDJSON funciona como
formato transacional e retomável da coleta.

`print(coleta)` usa apenas os metadados já presentes no objeto e não
abre os arquivos NDJSON. `datajud_abrir_coleta()` valida a estrutura, a
existência dos arquivos e a ausência de páginas órfãs sem ler o conteúdo
de todas as páginas. O checksum e a materialização acontecem
explicitamente em `datajud_ler_pagina(coleta, numero)`, sempre para uma
única página. Não existe uma operação implícita que leia a coleta
inteira para a memória.

### Migração da pesquisa antiga

`datajud_pesquisar_classe_orgao()` foi removida enquanto o pacote ainda
está em fase experimental. A substituição direta é
`datajud_pesquisar_processos()`:

| Argumento antigo | Argumento atual | Observação |
|----|----|----|
| `tribunal` | `tribunal` | Mantém a sigla do tribunal. |
| `classe_codigo` | `classe_codigo` | Aceita um único código de classe. |
| `orgao_codigo` | `orgao_codigo` | Mantém um ou mais códigos de órgão. |
| `size` | `size` | Mantém o limite de 1 a 10.000 resultados por página. |
| `cliente` | `cliente` | Agora é opcional e, quando usado, deve ser o último argumento. |

``` r
# Antes:
# datajud_pesquisar_classe_orgao(
#   tribunal = "TJRJ", cliente = cliente,
#   classe_codigo = 1116, orgao_codigo = 13597, size = 500
# )

# Agora:
resultado <- datajud_pesquisar_processos(
  tribunal = "TJRJ",
  classe_codigo = 1116,
  orgao_codigo = 13597,
  size = 500
)

# Os leitores recebem diretamente o objeto de resultado:
processos <- datajud_ler_processo(resultado)
```

A função antiga devolvia diretamente a lista de hits. A nova função
devolve um `datajud_resultado`, que também preserva metadados, consulta
e cursor. Para obter apenas a estrutura antiga, use `resultado$hits`;
para análise tabular, use `tibble::as_tibble(resultado)`. Os leitores
aceitam tanto essa lista de hits quanto o próprio `datajud_resultado`,
inclusive quando ele representa uma página aberta por
`datajud_ler_pagina()`.

## Leitura dos resultados

Os leitores recebem os resultados explicitamente e retornam tibbles.
Assuntos são preservados em uma list-column para manter uma linha por
processo.

``` r
processos <- datajud_ler_processo(respostas)
movimentacoes <- datajud_ler_movimentacoes(respostas)
assuntos <- datajud_desaninhar_assuntos(processos)

# Um resultado de pesquisa também pode ser fornecido diretamente:
processos <- datajud_ler_processo(resultado)
```

## Contrato da API e cache da Wiki

As decisões sobre campos, tipos, operadores, limites e paginação são
rastreáveis na [nota técnica do contrato](inst/contrato-api-publica.md).
O repositório também contém scripts para atualizar o cache local da Wiki
e executar um probe real sanitizado. O HTML completo permanece em
`.cache/datajud-wiki/`, fora do Git; somente o manifesto de URLs, status
e hashes é versionado.

## Termos da API e licença do pacote

São regimes distintos:

- o código-fonte do pacote `datajud` é software livre sob a licença
  GPL-3-or-later, que permite usar, estudar, modificar e redistribuir o
  software nas condições dessa licença;
- o acesso à API Pública e o uso das informações do Datajud obedecem ao
  [Termo de Uso do
  CNJ](https://datajud-wiki.cnj.jus.br/api-publica/termo-uso/),
  incluindo as restrições e responsabilidades definidas pelo próprio
  CNJ.

Usar o pacote não substitui a leitura nem altera os termos do CNJ. O
projeto não é afiliado ao CNJ e não oferece garantia sobre
disponibilidade, precisão ou atualidade dos dados da API.

## Filosofia e contribuições

O pacote busca facilitar o acesso programático ao Datajud para pessoas
com diferentes níveis de experiência em R. Sua evolução segue práticas
do tidyverse: argumentos explícitos, funções composáveis, retornos
previsíveis e testes reproduzíveis sem rede.

Contribuições, sugestões e relatos de erro são bem-vindos. Consulte o
[roadmap](https://github.com/rfdornelles/datajud/issues?q=is%3Aissue+label%3Aroadmap)
antes de propor mudanças maiores.
