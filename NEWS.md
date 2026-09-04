# datajud 0.0.0.9000

* `datajud_abrir_coleta()` reabre coletas gravadas sem materializar hits, e
  `datajud_ler_pagina()` lê explicitamente um único arquivo NDJSON como
  `datajud_resultado`. A abertura rejeita páginas órfãs e adia a validação do
  conteúdo e do checksum para a leitura da página selecionada.
* Objetos `datajud_coleta` e `datajud_resultado` agora têm invariantes
  centralizadas e impressão compacta. Os leitores de processos e movimentações
  aceitam diretamente resultados de pesquisa ou páginas de uma coleta, mas
  rejeitam a materialização implícita de uma coleta inteira.

* `datajud_coletar_processos()` grava coletas extensas incrementalmente, com
  um arquivo NDJSON atômico por página, manifesto versionado, checksums, limites
  seguros e retomada após falhas sem carregar todos os hits na memória.
* `datajud_pesquisar_proxima_pagina()` continua uma pesquisa por `search_after`
  com uma requisição sequencial por chamada, pausa configurável e proteção
  contra cursor ou processos repetidos. Metadados agora registram página e
  cursor utilizado.
* `datajud_pesquisar_classe_orgao()` foi removida. Use
  `datajud_pesquisar_processos()` com os mesmos argumentos `tribunal`,
  `classe_codigo`, `orgao_codigo`, `size` e, quando necessário, `cliente`. A
  nova função retorna `datajud_resultado`; os hits brutos ficam em `$hits`.
* Funções públicas de consulta agora mantêm `cliente = NULL` como último
  argumento e criam um cliente transitório quando ele é omitido. O uso antigo
  do cliente como segundo argumento continua temporariamente disponível com
  aviso de depreciação.
* A nova `datajud_pesquisar_processos()` pesquisa uma página por assunto,
  classe e/ou órgão e retorna `datajud_resultado`, com hits, consulta sanitizada,
  total, relação do total, quantidade recebida e próximo cursor.
* Vetores de assunto usam OR por padrão; `exigir_todos_assuntos` cria um filtro
  por código para expressar AND. Classe aceita um único código, de acordo com a
  cardinalidade do campo `classe.codigo`.
* Resultados possuem impressão compacta e método `tibble::as_tibble()`, cuja
  chave única é `id`; o número do processo permanece um campo informativo.
* `datajud_listar_tribunais()` seleciona localmente tribunais estaduais,
  federais, eleitorais, superiores, trabalhistas, militares estaduais ou toda
  a Justiça comum, sem acessar a rede.
* Um construtor interno puro agora combina assuntos, classes e órgãos em
  consultas estruturadas, valida códigos, tamanho e cursor e inclui ordenação
  estável por `@timestamp` e `id.keyword`.
* O contrato vigente da API Pública agora possui nota técnica e fixture
  versionadas, distinguindo evidência oficial de comportamento apenas
  observado em probes sanitizados.
* Um downloader reproduzível mantém a Wiki completa em
  `.cache/datajud-wiki/` e versiona somente o manifesto com status, data e hash
  de cada página.
* Todo o tráfego HTTP passa por uma camada interna única baseada em `httr2`;
  consultas são listas R serializadas de forma estruturada e a autenticação é
  redigida ao imprimir requisições.
* Erros HTTP agora possuem classes e mensagens acionáveis em português, sem
  incluir URLs, corpos ou credenciais. Somente respostas transitórias (429,
  500, 502, 503 e 504) são repetidas, respeitando `Retry-After` e o limite do
  cliente.
* A chave pública obtida da Wiki do CNJ tem formato e contexto validados antes
  de ser usada; respostas não HTML ou páginas alteradas falham de modo claro.
* A API pública passa a usar `datajud_cliente()` e deixa de criar ou consultar
  objetos no `.GlobalEnv`.
* `datajud_login()` foi removida. Configure a chave por argumento ou pela
  variável `DATAJUD_API_KEY`; na ausência de ambas, o pacote consulta a chave
  pública na Wiki do CNJ e possui uma cópia vigente como contingência.
* `datajud_ler_processo()` e `datajud_ler_movimentacoes()` agora exigem a lista
  de respostas em `base`, retornam tibbles diretamente e não imprimem o objeto.
* A leitura de processos preserva o `id` como chave interna, mantém assuntos
  estruturados em list-column e oferece `datajud_desaninhar_assuntos()` para
  obter uma linha por assunto.
* Leitores de movimentos retornam um esquema vazio estável quando o processo
  não possui movimentos ou quando o campo é omitido.
