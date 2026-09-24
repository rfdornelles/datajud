#' Endpoints dos Tribunais no Datajud
#'
#' Este conjunto de dados foi obtido da API pública do Datajud e contém os endpoints de diversos tribunais.
#' Inclui a sigla do tribunal, o tipo de justiça a que pertence (como Estadual, Federal, Trabalhista, etc.),
#' e o nome completo do tribunal. A extração e processamento dos dados são feitos através de uma função que consulta
#' a API, extrai as siglas das URLs, classifica o ramo da justiça de cada tribunal e ajusta os nomes dos tribunais conforme necessário.
#'
#' @format Um data frame com as seguintes colunas:
#' \describe{
#'   \item{tribunal}{Nome completo do tribunal.}
#'   \item{sigla}{Sigla do tribunal extraída da URL do endpoint.}
#'   \item{tipo}{Classificação do ramo da justiça a que o tribunal pertence (Estadual, Federal, Trabalhista, etc.).}
#'   \item{url}{URL do endpoint da API pública do tribunal.}
#' }
#' @source \url{https://datajud-wiki.cnj.jus.br/api-publica/endpoints/}
#' @import tibble
"tribunais"

#' Assuntos das Tabelas Processuais Unificadas do CNJ
#'
#' Cópia local das TPU, com uma linha por código e itens ativos e inativos.
#' A consulta destes dados não realiza downloads. A versão e a proveniência
#' estão no atributo `tpu_fontes`: URLs, data da versão, instante de download
#' em UTC e hashes MD5 dos arquivos originais e dos catálogos.
#' Pais omitidos pelas planilhas são complementados pela tabela SQL oficial
#' da mesma versão. Seus códigos estão em `tpu_ancestrais_sql`; suas datas são
#' `NA` e as listas de aplicabilidade são vazias, pois esses campos não são
#' inferidos dos filhos nem das datas administrativas do SQL.
#'
#' @format Um tibble ordenado por `codigo`, com as colunas:
#' \describe{
#'   \item{codigo}{Código TPU inteiro e único.}
#'   \item{nome}{Nome oficial em UTF-8.}
#'   \item{codigo_pai}{Código inteiro do pai; `NA` para raízes.}
#'   \item{ativo}{Lógico; `FALSE` indica texto riscado na exportação do SGT
#'     ou situação inativa no SQL para ancestrais complementados.}
#'   \item{data_publicacao, data_alteracao, data_inativacao, data_reativacao}{
#'     Datas (`Date`), sem componente de horário; `NA` quando não informado.
#'     Publicação é a menor data informada entre os segmentos. As variantes
#'     divergentes ficam no atributo `tpu_publicacoes_divergentes`, com código,
#'     data, segmento e grau da fonte.
#'     Datas de inativação podem faltar inclusive em itens inativos.}
#'   \item{segmentos}{Lista de vetores de texto com os segmentos de aplicação.}
#'   \item{graus}{Lista de vetores de texto com os rótulos completos de aplicação
#'     do catálogo, incluindo segmento e grau, tribunal ou órgão. Os rótulos
#'     completos preservam o vínculo entre segmento e aplicação.}
#' }
#' @details
#' Para atualizar, o mantenedor executa `Rscript data-raw/tpu.R --atualizar`
#' na raiz do repositório. A descoberta da versão é automática. A reprodução
#' da versão registrada usa `Rscript data-raw/tpu.R`; `--offline` exige as
#' fontes no cache local. A atualização falha se houver conflitos de nome,
#' pai, status ou datas não ausentes (exceto publicação), pais ausentes ou ciclos
#' na hierarquia.
#' Separadores sem nome do exportador não são registros; pais ausentes são
#' recuperados recursivamente da tabela `ITENS` do SQL oficial.
#' Consulte também `inst/extdata/tpu-fontes.csv` e `data-raw/README-tpu.md`
#' no repositório para a estratégia de reprodução e atualização.
#' @source \url{https://www.cnj.jus.br/sgt/versoes.php?tipo_tabela=A}
#' @examples
#' head(datajud_assuntos[c("codigo", "nome", "codigo_pai", "ativo")])
#' unique(attr(datajud_assuntos, "tpu_fontes")$versao)
"datajud_assuntos"

#' Classes das Tabelas Processuais Unificadas do CNJ
#'
#' Cópia local de classes processuais, incluindo itens ativos e inativos.
#' Não realiza downloads durante o uso. Possui o mesmo esquema, atributo
#' `tpu_fontes` e estratégia de atualização de [datajud_assuntos].
#'
#' @format Um tibble com uma linha por código, ordenado por `codigo`.
#'   As colunas são `codigo`, `nome`, `codigo_pai`, `ativo`,
#'   `data_publicacao`, `data_alteracao`, `data_inativacao`, `data_reativacao`,
#'   `segmentos` e `graus`; veja [datajud_assuntos] para tipos e significado.
#' @source \url{https://www.cnj.jus.br/sgt/versoes.php?tipo_tabela=C}
#' @examples
#' head(datajud_classes[c("codigo", "nome", "ativo")])
"datajud_classes"
