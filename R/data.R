#' Endpoints dos Tribunais no DataJud
#'
#' Este conjunto de dados foi obtido da API pública do DataJud e contém os endpoints de diversos tribunais.
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
"tribunais"
