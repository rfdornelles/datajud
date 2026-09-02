## Funções auxiliares e de alto nível para extrair dados do Datajud, na modalidade de pesquisa por classe.codigo e/ou orgaoJulgador.codigo
## sabemos que o Datajud se baseia em consulta do Elastic e, em tese, qualquer campo pode ser buscado
## como TODO, uma função que permita busca por qualquer parâmetro

## Aqui a função de alto nível é a datajud_pesquisar_classe_orgao, as demais são auxiliares

monta_consulta_elasticsearch <- function(classe_codigo = NULL,
                                         orgao_codigo = NULL,
                                         size = 1000) {
  criar_query_datajud(
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = size
  )
}

validar_tribunal_pesquisa <- function(tribunal) {
  if (!is.character(tribunal) || length(tribunal) != 1L ||
      is.na(tribunal) || !nzchar(trimws(tribunal))) {
    cli::cli_abort("{.arg tribunal} deve ser uma sigla n\u00E3o vazia.")
  }

  toupper(trimws(tribunal))
}

#' Pesquisar processos por assunto, classe e órgão
#'
#' Executa uma página de pesquisa na API Pública do Datajud. Códigos dentro da
#' mesma categoria são combinados com OR; categorias diferentes são combinadas
#' com AND. A função retorna resultados e metadados diretamente, sem criar
#' objetos no ambiente global.
#'
#' @param tribunal Sigla do tribunal a consultar.
#' @param cliente Objeto criado por [datajud_cliente()].
#' @param assunto_codigo Vetor opcional de códigos de assunto.
#' @param classe_codigo Código opcional de uma única classe processual.
#' @param orgao_codigo Vetor opcional de códigos de órgão julgador.
#' @param size Número de resultados da página, entre 1 e 10.000.
#' @param cursor Cursor opaco `search_after` retornado pela página anterior.
#' @param exigir_todos_assuntos Se `TRUE`, exige a presença de todos os assuntos
#'   informados; por padrão, qualquer assunto satisfaz o filtro.
#'
#' @return Objeto `datajud_resultado` com hits, consulta sanitizada e metadados.
#' @export
#'
#' @examples
#' \dontrun{
#' cliente <- datajud_cliente()
#'
#' # Somente assunto ou somente classe
#' por_assunto <- datajud_pesquisar_processos(
#'   "TJSP", cliente, assunto_codigo = 899
#' )
#' qualquer_assunto <- datajud_pesquisar_processos(
#'   "TJSP", cliente, assunto_codigo = c(1, 2, 3, 4)
#' )
#' todos_assuntos <- datajud_pesquisar_processos(
#'   "TJSP", cliente,
#'   assunto_codigo = c(1, 2, 3, 4),
#'   exigir_todos_assuntos = TRUE
#' )
#' por_classe <- datajud_pesquisar_processos(
#'   "TJSP", cliente, classe_codigo = 1116
#' )
#' por_orgao <- datajud_pesquisar_processos(
#'   "TJSP", cliente, orgao_codigo = 13597
#' )
#'
#' # Vários assuntos, combinados com uma classe e um órgão
#' combinada <- datajud_pesquisar_processos(
#'   "TJSP", cliente,
#'   assunto_codigo = c(899, 900),
#'   classe_codigo = 1116,
#'   orgao_codigo = 13597
#' )
#' tibble::as_tibble(combinada)
#' }
datajud_pesquisar_processos <- function(
    tribunal,
    cliente,
    assunto_codigo = NULL,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100L,
    cursor = NULL,
    exigir_todos_assuntos = FALSE) {
  validar_cliente(cliente)
  tribunal <- validar_tribunal_pesquisa(tribunal)
  consulta <- criar_query_datajud(
    assunto_codigo = assunto_codigo,
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = size,
    cursor = cursor,
    exigir_todos_assuntos = exigir_todos_assuntos
  )
  endpoint <- aux_retorna_endpoint(tribunal)
  resposta <- requisitar_api_datajud(cliente, endpoint, consulta)

  novo_datajud_resultado(
    resposta = resposta,
    tribunal = tribunal,
    consulta = consulta
  )
}


### funcao para requisicao por assunto / codigo
#' Pesquisa processos no Datajud por classe e/ou órgão julgador
#'
#' Esta função realiza uma pesquisa no Datajud, permitindo filtrar processos por código de classe e/ou por código de órgão julgador.
#' É possível especificar um tamanho máximo para a amostra de resultados retornados.
#'
#' @param tribunal Identificador do tribunal a ser consultado.
#' @param cliente Objeto criado por `datajud_cliente()`.
#' @param classe_codigo Código opcional de uma única classe processual.
#' @param orgao_codigo Vetor opcional de códigos de órgão julgador para filtrar os processos.
#' @param size Tamanho máximo da amostra de resultados a ser retornada, com um valor padrão de 100. O tamanho máximo permitido é 10000.
#'
#' @return Uma lista com os processos encontrados pela consulta.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pesquisar processos no TJSP por classe de assunto 1116 e tamanho da amostra de 100
#' cliente <- datajud_cliente()
#' datajud_pesquisar_classe_orgao(tribunal = "TJSP", cliente = cliente,
#'                                classe_codigo = c(1116), size = 100)
#'
#' # Pesquisar processos no TJMG por órgão julgador 13597 com o tamanho padrão da amostra
#' datajud_pesquisar_classe_orgao(tribunal = "TJMG", cliente = cliente,
#'                                orgao_codigo = c(13597))
#'
#' # Pesquisar processos no TJRJ por classe de assunto e órgão julgador especificados
#' datajud_pesquisar_classe_orgao(tribunal = "TJRJ", cliente = cliente,
#'                                classe_codigo = c(1116),
#'                                orgao_codigo = c(13597), size = 500)
#' }

datajud_pesquisar_classe_orgao <- function(
    tribunal,
    cliente,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100) {

  if (length(tribunal) != 1L || is.na(tribunal) || !nzchar(tribunal)) {
    cli::cli_abort("Tribunal n\u00E3o informado")
  }

  if (is.null(classe_codigo) && is.null(orgao_codigo)) {
    cli::cli_abort("Nenhuma classe ou unidade informada")
  }

  if (!is.numeric(size) || length(size) != 1L || !is.finite(size) ||
      size != round(size)) {
    cli::cli_abort("Tamanho da amostra deve ser um n\u00FAmero inteiro")
  }

  if (size < 1 || size > 10000) {
    cli::cli_abort("Tamanho da amostra deve ser um n\u00FAmero inteiro entre 1 e 10000")
  }

  validar_cliente(cliente)

  body <- monta_consulta_elasticsearch(
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = round(size)
  )
  url <- aux_retorna_endpoint(tribunal)
  resposta <- requisitar_api_datajud(cliente, url, body)
  purrr::pluck(resposta, "hits", "hits", .default = list())
}
