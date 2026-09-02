## Funções auxiliares e de alto nível para extrair dados do Datajud, na modalidade de pesquisa por classe.codigo e/ou orgaoJulgador.codigo
## sabemos que o Datajud se baseia em consulta do Elastic e, em tese, qualquer campo pode ser buscado
## como TODO, uma função que permita busca por qualquer parâmetro

## Aqui a função de alto nível é a datajud_pesquisar_classe_orgao, as demais são auxiliares

monta_consulta_elasticsearch <- function(classe_codigo = NULL,
                                         orgao_codigo = NULL,
                                         size = 1000) {
  filtros <- list()

  if (!is.null(classe_codigo) && length(classe_codigo) > 0L) {
    filtros <- append(filtros, list(list(
      terms = list("classe.codigo" = I(unname(classe_codigo)))
    )))
  }

  if (!is.null(orgao_codigo) && length(orgao_codigo) > 0L) {
    filtros <- append(filtros, list(list(
      terms = list("orgaoJulgador.codigo" = I(unname(orgao_codigo)))
    )))
  }

  query <- if (length(filtros) == 0L) {
    list(match_all = list())
  } else {
    list(bool = list(filter = filtros))
  }

  list(size = as.integer(size), query = query)
}


### funcao para requisicao por assunto / codigo
#' Pesquisa processos no Datajud por classe e/ou órgão julgador
#'
#' Esta função realiza uma pesquisa no Datajud, permitindo filtrar processos por código de classe e/ou por código de órgão julgador.
#' É possível especificar um tamanho máximo para a amostra de resultados retornados.
#'
#' @param tribunal Identificador do tribunal a ser consultado.
#' @param cliente Objeto criado por `datajud_cliente()`.
#' @param classe_codigo Vetor opcional de códigos de classe para filtrar os processos.
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


  # montar body
  body <- monta_consulta_elasticsearch(
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = round(size)
  )


  # tribunal
  url <- aux_retorna_endpoint(tribunal)

  resposta <- requisitar_api_datajud(cliente, url, body)
  purrr::pluck(resposta, "hits", "hits", .default = list())
}
