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
#' @param ... Compatibilidade temporária com `cliente` na posição antiga. Novas
#'   chamadas devem nomear os argumentos após `tribunal`.
#' @param assunto_codigo Vetor opcional de códigos de assunto.
#' @param classe_codigo Código opcional de uma única classe processual.
#' @param orgao_codigo Vetor opcional de códigos de órgão julgador.
#' @param size Número de resultados da página, entre 1 e 10.000.
#' @param cursor Cursor opaco `search_after` retornado pela página anterior.
#' @param exigir_todos_assuntos Se `TRUE`, exige a presença de todos os assuntos
#'   informados; por padrão, qualquer assunto satisfaz o filtro.
#' @param cliente Objeto opcional criado por [datajud_cliente()]. Quando `NULL`,
#'   um cliente transitório é criado automaticamente.
#'
#' @return Objeto `datajud_resultado` com hits, consulta sanitizada e metadados.
#' @export
#'
#' @examples
#' \dontrun{
#' # Somente assunto ou somente classe
#' por_assunto <- datajud_pesquisar_processos(
#'   "TJSP", assunto_codigo = 899
#' )
#' qualquer_assunto <- datajud_pesquisar_processos(
#'   "TJSP", assunto_codigo = c(1, 2, 3, 4)
#' )
#' todos_assuntos <- datajud_pesquisar_processos(
#'   "TJSP",
#'   assunto_codigo = c(1, 2, 3, 4),
#'   exigir_todos_assuntos = TRUE
#' )
#' por_classe <- datajud_pesquisar_processos(
#'   "TJSP", classe_codigo = 1116
#' )
#' por_orgao <- datajud_pesquisar_processos(
#'   "TJSP", orgao_codigo = 13597
#' )
#'
#' # Vários filtros e um cliente configurado explicitamente
#' combinada <- datajud_pesquisar_processos(
#'   "TJSP",
#'   assunto_codigo = c(899, 900),
#'   classe_codigo = 1116,
#'   orgao_codigo = 13597,
#'   cliente = datajud_cliente(timeout = 60)
#' )
#' tibble::as_tibble(combinada)
#' }
datajud_pesquisar_processos <- function(
    tribunal,
    ...,
    assunto_codigo = NULL,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100L,
    cursor = NULL,
    exigir_todos_assuntos = FALSE,
    cliente = NULL) {
  argumentos <- list(...)
  tribunal <- validar_tribunal_pesquisa(tribunal)
  consulta <- criar_query_datajud(
    assunto_codigo = assunto_codigo,
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = size,
    cursor = cursor,
    exigir_todos_assuntos = exigir_todos_assuntos
  )
  cliente <- resolver_cliente_posicional(
    argumentos,
    cliente,
    "datajud_pesquisar_processos"
  )
  endpoint <- aux_retorna_endpoint(tribunal)
  resposta <- requisitar_api_datajud(cliente, endpoint, consulta)

  novo_datajud_resultado(
    resposta = resposta,
    tribunal = tribunal,
    consulta = consulta
  )
}
