validar_tribunal_pesquisa <- function(tribunal) {
  if (!is.character(tribunal) || length(tribunal) != 1L ||
      is.na(tribunal) || !nzchar(trimws(tribunal))) {
    cli::cli_abort("{.arg tribunal} deve ser uma sigla n\u00E3o vazia.")
  }

  toupper(trimws(tribunal))
}

executar_pesquisa_datajud <- function(tribunal, consulta, cliente,
                                      pagina = NULL) {
  endpoint <- aux_retorna_endpoint(tribunal)
  resposta <- requisitar_api_datajud(cliente, endpoint, consulta)

  novo_datajud_resultado(
    resposta = resposta,
    tribunal = tribunal,
    consulta = consulta,
    pagina = pagina
  )
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
#' @param cursor Cursor `search_after` da página anterior, com o timestamp
#'   numérico e o `id` textual retornados pela ordenação composta.
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
  executar_pesquisa_datajud(
    tribunal = tribunal,
    consulta = consulta,
    cliente = cliente
  )
}

validar_pausa_paginacao <- function(pausa) {
  valido <- is.numeric(pausa) &&
    length(pausa) == 1L &&
    is.finite(pausa) &&
    pausa >= 0 &&
    pausa <= 60
  if (!valido) {
    cli::cli_abort(
      "{.arg pausa} deve ser um n\u00FAmero entre 0 e 60 segundos."
    )
  }
  pausa
}

aguardar_proxima_pagina <- function(pausa) {
  if (pausa > 0) {
    Sys.sleep(pausa)
  }
  invisible(NULL)
}

validar_resultado_paginacao <- function(resultado) {
  valido <- inherits(resultado, "datajud_resultado") &&
    is.list(resultado$hits) &&
    is.list(resultado$consulta) &&
    is.list(resultado$metadados) &&
    "proximo_cursor" %in% names(resultado$metadados) &&
    is.character(resultado$metadados$tribunal) &&
    length(resultado$metadados$tribunal) == 1L &&
    !is.na(resultado$metadados$tribunal) &&
    nzchar(resultado$metadados$tribunal)
  if (!valido) {
    cli::cli_abort(
      "{.arg resultado} deve ser criado por datajud_pesquisar_processos()."
    )
  }
  invisible(resultado)
}

#' Pesquisar a página seguinte de um resultado
#'
#' Faz no máximo uma nova requisição, usando a consulta e o cursor preservados
#' em um [datajud_pesquisar_processos()] anterior. A função é sequencial,
#' aguarda `pausa` antes da requisição e interrompe cursores ou IDs repetidos.
#' Quando não há próximo cursor, retorna `NULL` sem acessar a rede.
#'
#' @param resultado Objeto `datajud_resultado` da página anterior.
#' @param pausa Segundos de espera antes da próxima requisição, entre 0 e 60.
#' @param cliente Objeto opcional criado por [datajud_cliente()]. Quando `NULL`,
#'   um cliente transitório é criado automaticamente.
#'
#' @return Um novo `datajud_resultado` ou `NULL` quando não há cursor.
#' @export
#'
#' @examples
#' \dontrun{
#' cliente <- datajud_cliente()
#' pagina_1 <- datajud_pesquisar_processos(
#'   "TJSP",
#'   assunto_codigo = 899,
#'   size = 100,
#'   cliente = cliente
#' )
#' pagina_2 <- datajud_pesquisar_proxima_pagina(
#'   pagina_1,
#'   pausa = 0.5,
#'   cliente = cliente
#' )
#' }
datajud_pesquisar_proxima_pagina <- function(
    resultado,
    pausa = 0.1,
    cliente = NULL) {
  validar_resultado_paginacao(resultado)
  pausa <- validar_pausa_paginacao(pausa)
  if (!is.null(cliente)) {
    validar_cliente(cliente)
  }

  cursor <- resultado$metadados$proximo_cursor
  if (is.null(cursor)) {
    return(NULL)
  }
  cursor <- normalizar_cursor_datajud(cursor)
  cliente <- resolver_cliente_datajud(cliente)
  consulta <- resultado$consulta
  consulta$search_after <- I(cursor)
  pagina_atual <- resultado$metadados$pagina
  proxima_pagina <- if (is.integer(pagina_atual) &&
      length(pagina_atual) == 1L && !is.na(pagina_atual)) {
    pagina_atual + 1L
  } else {
    NA_integer_
  }

  aguardar_proxima_pagina(pausa)
  proximo_resultado <- executar_pesquisa_datajud(
    tribunal = resultado$metadados$tribunal,
    consulta = consulta,
    cliente = cliente,
    pagina = proxima_pagina
  )
  ids_repetidos <- intersect(
    extrair_ids_hits(resultado$hits),
    extrair_ids_hits(proximo_resultado$hits)
  )
  if (length(ids_repetidos) > 0L) {
    abortar_paginacao_datajud(
      paste0(
        "A p\u00E1gina seguinte repetiu processo(s) da p\u00E1gina anterior. ",
        "A pagina\u00E7\u00E3o foi interrompida para evitar duplica\u00E7\u00E3o."
      )
    )
  }

  proximo_resultado
}
