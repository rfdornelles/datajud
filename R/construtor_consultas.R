# Construtor puro das consultas Elasticsearch enviadas ao Datajud.

validar_codigos_consulta <- function(codigos, argumento) {
  if (is.null(codigos)) {
    return(NULL)
  }

  valido <- is.numeric(codigos) &&
    !is.complex(codigos) &&
    !is.object(codigos) &&
    length(codigos) > 0L &&
    all(is.finite(codigos)) &&
    all(codigos > 0) &&
    all(codigos == floor(codigos))

  if (!valido) {
    cli::cli_abort(
      "{.arg {argumento}} deve conter um ou mais c\u00F3digos inteiros positivos."
    )
  }

  unique(unname(codigos))
}

validar_size_consulta <- function(size) {
  valido <- is.numeric(size) &&
    !is.complex(size) &&
    !is.object(size) &&
    length(size) == 1L &&
    is.finite(size) &&
    size == floor(size) &&
    size >= 1 &&
    size <= 10000

  if (!valido) {
    cli::cli_abort("{.arg size} deve ser um inteiro entre 1 e 10000.")
  }

  as.integer(size)
}

validar_ordenacao_consulta <- function(ordenacao) {
  campos <- c("@timestamp", "id.keyword")
  valido <- is.character(ordenacao) &&
    length(ordenacao) == length(campos) &&
    !anyNA(ordenacao) &&
    identical(unname(ordenacao), campos)

  if (!valido) {
    cli::cli_abort(
      "{.arg ordenacao} deve ser {.val @timestamp} seguido de {.val id.keyword}."
    )
  }

  unname(ordenacao)
}

filtro_terms_datajud <- function(campo, codigos) {
  list(terms = stats::setNames(list(I(codigos)), campo))
}

validar_exigir_todos <- function(valor, argumento) {
  if (!is.logical(valor) || length(valor) != 1L || is.na(valor)) {
    cli::cli_abort("{.arg {argumento}} deve ser `TRUE` ou `FALSE`.")
  }

  valor
}

#' Criar uma consulta estruturada para a API Pública do Datajud
#'
#' Construtor interno e puro. Códigos dentro de uma categoria são
#' combinados com OR; categorias diferentes são combinadas com AND.
#'
#' @param assunto_codigo,orgao_codigo Vetores numéricos de códigos inteiros
#'   positivos.
#' @param classe_codigo Código numérico único de classe processual.
#' @param size Quantidade de resultados, entre 1 e 10.000.
#' @param cursor Cursor opaco `search_after` devolvido pela API.
#' @param ordenacao Campos de ordenação estável confirmados no contrato.
#' @param exigir_todos_assuntos Se `TRUE`, cria um filtro para cada assunto.
#'
#' @return Lista pronta para serialização JSON.
#' @keywords internal
criar_query_datajud <- function(
    assunto_codigo = NULL,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100L,
    cursor = NULL,
    ordenacao = c("@timestamp", "id.keyword"),
    exigir_todos_assuntos = FALSE) {
  assunto_codigo <- validar_codigos_consulta(
    assunto_codigo,
    "assunto_codigo"
  )
  classe_codigo <- validar_codigos_consulta(classe_codigo, "classe_codigo")
  orgao_codigo <- validar_codigos_consulta(orgao_codigo, "orgao_codigo")
  if (!is.null(classe_codigo) && length(classe_codigo) != 1L) {
    cli::cli_abort("{.arg classe_codigo} deve conter um \u00FAnico c\u00F3digo.")
  }
  size <- validar_size_consulta(size)
  ordenacao <- validar_ordenacao_consulta(ordenacao)

  exigir_todos_assuntos <- validar_exigir_todos(
    exigir_todos_assuntos,
    "exigir_todos_assuntos"
  )

  if (all(vapply(
    list(assunto_codigo, classe_codigo, orgao_codigo),
    is.null,
    logical(1)
  ))) {
    cli::cli_abort("Informe ao menos um filtro de assunto, classe ou \u00F3rg\u00E3o.")
  }

  if (!is.null(cursor) &&
      (!(is.atomic(cursor) || is.list(cursor)) || length(cursor) == 0L)) {
    cli::cli_abort("{.arg cursor} deve ser um vetor ou lista n\u00E3o vazia.")
  }

  filtros <- list()
  if (!is.null(assunto_codigo)) {
    if (exigir_todos_assuntos) {
      filtros_assunto <- lapply(
        assunto_codigo,
        function(codigo) filtro_terms_datajud("assuntos.codigo", codigo)
      )
      filtros <- append(filtros, filtros_assunto)
    } else {
      filtros <- append(
        filtros,
        list(filtro_terms_datajud("assuntos.codigo", assunto_codigo))
      )
    }
  }
  if (!is.null(classe_codigo)) {
    filtros <- append(
      filtros,
      list(filtro_terms_datajud("classe.codigo", classe_codigo))
    )
  }
  if (!is.null(orgao_codigo)) {
    filtros <- append(
      filtros,
      list(filtro_terms_datajud("orgaoJulgador.codigo", orgao_codigo))
    )
  }

  consulta <- list(
    size = size,
    query = list(bool = list(filter = filtros)),
    sort = lapply(
      ordenacao,
      function(campo) stats::setNames(list(list(order = "asc")), campo)
    )
  )

  if (!is.null(cursor)) {
    consulta$search_after <- I(unname(cursor))
  }

  consulta
}
