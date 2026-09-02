# Transporte HTTP interno do pacote.

serializar_query_datajud <- function(query) {
  if (!is.list(query)) {
    cli::cli_abort("A consulta deve ser representada por uma lista R.")
  }

  jsonlite::toJSON(
    query,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

criar_requisicao_http <- function(url, metodo = "GET", cliente = NULL,
                                  corpo = NULL, timeout = NULL) {
  if (!is.character(url) || length(url) != 1L || is.na(url) || !nzchar(url)) {
    cli::cli_abort("A URL da requisi\u00E7\u00E3o deve ser um texto n\u00E3o vazio.")
  }

  if (!is.character(metodo) || length(metodo) != 1L || is.na(metodo)) {
    cli::cli_abort("O m\u00E9todo HTTP deve ser um texto de comprimento um.")
  }

  requisicao <- httr2::request(url) |>
    httr2::req_method(toupper(metodo))

  if (!is.null(cliente)) {
    validar_cliente(cliente)
    requisicao <- requisicao |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_headers_redacted(
        Authorization = paste("APIKey", cliente$api_key)
      ) |>
      httr2::req_user_agent(cliente_user_agent(cliente))
    if (is.null(timeout)) {
      timeout <- cliente$timeout
    }
  }

  if (!is.null(timeout)) {
    requisicao <- httr2::req_timeout(requisicao, seconds = timeout)
  }

  if (!is.null(corpo)) {
    requisicao <- httr2::req_body_raw(
      requisicao,
      body = charToRaw(serializar_query_datajud(corpo)),
      type = "application/json"
    )
  }

  httr2::req_error(requisicao, is_error = function(resp) FALSE)
}

executar_requisicao_http <- function(url, metodo = "GET", cliente = NULL,
                                     corpo = NULL, timeout = NULL) {
  requisicao <- criar_requisicao_http(
    url = url,
    metodo = metodo,
    cliente = cliente,
    corpo = corpo,
    timeout = timeout
  )
  resposta <- httr2::req_perform(requisicao)
  status <- httr2::resp_status(resposta)

  if (status < 200L || status >= 300L) {
    cli::cli_abort("A requisi\u00E7\u00E3o ao Datajud falhou com status HTTP {status}.")
  }

  resposta
}

requisitar_api_datajud <- function(cliente, endpoint, query) {
  resposta <- executar_requisicao_http(
    url = endpoint,
    metodo = "POST",
    cliente = cliente,
    corpo = query
  )

  tryCatch(
    httr2::resp_body_json(
      resposta,
      check_type = FALSE,
      simplifyVector = FALSE
    ),
    error = function(cnd) {
      cli::cli_abort(
        "A API do Datajud retornou um corpo JSON inv\u00E1lido.",
        parent = cnd
      )
    }
  )
}
