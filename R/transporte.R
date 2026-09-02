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

status_transitorio_datajud <- function(resp) {
  httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
}

abortar_status_http <- function(status, autenticada = FALSE) {
  if (status == 400L) {
    mensagem <- if (autenticada) {
      "A API rejeitou a consulta (HTTP 400). Revise os filtros e os tipos informados."
    } else {
      "O servi\u00E7o remoto rejeitou a requisi\u00E7\u00E3o (HTTP 400)."
    }
    classe <- "datajud_erro_requisicao"
  } else if (status %in% c(401L, 403L)) {
    mensagem <- if (autenticada) {
      paste0(
        "A API recusou a chave p\u00FAblica (HTTP ", status, "). ",
        "Reconfigure DATAJUD_API_KEY ou consulte a chave vigente na Wiki do CNJ."
      )
    } else {
      paste0("O servi\u00E7o remoto recusou o acesso (HTTP ", status, ").")
    }
    classe <- "datajud_erro_autenticacao"
  } else if (status == 404L) {
    mensagem <- paste0(
      "O recurso solicitado n\u00E3o foi encontrado (HTTP 404). ",
      "Confirme o endere\u00E7o e os par\u00E2metros informados."
    )
    classe <- "datajud_erro_nao_encontrado"
  } else if (status == 429L) {
    mensagem <- paste0(
      "O limite de requisi\u00E7\u00F5es foi atingido (HTTP 429). ",
      "Aguarde antes de tentar novamente."
    )
    classe <- "datajud_erro_limite"
  } else if (status >= 500L && status < 600L) {
    mensagem <- paste0(
      "O servi\u00E7o remoto est\u00E1 temporariamente indispon\u00EDvel (HTTP ",
      status, "). Tente novamente mais tarde."
    )
    classe <- "datajud_erro_servidor"
  } else {
    mensagem <- paste0("A requisi\u00E7\u00E3o HTTP falhou com status ", status, ".")
    classe <- "datajud_erro_http"
  }

  cli::cli_abort(mensagem, class = unique(c(classe, "datajud_erro_http")))
}

validar_tipo_conteudo <- function(resposta, esperado, contexto) {
  tipo <- httr2::resp_header(resposta, "content-type", default = "")
  if (!nzchar(tipo) || !grepl(esperado, tipo, ignore.case = TRUE)) {
    cli::cli_abort(
      "{contexto} retornou um tipo de conte\u00FAdo inesperado.",
      class = c("datajud_erro_conteudo", "datajud_erro_http")
    )
  }
  invisible(resposta)
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
      httr2::req_user_agent(cliente_user_agent(cliente)) |>
      httr2::req_retry(
        max_tries = cliente$max_tentativas,
        is_transient = status_transitorio_datajud,
        after = httr2::resp_retry_after
      )
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
  resposta <- tryCatch(
    httr2::req_perform(requisicao),
    error = function(cnd) {
      cli::cli_abort(
        "N\u00E3o foi poss\u00EDvel concluir a requisi\u00E7\u00E3o HTTP.",
        class = c("datajud_erro_conexao", "datajud_erro_http")
      )
    }
  )
  status <- httr2::resp_status(resposta)

  if (status < 200L || status >= 300L) {
    abortar_status_http(status, autenticada = !is.null(cliente))
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
  validar_tipo_conteudo(resposta, "application/json", "A API do Datajud")

  tryCatch(
    httr2::resp_body_json(
      resposta,
      check_type = FALSE,
      simplifyVector = FALSE
    ),
    error = function(cnd) {
      cli::cli_abort(
        "A API do Datajud retornou um corpo JSON inv\u00E1lido.",
        class = c("datajud_erro_conteudo", "datajud_erro_http")
      )
    }
  )
}
