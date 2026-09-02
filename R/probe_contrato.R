# Probe interno e sanitizado do contrato observado da API Pública do Datajud.

probe_requisitar <- function(endpoint, cliente, corpo) {
  tryCatch(
    httr2::request(endpoint) |>
      httr2::req_method("POST") |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_headers_redacted(
        Authorization = paste("APIKey", cliente$api_key)
      ) |>
      httr2::req_user_agent("datajud-contract-probe/1.0") |>
      httr2::req_timeout(seconds = cliente$timeout) |>
      httr2::req_body_json(corpo, auto_unbox = TRUE) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = function(cnd) {
      cli::cli_abort(
        "N\u00E3o foi poss\u00EDvel executar o probe do contrato.",
        class = "datajud_erro_probe_contrato"
      )
    }
  )
}

probe_ler_json <- function(resposta) {
  tipo <- httr2::resp_header(resposta, "content-type", default = "")
  if (!grepl("application/json", tipo, ignore.case = TRUE)) {
    return(NULL)
  }
  tryCatch(
    httr2::resp_body_json(resposta, simplifyVector = FALSE),
    error = function(cnd) NULL
  )
}

probe_consulta_ordenada <- function(campo) {
  item_sort <- stats::setNames(list(list(order = "asc")), campo)
  list(
    size = 1L,
    query = list(term = list("classe.codigo" = 1116L)),
    sort = list(item_sort)
  )
}

executar_probe_contrato <- function(endpoint, cliente) {
  if (!inherits(cliente, "datajud_cliente")) {
    cli::cli_abort("cliente deve ser criado com datajud_cliente().")
  }
  respostas <- list(
    id_keyword = probe_requisitar(
      endpoint,
      cliente,
      probe_consulta_ordenada("id.keyword")
    ),
    timestamp = probe_requisitar(
      endpoint,
      cliente,
      probe_consulta_ordenada("@timestamp")
    ),
    erro = probe_requisitar(
      endpoint,
      cliente,
      list(query = list(operador_inexistente = list(campo = TRUE)))
    )
  )
  corpos <- lapply(respostas, probe_ler_json)
  hit_id <- purrr::pluck(corpos, "id_keyword", "hits", "hits", 1, .default = NULL)
  total <- purrr::pluck(corpos, "id_keyword", "hits", "total", .default = NULL)
  erro <- corpos$erro

  tibble::tibble(
    observado_em_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    status_id_keyword = httr2::resp_status(respostas$id_keyword),
    status_timestamp = httr2::resp_status(respostas$timestamp),
    total_campos = paste(names(total), collapse = ","),
    total_value_tipo = typeof(total$value),
    total_relation = purrr::pluck(
      total,
      "relation",
      .default = NA_character_
    ),
    sort_id_tipo = typeof(purrr::pluck(hit_id, "sort", 1, .default = NULL)),
    id_unico_confere = identical(
      purrr::pluck(hit_id, "_id", .default = NULL),
      purrr::pluck(hit_id, "_source", "id", .default = NULL)
    ),
    status_erro = httr2::resp_status(respostas$erro),
    erro_campos = paste(names(erro), collapse = ","),
    erro_objeto_tipo = typeof(purrr::pluck(erro, "error", .default = NULL))
  )
}

imprimir_probe_contrato <- function(resultado) {
  cli::cli_inform(c(
    "i" = "Probe executado em {resultado$observado_em_utc}.",
    "v" = "Ordena\u00E7\u00E3o id.keyword: HTTP {resultado$status_id_keyword}.",
    "v" = "Ordena\u00E7\u00E3o @timestamp: HTTP {resultado$status_timestamp}.",
    "i" = "Total: campos {resultado$total_campos}; relation={resultado$total_relation}.",
    "i" = "Erro observado: HTTP {resultado$status_erro}; campos {resultado$erro_campos}."
  ))
  invisible(resultado)
}
