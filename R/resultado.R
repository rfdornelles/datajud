# Objeto de resultado das pesquisas gerais do Datajud.

abortar_resposta_pesquisa <- function(mensagem) {
  cli::cli_abort(
    mensagem,
    class = c("datajud_erro_conteudo", "datajud_erro_http")
  )
}

abortar_paginacao_datajud <- function(mensagem) {
  cli::cli_abort(
    mensagem,
    class = c(
      "datajud_erro_paginacao",
      "datajud_erro_conteudo",
      "datajud_erro_http"
    )
  )
}

extrair_ids_hits <- function(hits) {
  ids <- vapply(hits, function(hit) {
    id <- purrr::pluck(hit, "_source", "id", .default = NA_character_)
    if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
      return(NA_character_)
    }
    id
  }, character(1))

  if (anyNA(ids)) {
    abortar_resposta_pesquisa(
      "A resposta da API cont\u00E9m processo sem o campo \u00FAnico id."
    )
  }
  if (anyDuplicated(ids)) {
    abortar_resposta_pesquisa(
      "A resposta da API cont\u00E9m valores duplicados no campo \u00FAnico id."
    )
  }

  ids
}

extrair_total_pesquisa <- function(resposta) {
  total <- purrr::pluck(resposta, "hits", "total", .default = NULL)
  valor <- purrr::pluck(total, "value", .default = NULL)
  relacao <- purrr::pluck(total, "relation", .default = NULL)

  valor_valido <- is.numeric(valor) &&
    length(valor) == 1L &&
    is.finite(valor) &&
    valor >= 0 &&
    valor == floor(valor)
  relacao_valida <- is.character(relacao) &&
    length(relacao) == 1L &&
    !is.na(relacao) &&
    relacao %in% c("eq", "gte")

  if (!valor_valido || !relacao_valida) {
    abortar_resposta_pesquisa(
      "A resposta da API n\u00E3o cont\u00E9m um total de resultados v\u00E1lido."
    )
  }

  list(valor = valor, relacao = relacao)
}

sanitizar_consulta_datajud <- function(consulta) {
  if (!is.list(consulta)) {
    return(consulta)
  }

  nomes <- names(consulta)
  if (!is.null(nomes)) {
    sensiveis <- grepl(
      "^(authorization|api[_-]?key|access[_-]?token|token|senha|secret|credencial)$",
      tolower(nomes)
    )
    consulta <- consulta[!sensiveis]
  }

  consulta[] <- lapply(consulta, sanitizar_consulta_datajud)
  consulta
}

extrair_cursor_resultado <- function(hit) {
  cursor <- purrr::pluck(hit, "sort", .default = NULL)
  if (is.null(cursor)) {
    abortar_resposta_pesquisa(
      paste0(
        "A resposta da API cont\u00E9m cursor de pagina\u00E7\u00E3o ausente ou ",
        "malformado."
      )
    )
  }
  tryCatch(
    normalizar_cursor_datajud(cursor),
    error = function(cnd) {
      abortar_resposta_pesquisa(
        paste0(
          "A resposta da API cont\u00E9m cursor de pagina\u00E7\u00E3o ausente ou ",
          "malformado."
        )
      )
    }
  )
}

construir_datajud_resultado <- function(hits, consulta, metadados) {
  if (!is.list(hits)) {
    abortar_resposta_pesquisa(
      "Os hits do resultado devem ser armazenados em uma lista."
    )
  }
  extrair_ids_hits(hits)
  if (!is.list(consulta)) {
    abortar_resposta_pesquisa(
      "A consulta do resultado deve ser armazenada em uma lista."
    )
  }

  campos <- c(
    "tribunal", "total_valor", "total_relacao", "quantidade_recebida",
    "cursor_utilizado", "proximo_cursor"
  )
  if (!is.list(metadados) || !all(campos %in% names(metadados))) {
    abortar_resposta_pesquisa(
      "Os metadados do resultado est\u00E3o incompletos."
    )
  }
  total_valido <- is.numeric(metadados$total_valor) &&
    length(metadados$total_valor) == 1L &&
    is.finite(metadados$total_valor) &&
    metadados$total_valor >= 0 &&
    metadados$total_valor == floor(metadados$total_valor)
  quantidade_valida <- is.numeric(metadados$quantidade_recebida) &&
    length(metadados$quantidade_recebida) == 1L &&
    identical(
      as.integer(metadados$quantidade_recebida),
      as.integer(length(hits))
    )
  if (!total_valido ||
      !identical(metadados$total_relacao %in% c("eq", "gte"), TRUE) ||
      !quantidade_valida) {
    abortar_resposta_pesquisa(
      paste0(
        "As contagens do resultado s\u00E3o inv\u00E1lidas ou ",
        "incompat\u00EDveis com os hits."
      )
    )
  }
  if (!"pagina" %in% names(metadados)) {
    metadados$pagina <- NA_integer_
  }
  tribunal_valido <- is.character(metadados$tribunal) &&
    length(metadados$tribunal) == 1L &&
    !is.na(metadados$tribunal) &&
    nzchar(metadados$tribunal)
  pagina_valida <- is.numeric(metadados$pagina) &&
    length(metadados$pagina) == 1L &&
    (is.na(metadados$pagina) ||
       (is.finite(metadados$pagina) &&
          metadados$pagina >= 1 &&
          metadados$pagina == floor(metadados$pagina)))
  if (!tribunal_valido || !pagina_valida ||
      metadados$total_valor < length(hits)) {
    abortar_resposta_pesquisa(
      paste0(
        "Os metadados de tribunal, p\u00E1gina ou total do resultado ",
        "s\u00E3o inv\u00E1lidos."
      )
    )
  }
  metadados["cursor_utilizado"] <- list(normalizar_cursor_datajud(
    metadados$cursor_utilizado
  ))
  metadados["proximo_cursor"] <- list(normalizar_cursor_datajud(
    metadados$proximo_cursor
  ))
  cursor_esperado <- if (length(hits) == 0L) {
    NULL
  } else {
    extrair_cursor_resultado(hits[[length(hits)]])
  }
  if (!identical(metadados$proximo_cursor, cursor_esperado)) {
    abortar_resposta_pesquisa(
      paste0(
        "O pr\u00F3ximo cursor do resultado n\u00E3o corresponde ao ",
        "\u00FAltimo hit."
      )
    )
  }

  structure(
    list(
      hits = hits,
      consulta = sanitizar_consulta_datajud(consulta),
      metadados = metadados
    ),
    class = "datajud_resultado"
  )
}

validar_datajud_resultado <- function(x) {
  if (!inherits(x, "datajud_resultado") ||
      !is.list(x) ||
      !all(c("hits", "consulta", "metadados") %in% names(x))) {
    abortar_resposta_pesquisa(
      "O objeto `datajud_resultado` \u00E9 inv\u00E1lido."
    )
  }
  resultado <- construir_datajud_resultado(
    x$hits,
    x$consulta,
    x$metadados
  )
  if (!identical(resultado$consulta, x$consulta)) {
    abortar_resposta_pesquisa(
      "O resultado cont\u00E9m dados sens\u00EDveis em sua consulta."
    )
  }
  invisible(x)
}

novo_datajud_resultado <- function(resposta, tribunal, consulta,
                                   pagina = NULL) {
  hits <- purrr::pluck(resposta, "hits", "hits", .default = NULL)
  if (!is.list(hits)) {
    abortar_resposta_pesquisa(
      "A resposta da API n\u00E3o cont\u00E9m uma lista de resultados v\u00E1lida."
    )
  }

  extrair_ids_hits(hits)
  total <- extrair_total_pesquisa(resposta)
  cursor_utilizado <- purrr::pluck(
    consulta,
    "search_after",
    .default = NULL
  )
  cursor_utilizado <- normalizar_cursor_datajud(cursor_utilizado)
  proximo_cursor <- if (length(hits) == 0L) {
    NULL
  } else {
    extrair_cursor_resultado(hits[[length(hits)]])
  }
  if (!is.null(cursor_utilizado) &&
      identical(cursor_utilizado, proximo_cursor)) {
    abortar_paginacao_datajud(
      paste0(
        "A API repetiu o cursor da p\u00E1gina anterior. ",
        "A pagina\u00E7\u00E3o foi interrompida para evitar um loop infinito."
      )
    )
  }
  if (is.null(pagina)) {
    pagina <- if (is.null(cursor_utilizado)) 1L else NA_integer_
  }

  construir_datajud_resultado(
    hits = hits,
    consulta = consulta,
    metadados = list(
      tribunal = tribunal,
      total_valor = total$valor,
      total_relacao = total$relacao,
      quantidade_recebida = length(hits),
      pagina = pagina,
      cursor_utilizado = cursor_utilizado,
      proximo_cursor = proximo_cursor
    )
  )
}

#' Imprimir um resultado de pesquisa do Datajud
#'
#' @param x Objeto `datajud_resultado`.
#' @param ... Argumentos adicionais, atualmente ignorados.
#'
#' @return O próprio objeto, invisivelmente.
#' @export
print.datajud_resultado <- function(x, ...) {
  validar_datajud_resultado(x)
  total <- if (identical(x$metadados$total_relacao, "gte")) {
    paste("pelo menos", x$metadados$total_valor)
  } else {
    as.character(x$metadados$total_valor)
  }
  cursor <- if (is.null(x$metadados$proximo_cursor)) "n\u00E3o" else "sim"
  pagina_valor <- purrr::pluck(
    x,
    "metadados",
    "pagina",
    .default = NA_integer_
  )
  pagina <- if (!is.numeric(pagina_valor) || length(pagina_valor) != 1L ||
      is.na(pagina_valor)) {
    "n\u00E3o determinada"
  } else {
    as.character(pagina_valor)
  }

  cli::cli_text("<datajud_resultado>")
  cli::cli_text("Tribunal: {x$metadados$tribunal} | P\u00E1gina: {pagina}")
  cli::cli_text(
    "Resultados recebidos: {x$metadados$quantidade_recebida} | Total: {total}"
  )
  cli::cli_text("Pr\u00F3ximo cursor dispon\u00EDvel: {cursor}")
  invisible(x)
}

#' Converter um resultado do Datajud em tibble
#'
#' A conversão cria uma linha por hit e preserva a fonte completa em uma
#' list-column. O campo `id` é a chave única usada pelo pacote;
#' `numero_processo` não é usado como chave.
#'
#' @param x Objeto `datajud_resultado`.
#' @param ... Argumentos adicionais, atualmente ignorados.
#'
#' @return Tibble com as colunas `id`, `numero_processo` e `dados`.
#' @export
as_tibble.datajud_resultado <- function(x, ...) {
  validar_datajud_resultado(x)
  fontes <- lapply(
    x$hits,
    function(hit) purrr::pluck(hit, "_source", .default = list())
  )
  ids <- extrair_ids_hits(x$hits)
  numeros <- vapply(fontes, function(fonte) {
    numero <- purrr::pluck(fonte, "numeroProcesso", .default = NA_character_)
    if (!is.character(numero) || length(numero) != 1L) {
      return(NA_character_)
    }
    numero
  }, character(1))

  tibble::tibble(
    id = ids,
    numero_processo = numeros,
    dados = fontes
  )
}
