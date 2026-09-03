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

  structure(
    list(
      hits = hits,
      consulta = sanitizar_consulta_datajud(consulta),
      metadados = list(
        tribunal = tribunal,
        total_valor = total$valor,
        total_relacao = total$relacao,
        quantidade_recebida = length(hits),
        pagina = pagina,
        cursor_utilizado = cursor_utilizado,
        proximo_cursor = proximo_cursor
      )
    ),
    class = "datajud_resultado"
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
