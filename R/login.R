# Chave pública vigente publicada pelo CNJ em 01/09/2026.
DATAJUD_CHAVE_PUBLICA_ATUAL <- "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="

chave_publica_valida <- function(chave) {
  formato_valido <- is.character(chave) &&
    length(chave) == 1L &&
    !is.na(chave) &&
    grepl("^[A-Za-z0-9_+/-]{20,}={0,2}$", chave)

  if (!formato_valido) {
    return(FALSE)
  }

  possui_padding <- grepl("=", chave, fixed = TRUE)
  if (possui_padding) {
    nchar(chave) %% 4L == 0L
  } else {
    nchar(chave) %% 4L != 1L
  }
}

validar_chave_publica <- function(chave) {
  if (!chave_publica_valida(chave)) {
    cli::cli_abort(
      "api_key n\u00E3o possui o formato esperado para a chave p\u00FAblica do CNJ.",
      class = "datajud_erro_credencial"
    )
  }
  invisible(chave)
}

extrair_chave_publica_cnj <- function(html) {
  pagina <- tryCatch(
    xml2::read_html(html),
    error = function(cnd) {
      cli::cli_abort(
        "N\u00E3o foi poss\u00EDvel interpretar a p\u00E1gina p\u00FAblica do CNJ.",
        class = "datajud_erro_credencial"
      )
    }
  )
  no_chave <- xml2::xml_find_first(
    pagina,
    "//strong[normalize-space(.)='APIKey atual']/following::strong[1]"
  )
  chave <- xml2::xml_text(no_chave, trim = TRUE)

  if (is.na(chave) || !nzchar(chave)) {
    texto <- xml2::xml_text(pagina)
    chave <- stringr::str_match(
      texto,
      "Authorization:\\s*APIKey\\s+([A-Za-z0-9_+/-]{20,}={1,2})"
    )[, 2]
  }
  if (!chave_publica_valida(chave)) {
    cli::cli_abort(
      "A p\u00E1gina do CNJ n\u00E3o cont\u00E9m uma chave p\u00FAblica reconhec\u00EDvel.",
      class = "datajud_erro_credencial"
    )
  }
  chave
}

#' Obtém a chave pública vigente na Wiki do CNJ
#'
#' @param url URL da página oficial de acesso da API.
#' @return A chave pública como caractere de comprimento um.
#' @export
obter_chave_publica_cnj <- function(
    url = "https://datajud-wiki.cnj.jus.br/api-publica/acesso/") {
  resposta <- executar_requisicao_http(url = url, timeout = 20)
  validar_tipo_conteudo(resposta, "text/html", "A p\u00E1gina p\u00FAblica do CNJ")
  extrair_chave_publica_cnj(httr2::resp_body_string(resposta))
}

#' Cria um cliente explícito para a API do Datajud
#'
#' @param api_key Chave pública da API. Quando omitida, usa `DATAJUD_API_KEY`,
#'   consulta a Wiki oficial e, se necessário, recorre à cópia vigente incluída
#'   no pacote.
#' @param email E-mail opcional para identificação no User-Agent. Quando omitido,
#'   usa `DATAJUD_EMAIL`.
#' @param timeout Tempo máximo de espera de cada requisição, em segundos.
#' @param max_tentativas Número máximo de tentativas reservado ao cliente HTTP.
#' @return Um objeto da classe `datajud_cliente`.
#' @export
datajud_cliente <- function(api_key = NULL, email = NULL, timeout = 30,
                            max_tentativas = 3) {
  chave <- if (is.null(api_key)) Sys.getenv("DATAJUD_API_KEY", unset = "") else api_key
  chave_ausente <- is.character(chave) && length(chave) == 1L &&
    !is.na(chave) && !nzchar(chave)
  if (chave_ausente) {
    chave <- tryCatch(
      obter_chave_publica_cnj(),
      error = function(e) DATAJUD_CHAVE_PUBLICA_ATUAL
    )
  }
  endereco <- if (is.null(email)) Sys.getenv("DATAJUD_EMAIL", unset = "") else email
  validar_chave_publica(chave)
  if (!is.character(endereco) || length(endereco) != 1L ||
      (nzchar(endereco) && !grepl("^[^@[:space:]]+@[^@[:space:]]+\\.[^@[:space:]]+$", endereco))) {
    cli::cli_abort("O e-mail informado \u00E9 inv\u00E1lido.")
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
    cli::cli_abort("timeout deve ser um n\u00FAmero positivo.")
  }
  if (!is.numeric(max_tentativas) || length(max_tentativas) != 1L ||
      !is.finite(max_tentativas) || max_tentativas < 1 || max_tentativas != round(max_tentativas)) {
    cli::cli_abort("max_tentativas deve ser um inteiro positivo.")
  }
  structure(list(api_key = chave, email = endereco, timeout = timeout,
                 max_tentativas = as.integer(max_tentativas)),
            class = "datajud_cliente")
}

#' @export
print.datajud_cliente <- function(x, ...) {
  cli::cli_text("<datajud_cliente>")
  cli::cli_text("E-mail: {if (nzchar(x$email)) x$email else 'n\u00E3o informado'}")
  cli::cli_text("Timeout: {x$timeout}s | Tentativas: {x$max_tentativas}")
  invisible(x)
}

cliente_user_agent <- function(cliente) {
  texto <- paste0("datajud/", utils::packageVersion("datajud"), " R/", getRversion())
  if (nzchar(cliente$email)) texto <- paste0(texto, " - e-mail: ", cliente$email)
  texto
}

validar_cliente <- function(cliente) {
  if (!inherits(cliente, "datajud_cliente")) {
    cli::cli_abort("cliente deve ser criado com datajud_cliente().")
  }
  invisible(cliente)
}
