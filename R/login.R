# Chave pública vigente publicada pelo CNJ em 01/09/2026.
DATAJUD_CHAVE_PUBLICA_ATUAL <- "cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw=="

#' Obtém a chave pública vigente na Wiki do CNJ
#'
#' @param url URL da página oficial de acesso da API.
#' @return A chave pública como caractere de comprimento um.
#' @export
obter_chave_publica_cnj <- function(
    url = "https://datajud-wiki.cnj.jus.br/api-publica/acesso/") {
  resposta <- httr::GET(url, httr::timeout(20))
  if (httr::status_code(resposta) != 200L) {
    cli::cli_abort("N\u00E3o foi poss\u00EDvel obter a chave p\u00FAblica do CNJ (HTTP {httr::status_code(resposta)}).")
  }

  texto <- resposta |>
    httr::content(as = "text", encoding = "UTF-8") |>
    xml2::read_html() |>
    xml2::xml_text()
  chave <- stringr::str_match(texto, "APIKey\\s+([A-Za-z0-9+/=]{20,})")[, 2]
  if (is.na(chave) || !nzchar(chave)) {
    cli::cli_abort("A p\u00E1gina do CNJ n\u00E3o cont\u00E9m uma chave p\u00FAblica reconhec\u00EDvel.")
  }
  chave
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
  if (!nzchar(chave)) {
    chave <- tryCatch(
      obter_chave_publica_cnj(),
      error = function(e) DATAJUD_CHAVE_PUBLICA_ATUAL
    )
  }
  endereco <- if (is.null(email)) Sys.getenv("DATAJUD_EMAIL", unset = "") else email
  if (!is.character(chave) || length(chave) != 1L || !nzchar(chave)) {
    cli::cli_abort("Informe api_key ou configure DATAJUD_API_KEY.")
  }
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
  texto <- "Pacote datajud para R"
  if (nzchar(cliente$email)) texto <- paste0(texto, " - e-mail: ", cliente$email)
  httr::user_agent(texto)
}

validar_cliente <- function(cliente) {
  if (!inherits(cliente, "datajud_cliente")) {
    cli::cli_abort("cliente deve ser criado com datajud_cliente().")
  }
  invisible(cliente)
}
