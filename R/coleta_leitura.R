# Abertura, impressão e leitura seletiva de coletas em disco.

validar_datajud_coleta <- function(x) {
  campos <- c(
    "diretorio", "manifesto", "arquivos", "consulta", "paginas", "metadados"
  )
  metadados <- c(
    "tribunal", "versao_esquema", "estado", "registros", "paginas",
    "requisicoes", "proximo_cursor", "falha", "criado_em", "atualizado_em"
  )
  if (!inherits(x, "datajud_coleta") ||
      !is.list(x) ||
      !all(campos %in% names(x)) ||
      !is.list(x$paginas) ||
      !is.list(x$metadados) ||
      !all(metadados %in% names(x$metadados))) {
    abortar_coleta_datajud(
      "O objeto `datajud_coleta` \u00E9 inv\u00E1lido.",
      "datajud_erro_coleta_integridade"
    )
  }
  if ("hits" %in% names(x) ||
      length(x$arquivos) != length(x$paginas) ||
      !identical(as.integer(x$metadados$paginas), as.integer(length(x$paginas)))) {
    abortar_coleta_datajud(
      "As p\u00E1ginas do objeto `datajud_coleta` s\u00E3o incompat\u00EDveis.",
      "datajud_erro_coleta_integridade"
    )
  }
  if (!identical(sanitizar_consulta_datajud(x$consulta), x$consulta)) {
    abortar_coleta_datajud(
      "A coleta cont\u00E9m dados sens\u00EDveis em sua consulta.",
      "datajud_erro_coleta_integridade"
    )
  }
  invisible(x)
}

validar_numero_pagina_coleta <- function(pagina, total) {
  valido <- is.numeric(pagina) &&
    !is.complex(pagina) &&
    length(pagina) == 1L &&
    is.finite(pagina) &&
    pagina >= 1 &&
    pagina == floor(pagina)
  if (!valido) {
    cli::cli_abort("{.arg pagina} deve ser um inteiro positivo.")
  }
  pagina <- as.integer(pagina)
  if (pagina > total) {
    cli::cli_abort(
      paste0(
        "A coleta possui {total} p\u00E1gina{?s}; ",
        "a p\u00E1gina {pagina} n\u00E3o existe."
      ),
      class = "datajud_erro_coleta_pagina"
    )
  }
  pagina
}

#' Abrir uma coleta do Datajud gravada em disco
#'
#' Lê e valida a estrutura do manifesto, os caminhos e a ausência de páginas
#' órfãs. A função não lê o conteúdo dos arquivos NDJSON nem recalcula todos os
#' checksums. Para validar e materializar somente uma página, use
#' [datajud_ler_pagina()].
#'
#' @param diretorio Diretório que contém `manifesto.json` e as páginas NDJSON.
#'
#' @return Objeto `datajud_coleta`, sem hits em memória e sem credenciais.
#' @export
#'
#' @examples
#' \dontrun{
#' coleta <- datajud_abrir_coleta("dados/tjsp-assunto-899")
#' coleta
#' pagina_1 <- datajud_ler_pagina(coleta, 1)
#' }
datajud_abrir_coleta <- function(diretorio) {
  valido <- is.character(diretorio) &&
    length(diretorio) == 1L &&
    !is.na(diretorio) &&
    nzchar(trimws(diretorio))
  if (!valido || !dir.exists(diretorio)) {
    cli::cli_abort(
      "{.arg diretorio} deve indicar uma coleta existente."
    )
  }
  diretorio <- normalizePath(diretorio, winslash = "/", mustWork = TRUE)
  caminho_manifesto <- file.path(
    diretorio,
    NOME_MANIFESTO_COLETA_DATAJUD
  )
  if (!file.exists(caminho_manifesto)) {
    abortar_coleta_datajud(
      paste0(
        "O diret\u00F3rio n\u00E3o cont\u00E9m o arquivo ",
        "{.file manifesto.json}."
      ),
      "datajud_erro_coleta_integridade"
    )
  }
  manifesto <- ler_manifesto_coleta(caminho_manifesto)
  tribunal <- tryCatch(
    validar_tribunal_pesquisa(manifesto$tribunal),
    error = function(cnd) {
      abortar_coleta_datajud(
        "O manifesto cont\u00E9m um tribunal inv\u00E1lido.",
        "datajud_erro_coleta_integridade",
        parent = cnd
      )
    }
  )
  manifesto <- validar_manifesto_coleta(
    manifesto,
    diretorio,
    tribunal,
    manifesto$consulta_hash,
    verificar_checksums = FALSE
  )
  orfaos <- listar_paginas_orfas(manifesto, diretorio)
  if (length(orfaos) > 0L) {
    abortar_coleta_datajud(
      paste0(
        "A coleta cont\u00E9m p\u00E1gina NDJSON n\u00E3o registrada no manifesto. ",
        "Retome-a com {.fun datajud_coletar_processos} antes de abri-la."
      ),
      "datajud_erro_coleta_integridade"
    )
  }
  resultado_coleta_datajud(manifesto, diretorio, caminho_manifesto)
}

#' Ler somente uma página de uma coleta do Datajud
#'
#' Materializa os hits de um único arquivo NDJSON e os devolve como
#' `datajud_resultado`. Nenhuma outra página de hits é aberta. O objeto pode ser
#' convertido com `tibble::as_tibble()` ou fornecido diretamente a
#' [datajud_ler_processo()] e [datajud_ler_movimentacoes()].
#'
#' @param coleta Objeto criado por [datajud_coletar_processos()] ou
#'   [datajud_abrir_coleta()].
#' @param pagina Número da página a ler, começando em 1.
#'
#' @return Objeto `datajud_resultado` com os hits da página solicitada.
#' @export
#'
#' @examples
#' \dontrun{
#' coleta <- datajud_abrir_coleta("dados/tjsp-assunto-899")
#' pagina <- datajud_ler_pagina(coleta, 1)
#' tibble::as_tibble(pagina)
#' datajud_ler_processo(pagina)
#' }
datajud_ler_pagina <- function(coleta, pagina) {
  validar_datajud_coleta(coleta)
  pagina <- validar_numero_pagina_coleta(pagina, length(coleta$paginas))
  metadados_pagina <- coleta$paginas[[pagina]]
  caminho <- validar_checksum_pagina(
    metadados_pagina,
    coleta$diretorio
  )
  hits <- ler_pagina_ndjson(caminho)
  ids <- extrair_ids_hits(hits)

  consistente <- identical(
    as.integer(length(hits)),
    as.integer(metadados_pagina$registros)
  ) &&
    identical(ids[[1]], metadados_pagina$primeiro_id) &&
    identical(ids[[length(ids)]], metadados_pagina$ultimo_id) &&
    identical(
      extrair_cursor_resultado(hits[[length(hits)]]),
      normalizar_cursor_manifesto(
        metadados_pagina$proximo_cursor,
        paste0("p\u00E1gina ", pagina)
      )
    )
  if (!consistente) {
    abortar_coleta_datajud(
      paste0(
        "O conte\u00FAdo da p\u00E1gina n\u00E3o corresponde aos metadados ",
        "do manifesto."
      ),
      "datajud_erro_coleta_integridade"
    )
  }

  consulta <- coleta$consulta
  cursor_utilizado <- normalizar_cursor_manifesto(
    metadados_pagina$cursor_utilizado,
    paste0("p\u00E1gina ", pagina)
  )
  if (!is.null(cursor_utilizado)) {
    consulta$search_after <- I(cursor_utilizado)
  }
  total_valor <- metadados_pagina$total_valor
  total_relacao <- metadados_pagina$total_relacao
  if (!is.numeric(total_valor) || length(total_valor) != 1L ||
      !is.finite(total_valor) || total_valor < 0) {
    total_valor <- coleta$metadados$registros
    total_relacao <- if (identical(coleta$metadados$estado, "completa")) {
      "eq"
    } else {
      "gte"
    }
  }

  construir_datajud_resultado(
    hits = hits,
    consulta = consulta,
    metadados = list(
      tribunal = coleta$metadados$tribunal,
      total_valor = total_valor,
      total_relacao = total_relacao,
      quantidade_recebida = length(hits),
      pagina = pagina,
      cursor_utilizado = cursor_utilizado,
      proximo_cursor = metadados_pagina$proximo_cursor,
      origem = "coleta",
      arquivo = caminho
    )
  )
}

#' Imprimir o resumo de uma coleta do Datajud
#'
#' O método usa somente os metadados que já estão no objeto. Nenhum arquivo de
#' hits é aberto ou materializado durante a impressão.
#'
#' @param x Objeto `datajud_coleta`.
#' @param ... Argumentos adicionais, atualmente ignorados.
#'
#' @return O próprio objeto, invisivelmente.
#' @export
print.datajud_coleta <- function(x, ...) {
  validar_datajud_coleta(x)
  cli::cli_text("<datajud_coleta>")
  cli::cli_text(
    "Tribunal: {x$metadados$tribunal} | Estado: {x$metadados$estado}"
  )
  cli::cli_text(
    "P\u00E1ginas: {x$metadados$paginas} | Registros: {x$metadados$registros}"
  )
  cli::cli_text("Diret\u00F3rio: {x$diretorio}")
  invisible(x)
}
