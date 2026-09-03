# Coleta incremental em arquivos NDJSON.

VERSAO_ESQUEMA_COLETA_DATAJUD <- 1L
NOME_MANIFESTO_COLETA_DATAJUD <- "manifesto.json"

abortar_coleta_datajud <- function(mensagem, classe = "datajud_erro_coleta",
                                   parent = NULL) {
  cli::cli_abort(
    mensagem,
    class = unique(c(classe, "datajud_erro_coleta")),
    parent = parent,
    .envir = parent.frame()
  )
}

validar_limite_coleta <- function(valor, argumento) {
  valido <- is.numeric(valor) &&
    !is.complex(valor) &&
    !is.object(valor) &&
    length(valor) == 1L &&
    is.finite(valor) &&
    valor >= 1 &&
    valor <= .Machine$integer.max &&
    valor == floor(valor)
  if (!valido) {
    cli::cli_abort(
      "{.arg {argumento}} deve ser um inteiro positivo."
    )
  }
  as.integer(valor)
}

validar_retomar_coleta <- function(retomar) {
  if (!is.logical(retomar) || length(retomar) != 1L || is.na(retomar)) {
    cli::cli_abort("{.arg retomar} deve ser `TRUE` ou `FALSE`.")
  }
  retomar
}

preparar_diretorio_coleta <- function(diretorio) {
  valido <- is.character(diretorio) &&
    length(diretorio) == 1L &&
    !is.na(diretorio) &&
    nzchar(trimws(diretorio))
  if (!valido) {
    cli::cli_abort("{.arg diretorio} deve ser um caminho n\u00E3o vazio.")
  }
  if (file.exists(diretorio) && !dir.exists(diretorio)) {
    abortar_coleta_datajud(
      "O caminho informado em {.arg diretorio} existe e n\u00E3o \u00E9 uma pasta."
    )
  }
  if (!dir.exists(diretorio)) {
    criado <- dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
    if (!criado) {
      abortar_coleta_datajud(
        "N\u00E3o foi poss\u00EDvel criar o {.arg diretorio} da coleta."
      )
    }
  }
  normalizePath(diretorio, winslash = "/", mustWork = TRUE)
}

agora_coleta_datajud <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

serializar_objeto_coleta <- function(objeto, pretty = FALSE) {
  jsonlite::toJSON(
    objeto,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = pretty
  )
}

md5_texto_coleta <- function(texto) {
  caminho <- tempfile("datajud-hash-")
  on.exit(unlink(caminho), add = TRUE)
  writeBin(charToRaw(enc2utf8(texto)), caminho)
  unname(tools::md5sum(caminho))
}

hash_consulta_coleta <- function(consulta) {
  consulta |>
    sanitizar_consulta_datajud() |>
    serializar_objeto_coleta() |>
    md5_texto_coleta()
}

salvar_manifesto_coleta <- function(manifesto, caminho) {
  temporario <- tempfile(".manifesto-", tmpdir = dirname(caminho))
  on.exit(unlink(temporario), add = TRUE)
  jsonlite::write_json(
    manifesto,
    path = temporario,
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA,
    pretty = TRUE
  )

  renomeado <- file.rename(temporario, caminho)
  if (!renomeado) {
    copiado <- file.copy(temporario, caminho, overwrite = TRUE)
    if (!copiado) {
      abortar_coleta_datajud(
        "N\u00E3o foi poss\u00EDvel atualizar o manifesto da coleta."
      )
    }
  }
  invisible(caminho)
}

ler_manifesto_coleta <- function(caminho) {
  tryCatch(
    jsonlite::read_json(caminho, simplifyVector = FALSE),
    error = function(cnd) {
      abortar_coleta_datajud(
        "O manifesto da coleta n\u00E3o cont\u00E9m JSON v\u00E1lido.",
        "datajud_erro_coleta_integridade",
        parent = cnd
      )
    }
  )
}

validar_checksum_pagina <- function(pagina, diretorio) {
  arquivo <- pagina$arquivo
  valido <- is.character(arquivo) &&
    length(arquivo) == 1L &&
    !is.na(arquivo) &&
    identical(basename(arquivo), arquivo) &&
    grepl("^pagina-[0-9]{6,}\\.ndjson$", arquivo)
  if (!valido) {
    abortar_coleta_datajud(
      "O manifesto cont\u00E9m um caminho de p\u00E1gina inv\u00E1lido.",
      "datajud_erro_coleta_integridade"
    )
  }
  caminho <- file.path(diretorio, arquivo)
  checksum <- pagina$checksum_md5
  checksum_valido <- is.character(checksum) &&
    length(checksum) == 1L &&
    !is.na(checksum) &&
    grepl("^[a-f0-9]{32}$", checksum)
  if (!file.exists(caminho) || !checksum_valido ||
      !identical(unname(tools::md5sum(caminho)), checksum)) {
    abortar_coleta_datajud(
      paste0(
        "A p\u00E1gina ", arquivo,
        " est\u00E1 ausente ou possui checksum inv\u00E1lido."
      ),
      "datajud_erro_coleta_integridade"
    )
  }
  invisible(caminho)
}

normalizar_cursor_manifesto <- function(cursor, contexto) {
  if (is.null(cursor)) {
    return(NULL)
  }
  tryCatch(
    normalizar_cursor_datajud(cursor),
    error = function(cnd) {
      abortar_coleta_datajud(
        paste0("O manifesto cont\u00E9m cursor inv\u00E1lido em ", contexto, "."),
        "datajud_erro_coleta_integridade",
        parent = cnd
      )
    }
  )
}

validar_pagina_manifesto <- function(pagina, indice, diretorio,
                                      cursor_esperado) {
  obrigatorios <- c(
    "numero", "arquivo", "registros", "primeiro_id", "ultimo_id",
    "cursor_utilizado", "proximo_cursor", "checksum_md5", "recuperada"
  )
  if (!is.list(pagina) || !all(obrigatorios %in% names(pagina))) {
    abortar_coleta_datajud(
      "O manifesto cont\u00E9m metadados de p\u00E1gina incompletos.",
      "datajud_erro_coleta_integridade"
    )
  }
  if (!is.numeric(pagina$numero) || length(pagina$numero) != 1L ||
      !identical(as.integer(pagina$numero), as.integer(indice))) {
    abortar_coleta_datajud(
      "A numera\u00E7\u00E3o das p\u00E1ginas no manifesto n\u00E3o \u00E9 sequencial.",
      "datajud_erro_coleta_integridade"
    )
  }
  registros <- pagina$registros
  if (!is.numeric(registros) || length(registros) != 1L ||
      !is.finite(registros) || registros < 1 || registros != floor(registros)) {
    abortar_coleta_datajud(
      "O manifesto cont\u00E9m uma contagem de registros inv\u00E1lida.",
      "datajud_erro_coleta_integridade"
    )
  }
  ids_validos <- vapply(
    pagina[c("primeiro_id", "ultimo_id")],
    function(id) {
      is.character(id) && length(id) == 1L && !is.na(id) && nzchar(id)
    },
    logical(1)
  )
  if (!all(ids_validos) ||
      !is.logical(pagina$recuperada) ||
      length(pagina$recuperada) != 1L ||
      is.na(pagina$recuperada)) {
    abortar_coleta_datajud(
      "O manifesto cont\u00E9m metadados de p\u00E1gina inv\u00E1lidos.",
      "datajud_erro_coleta_integridade"
    )
  }
  cursor_utilizado <- normalizar_cursor_manifesto(
    pagina$cursor_utilizado,
    paste0("cursor_utilizado da p\u00E1gina ", indice)
  )
  proximo_cursor <- normalizar_cursor_manifesto(
    pagina$proximo_cursor,
    paste0("proximo_cursor da p\u00E1gina ", indice)
  )
  if (!identical(cursor_utilizado, cursor_esperado) ||
      is.null(proximo_cursor)) {
    abortar_coleta_datajud(
      "A cadeia de cursores das p\u00E1ginas no manifesto \u00E9 inv\u00E1lida.",
      "datajud_erro_coleta_integridade"
    )
  }
  validar_checksum_pagina(pagina, diretorio)
  list(registros = as.integer(registros), proximo_cursor = proximo_cursor)
}

validar_manifesto_coleta <- function(manifesto, diretorio, tribunal,
                                      consulta_hash) {
  obrigatorios <- c(
    "versao_esquema", "tribunal", "consulta", "consulta_hash",
    "estado", "paginas", "contagens", "proximo_cursor"
  )
  if (!is.list(manifesto) ||
      !all(obrigatorios %in% names(manifesto)) ||
      !is.list(manifesto$paginas) ||
      !is.list(manifesto$contagens) ||
      !all(c("registros", "paginas", "requisicoes") %in%
           names(manifesto$contagens))) {
    abortar_coleta_datajud(
      "O manifesto da coleta possui estrutura inv\u00E1lida.",
      "datajud_erro_coleta_integridade"
    )
  }
  if (!identical(as.integer(manifesto$versao_esquema),
                 VERSAO_ESQUEMA_COLETA_DATAJUD)) {
    abortar_coleta_datajud(
      "A vers\u00E3o do esquema do manifesto \u00E9 incompat\u00EDvel com o pacote.",
      "datajud_erro_coleta_incompativel"
    )
  }
  if (!identical(manifesto$tribunal, tribunal) ||
      !identical(manifesto$consulta_hash, consulta_hash)) {
    abortar_coleta_datajud(
      paste0(
        "A consulta informada \u00E9 incompat\u00EDvel com a coleta existente. ",
        "Use outro diret\u00F3rio."
      ),
      "datajud_erro_coleta_incompativel"
    )
  }
  if (!identical(hash_consulta_coleta(manifesto$consulta),
                 manifesto$consulta_hash)) {
    abortar_coleta_datajud(
      "A consulta armazenada no manifesto foi alterada.",
      "datajud_erro_coleta_integridade"
    )
  }
  estados <- c(
    "em_andamento", "parcial", "completa",
    "limite_registros", "limite_paginas"
  )
  if (!is.character(manifesto$estado) ||
      length(manifesto$estado) != 1L ||
      !manifesto$estado %in% estados) {
    abortar_coleta_datajud(
      "O manifesto cont\u00E9m um estado de coleta inv\u00E1lido.",
      "datajud_erro_coleta_integridade"
    )
  }

  paginas <- manifesto$paginas
  cursor_esperado <- NULL
  registros_paginas <- integer(length(paginas))
  for (indice in seq_along(paginas)) {
    validada <- validar_pagina_manifesto(
      paginas[[indice]],
      indice,
      diretorio,
      cursor_esperado
    )
    registros_paginas[[indice]] <- validada$registros
    cursor_esperado <- validada$proximo_cursor
  }
  registros <- sum(registros_paginas)
  paginas_registradas <- manifesto$contagens$paginas
  registros_registrados <- manifesto$contagens$registros
  requisicoes_registradas <- manifesto$contagens$requisicoes
  if (!identical(as.integer(paginas_registradas), as.integer(length(paginas))) ||
      !identical(as.integer(registros_registrados), as.integer(registros)) ||
      !is.numeric(requisicoes_registradas) ||
      length(requisicoes_registradas) != 1L ||
      !is.finite(requisicoes_registradas) ||
      requisicoes_registradas < length(paginas) ||
      requisicoes_registradas != floor(requisicoes_registradas)) {
    abortar_coleta_datajud(
      "As contagens do manifesto n\u00E3o correspondem \u00E0s p\u00E1ginas gravadas.",
      "datajud_erro_coleta_integridade"
    )
  }
  proximo_cursor <- normalizar_cursor_manifesto(
    manifesto$proximo_cursor,
    "proximo_cursor global"
  )
  if (identical(manifesto$estado, "completa")) {
    if (!is.null(proximo_cursor)) {
      abortar_coleta_datajud(
        "Uma coleta completa n\u00E3o pode manter pr\u00F3ximo cursor.",
        "datajud_erro_coleta_integridade"
      )
    }
  } else if (!identical(proximo_cursor, cursor_esperado)) {
    abortar_coleta_datajud(
      "O cursor global n\u00E3o corresponde \u00E0 \u00FAltima p\u00E1gina gravada.",
      "datajud_erro_coleta_integridade"
    )
  }
  manifesto
}
