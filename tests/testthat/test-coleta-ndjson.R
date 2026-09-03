resposta_coleta_ndjson <- function(ids, cursores, total = length(ids),
                                    relacao = "eq") {
  hits <- Map(function(id, cursor) {
    list(
      `_source` = list(id = id, numeroProcesso = paste0("processo-", id)),
      sort = cursor
    )
  }, ids, cursores)
  list(hits = list(
    total = list(value = as.integer(total), relation = relacao),
    hits = unname(hits)
  ))
}

ler_manifesto_teste <- function(diretorio) {
  jsonlite::read_json(
    file.path(diretorio, "manifesto.json"),
    simplifyVector = FALSE
  )
}

test_that("coleta completa grava NDJSON atômico e retorna apenas caminhos", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  chamadas <- 0L
  consultas <- list()
  resposta <- resposta_coleta_ndjson(
    c("id-1", "id-2"),
    list(list(1000, "id-1"), list(1001, "id-2")),
    total = 2L
  )
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamadas <<- chamadas + 1L
      consultas[[chamadas]] <<- query
      resposta
    },
    .package = "datajud"
  )

  coleta <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 2,
    pausa = 0,
    cliente = cliente
  )

  expect_s3_class(coleta, "datajud_coleta")
  expect_false("hits" %in% names(coleta))
  expect_identical(coleta$metadados$estado, "completa")
  expect_identical(coleta$metadados$registros, 2L)
  expect_identical(coleta$metadados$paginas, 1L)
  expect_length(coleta$arquivos, 1L)
  expect_true(file.exists(coleta$manifesto))
  expect_true(file.exists(coleta$arquivos))
  expect_length(readLines(coleta$arquivos, warn = FALSE), 2L)
  expect_false(any(grepl("^\\.pagina-", list.files(diretorio))))
  expect_identical(
    vapply(consultas[[1]]$sort, names, character(1)),
    c("@timestamp", "id.keyword")
  )

  manifesto <- ler_manifesto_teste(diretorio)
  expect_identical(manifesto$motivo_termino, "total_informado")
  expect_true("proximo_cursor" %in% names(manifesto))
  expect_null(manifesto$proximo_cursor)
  expect_identical(
    unname(tools::md5sum(coleta$arquivos)),
    manifesto$paginas[[1]]$checksum_md5
  )
  conteudo <- paste(readLines(coleta$manifesto, warn = FALSE), collapse = "")
  expect_false(grepl(cliente$api_key, conteudo, fixed = TRUE))
  expect_false(grepl("authorization", conteudo, ignore.case = TRUE))

  repetida <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 2,
    pausa = 0,
    cliente = cliente
  )
  expect_identical(chamadas, 1L)
  expect_identical(repetida$arquivos, coleta$arquivos)
})

test_that("página vazia conclui sem criar arquivo de dados", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      resposta_coleta_ndjson(character(), list(), total = 0L)
    },
    .package = "datajud"
  )

  coleta <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    pausa = 0,
    cliente = cliente
  )
  manifesto <- ler_manifesto_teste(diretorio)

  expect_length(coleta$arquivos, 0L)
  expect_identical(coleta$metadados$estado, "completa")
  expect_identical(coleta$metadados$requisicoes, 1L)
  expect_identical(manifesto$motivo_termino, "pagina_vazia")
  expect_identical(manifesto$pagina_terminal_vazia, 1L)
  expect_true("proximo_cursor" %in% names(manifesto))
})

test_that("limite de registros não grava hits além do solicitado", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    resposta_coleta_ndjson(
      c("id-1", "id-2", "id-3"),
      list(
        list(1000, "id-1"),
        list(1001, "id-2"),
        list(1002, "id-3")
      ),
      total = 10L,
      relacao = "gte"
    ),
    resposta_coleta_ndjson(
      c("id-3", "id-4"),
      list(list(1002, "id-3"), list(1003, "id-4")),
      total = 4L
    )
  )
  consultas <- list()
  chamada <- 0L
  pausas <- numeric()
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamada <<- chamada + 1L
      consultas[[chamada]] <<- query
      respostas[[chamada]]
    },
    aguardar_proxima_pagina = function(pausa) {
      pausas <<- c(pausas, pausa)
      invisible(NULL)
    },
    .package = "datajud"
  )

  limitada <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 3,
    limite_registros = 2,
    pausa = 0.25,
    cliente = cliente
  )
  expect_identical(limitada$metadados$estado, "limite_registros")
  expect_length(readLines(limitada$arquivos[[1]], warn = FALSE), 2L)
  manifesto_limitado <- ler_manifesto_teste(diretorio)
  expect_identical(
    manifesto_limitado$proximo_cursor,
    list(1001L, "id-2")
  )

  concluida <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 3,
    limite_registros = 4,
    pausa = 0.25,
    cliente = cliente
  )
  expect_identical(chamada, 2L)
  expect_identical(pausas, 0.25)
  expect_identical(
    unclass(consultas[[2]]$search_after),
    list(1001L, "id-2")
  )
  expect_identical(concluida$metadados$estado, "completa")
  expect_identical(concluida$metadados$registros, 4L)
  expect_identical(basename(concluida$arquivos), c(
    "pagina-000001.ndjson",
    "pagina-000002.ndjson"
  ))
})

test_that("falha intermediária é registrada e retomada sem duplicar página", {
  diretorio <- withr::local_tempdir()
  chave <- chave_publica_teste()
  cliente <- datajud::datajud_cliente(chave)
  pagina_1 <- resposta_coleta_ndjson(
    "id-1",
    list(list(1000, "id-1")),
    total = 3L,
    relacao = "gte"
  )
  chamada <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamada <<- chamada + 1L
      if (chamada == 1L) {
        return(pagina_1)
      }
      cli::cli_abort(paste("falha simulada", chave))
    },
    aguardar_proxima_pagina = function(pausa) invisible(NULL),
    .package = "datajud"
  )

  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP",
      diretorio,
      assunto_codigo = 899,
      size = 1,
      pausa = 0,
      cliente = cliente
    ),
    "interrompida na página 2",
    class = "datajud_erro_coleta_interrompida"
  )
  parcial <- ler_manifesto_teste(diretorio)
  primeiro_checksum <- parcial$paginas[[1]]$checksum_md5
  expect_identical(parcial$estado, "parcial")
  expect_identical(parcial$falha$pagina, 2L)
  expect_false(grepl(chave, parcial$falha$mensagem, fixed = TRUE))
  expect_identical(parcial$contagens$paginas, 1L)

  consultas_retomada <- list()
  respostas_retomada <- list(
    resposta_coleta_ndjson(
      c("id-2", "id-3"),
      list(list(1001, "id-2"), list(1002, "id-3")),
      total = 3L
    )
  )
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      consultas_retomada[[length(consultas_retomada) + 1L]] <<- query
      respostas_retomada[[length(consultas_retomada)]]
    },
    aguardar_proxima_pagina = function(pausa) invisible(NULL),
    .package = "datajud"
  )
  retomada <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 1,
    pausa = 0,
    cliente = cliente
  )

  expect_identical(retomada$metadados$estado, "completa")
  expect_identical(retomada$metadados$registros, 3L)
  expect_length(retomada$arquivos, 2L)
  expect_identical(
    unclass(consultas_retomada[[1]]$search_after),
    list(1000L, "id-1")
  )
  final <- ler_manifesto_teste(diretorio)
  expect_identical(final$paginas[[1]]$checksum_md5, primeiro_checksum)
  expect_identical(final$contagens$paginas, 2L)
})

test_that("consulta incompatível e checksum alterado falham antes da rede", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  chamadas <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamadas <<- chamadas + 1L
      resposta_coleta_ndjson(
        "id-1",
        list(list(1000, "id-1")),
        total = 10L,
        relacao = "gte"
      )
    },
    .package = "datajud"
  )
  coleta <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 1,
    limite_paginas = 1,
    pausa = 0,
    cliente = cliente
  )
  expect_identical(coleta$metadados$estado, "limite_paginas")

  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP",
      diretorio,
      assunto_codigo = 900,
      size = 1,
      pausa = 0,
      cliente = cliente
    ),
    "incompatível",
    class = "datajud_erro_coleta_incompativel"
  )
  expect_identical(chamadas, 1L)

  writeLines("conteúdo alterado", coleta$arquivos[[1]], useBytes = TRUE)
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP",
      diretorio,
      assunto_codigo = 899,
      size = 1,
      pausa = 0,
      cliente = cliente
    ),
    "checksum inválido",
    class = "datajud_erro_coleta_integridade"
  )
  expect_identical(chamadas, 1L)
})

test_that("cursor não adjacente repetido interrompe um ciclo", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    resposta_coleta_ndjson(
      "id-1", list(list(1000, "id-a")), 10L, "gte"
    ),
    resposta_coleta_ndjson(
      "id-2", list(list(1001, "id-b")), 10L, "gte"
    ),
    resposta_coleta_ndjson(
      "id-3", list(list(1000, "id-a")), 10L, "gte"
    )
  )
  chamada <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamada <<- chamada + 1L
      respostas[[chamada]]
    },
    aguardar_proxima_pagina = function(pausa) invisible(NULL),
    .package = "datajud"
  )

  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP",
      diretorio,
      assunto_codigo = 899,
      size = 1,
      pausa = 0,
      cliente = cliente
    ),
    "cursor já gravado",
    class = "datajud_erro_coleta_interrompida"
  )
  manifesto <- ler_manifesto_teste(diretorio)
  expect_identical(manifesto$estado, "parcial")
  expect_identical(manifesto$falha$pagina, 3L)
  expect_identical(manifesto$contagens$paginas, 2L)
  expect_length(list.files(diretorio, pattern = "\\.ndjson$"), 2L)
})

test_that("processo repetido na fronteira não é gravado novamente", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    resposta_coleta_ndjson(
      c("id-1", "id-2"),
      list(list(1000, "id-1"), list(1001, "id-2")),
      10L,
      "gte"
    ),
    resposta_coleta_ndjson(
      c("id-2", "id-3"),
      list(list(1002, "id-2"), list(1003, "id-3")),
      10L,
      "gte"
    )
  )
  chamada <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamada <<- chamada + 1L
      respostas[[chamada]]
    },
    aguardar_proxima_pagina = function(pausa) invisible(NULL),
    .package = "datajud"
  )

  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP",
      diretorio,
      assunto_codigo = 899,
      size = 2,
      pausa = 0,
      cliente = cliente
    ),
    "repetiu processo.*página anterior",
    class = "datajud_erro_coleta_interrompida"
  )
  manifesto <- ler_manifesto_teste(diretorio)
  expect_identical(manifesto$estado, "parcial")
  expect_identical(manifesto$contagens$paginas, 1L)
  expect_length(list.files(diretorio, pattern = "\\.ndjson$"), 1L)
})

test_that("página órfã atômica é reconciliada antes da retomada", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      resposta_coleta_ndjson(
        "id-1", list(list(1000, "id-1")), 10L, "gte"
      )
    },
    .package = "datajud"
  )
  datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 1,
    limite_paginas = 1,
    pausa = 0,
    cliente = cliente
  )

  escrever <- getFromNamespace("escrever_pagina_ndjson", "datajud")
  hit_orfao <- resposta_coleta_ndjson(
    "id-2", list(list(1001, "id-2")), 10L, "gte"
  )$hits$hits
  escrever(hit_orfao, file.path(diretorio, "pagina-000002.ndjson"))

  consultas <- list()
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      consultas[[length(consultas) + 1L]] <<- query
      resposta_coleta_ndjson(character(), list(), 10L, "gte")
    },
    aguardar_proxima_pagina = function(pausa) invisible(NULL),
    .package = "datajud"
  )
  coleta <- datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 1,
    limite_paginas = 3,
    pausa = 0,
    cliente = cliente
  )
  manifesto <- ler_manifesto_teste(diretorio)

  expect_identical(coleta$metadados$estado, "completa")
  expect_identical(coleta$metadados$registros, 2L)
  expect_identical(coleta$metadados$requisicoes, 3L)
  expect_true(manifesto$paginas[[2]]$recuperada)
  expect_identical(
    unclass(consultas[[1]]$search_after),
    list(1001L, "id-2")
  )
})

test_that("coleta valida diretório, limites, retomada e manifesto", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", character(), assunto_codigo = 899, cliente = cliente
    ),
    "diretorio"
  )
  arquivo <- tempfile()
  writeLines("não é diretório", arquivo)
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", arquivo, assunto_codigo = 899, cliente = cliente
    ),
    "não é uma pasta"
  )
  preenchido <- withr::local_tempdir()
  writeLines("arquivo alheio", file.path(preenchido, "dados.txt"))
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", preenchido, assunto_codigo = 899, cliente = cliente
    ),
    "não está vazio"
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", tempfile(), assunto_codigo = 899,
      limite_registros = 0, cliente = cliente
    ),
    "limite_registros"
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", tempfile(), assunto_codigo = 899,
      limite_paginas = Inf, cliente = cliente
    ),
    "limite_paginas"
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", tempfile(), assunto_codigo = 899,
      pausa = 61, cliente = cliente
    ),
    "pausa"
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", tempfile(), assunto_codigo = 899,
      retomar = NA, cliente = cliente
    ),
    "retomar"
  )

  diretorio <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      resposta_coleta_ndjson(character(), list(), 0L)
    },
    .package = "datajud"
  )
  datajud::datajud_coletar_processos(
    "TJSP", diretorio, assunto_codigo = 899, pausa = 0, cliente = cliente
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", diretorio, assunto_codigo = 899,
      pausa = 0, retomar = FALSE, cliente = cliente
    ),
    "já possui uma coleta"
  )

  manifesto <- ler_manifesto_teste(diretorio)
  manifesto$versao_esquema <- 999L
  jsonlite::write_json(
    manifesto,
    file.path(diretorio, "manifesto.json"),
    auto_unbox = TRUE,
    null = "null"
  )
  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", diretorio, assunto_codigo = 899,
      pausa = 0, cliente = cliente
    ),
    "esquema.*incompatível",
    class = "datajud_erro_coleta_incompativel"
  )
})

test_that("validador rejeita adulterações estruturais do manifesto", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      resposta_coleta_ndjson(
        "id-1", list(list(1000, "id-1")), 10L, "gte"
      )
    },
    .package = "datajud"
  )
  datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 1,
    limite_paginas = 1,
    pausa = 0,
    cliente = cliente
  )
  original <- ler_manifesto_teste(diretorio)
  validar <- getFromNamespace("validar_manifesto_coleta", "datajud")
  validar_copia <- function(alterar, padrao) {
    manifesto <- unserialize(serialize(original, NULL))
    manifesto <- alterar(manifesto)
    expect_error(
      validar(
        manifesto,
        diretorio,
        "TJSP",
        original$consulta_hash
      ),
      padrao,
      class = "datajud_erro_coleta_integridade"
    )
  }

  validar_copia(function(x) {
    x$paginas <- "inválido"
    x
  }, "estrutura inválida")
  validar_copia(function(x) {
    x$estado <- "desconhecido"
    x
  }, "estado de coleta inválido")
  validar_copia(function(x) {
    x$consulta$query <- list()
    x
  }, "consulta armazenada")
  validar_copia(function(x) {
    x$contagens$registros <- 2L
    x
  }, "contagens do manifesto")
  validar_copia(function(x) {
    x$contagens$requisicoes <- 0L
    x
  }, "contagens do manifesto")
  validar_copia(function(x) {
    x$paginas[[1]]$numero <- 2L
    x
  }, "numeração.*não é sequencial")
  validar_copia(function(x) {
    x$paginas[[1]]$cursor_utilizado <- list(999, "outro")
    x
  }, "cadeia de cursores")
  validar_copia(function(x) {
    x$paginas[[1]]$recuperada <- NA
    x
  }, "metadados de página inválidos")
  validar_copia(function(x) {
    x$proximo_cursor <- list("timestamp", "id")
    x
  }, "cursor inválido")
  validar_copia(function(x) {
    x$paginas[[1]]$arquivo <- "../pagina-000001.ndjson"
    x
  }, "caminho de página inválido")

  completa <- unserialize(serialize(original, NULL))
  completa$estado <- "completa"
  expect_error(
    validar(completa, diretorio, "TJSP", original$consulta_hash),
    "coleta completa.*próximo cursor",
    class = "datajud_erro_coleta_integridade"
  )
})

test_that("leitura e escrita defensivas rejeitam arquivos inválidos", {
  ler_manifesto <- getFromNamespace("ler_manifesto_coleta", "datajud")
  ler_pagina <- getFromNamespace("ler_pagina_ndjson", "datajud")
  escrever <- getFromNamespace("escrever_pagina_ndjson", "datajud")

  manifesto_invalido <- tempfile(fileext = ".json")
  writeLines("{", manifesto_invalido)
  expect_error(
    ler_manifesto(manifesto_invalido),
    "JSON válido",
    class = "datajud_erro_coleta_integridade"
  )

  pagina_vazia <- tempfile(fileext = ".ndjson")
  file.create(pagina_vazia)
  expect_error(
    ler_pagina(pagina_vazia),
    "vazia ou malformada",
    class = "datajud_erro_coleta_integridade"
  )
  pagina_invalida <- tempfile(fileext = ".ndjson")
  writeLines("{", pagina_invalida)
  expect_error(
    ler_pagina(pagina_invalida),
    "JSON inválido",
    class = "datajud_erro_coleta_integridade"
  )

  diretorio <- withr::local_tempdir()
  existente <- file.path(diretorio, "pagina-000001.ndjson")
  writeLines("ocupado", existente)
  expect_error(
    escrever(list(list()), existente),
    "já existe",
    class = "datajud_erro_coleta_integridade"
  )
})

test_that("coleta completa rejeita página órfã posterior", {
  diretorio <- withr::local_tempdir()
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      resposta_coleta_ndjson(character(), list(), 0L)
    },
    .package = "datajud"
  )
  datajud::datajud_coletar_processos(
    "TJSP", diretorio, assunto_codigo = 899, pausa = 0, cliente = cliente
  )
  writeLines("{}", file.path(diretorio, "pagina-000001.ndjson"))

  expect_error(
    datajud::datajud_coletar_processos(
      "TJSP", diretorio, assunto_codigo = 899, pausa = 0, cliente = cliente
    ),
    "coleta completa.*páginas órfãs",
    class = "datajud_erro_coleta_integridade"
  )
})
