criar_pagina_paginacao <- function(ids, cursores, total = length(ids)) {
  hits <- Map(function(id, cursor) {
    list(
      `_source` = list(
        id = id,
        numeroProcesso = paste0("processo-", id)
      ),
      sort = cursor
    )
  }, ids, cursores)

  list(hits = list(
    total = list(value = as.integer(total), relation = "eq"),
    hits = unname(hits)
  ))
}

test_that("duas páginas usam cursor composto mesmo com timestamp empatado", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    criar_pagina_paginacao(
      c("id-1", "id-2"),
      list(list(1000, "id-1"), list(1000, "id-2")),
      total = 4L
    ),
    criar_pagina_paginacao(
      c("id-3", "id-4"),
      list(list(1000, "id-3"), list(1001, "id-4")),
      total = 4L
    )
  )
  consultas <- list()
  pausa_recebida <- NULL
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      consultas[[length(consultas) + 1L]] <<- query
      respostas[[length(consultas)]]
    },
    aguardar_proxima_pagina = function(pausa) {
      pausa_recebida <<- pausa
      invisible(NULL)
    },
    .package = "datajud"
  )

  pagina_1 <- datajud::datajud_pesquisar_processos(
    "TJSP",
    assunto_codigo = 899,
    size = 2,
    cliente = cliente
  )
  pagina_2 <- datajud::datajud_pesquisar_proxima_pagina(
    pagina_1,
    pausa = 0.25,
    cliente = cliente
  )

  expect_length(consultas, 2L)
  expect_identical(pausa_recebida, 0.25)
  expect_identical(
    unclass(consultas[[2]]$search_after),
    list(1000, "id-2")
  )
  expect_identical(pagina_1$metadados$pagina, 1L)
  expect_null(pagina_1$metadados$cursor_utilizado)
  expect_identical(pagina_2$metadados$pagina, 2L)
  expect_identical(
    pagina_2$metadados$cursor_utilizado,
    list(1000, "id-2")
  )
  expect_identical(
    vapply(pagina_2$hits, function(hit) hit$`_source`$id, character(1)),
    c("id-3", "id-4")
  )
})

test_that("cursor repetido interrompe a paginação", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    criar_pagina_paginacao("id-1", list(list(1000, "id-1")), 2L),
    criar_pagina_paginacao("id-2", list(list(1000, "id-1")), 2L)
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

  pagina_1 <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )
  expect_error(
    datajud::datajud_pesquisar_proxima_pagina(
      pagina_1, pausa = 0, cliente = cliente
    ),
    "loop infinito",
    class = "datajud_erro_paginacao"
  )
})

test_that("processo repetido entre páginas interrompe a paginação", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    criar_pagina_paginacao("id-1", list(list(1000, "id-1")), 2L),
    criar_pagina_paginacao("id-1", list(list(1001, "id-1")), 2L)
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

  pagina_1 <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )
  expect_error(
    datajud::datajud_pesquisar_proxima_pagina(
      pagina_1, pausa = 0, cliente = cliente
    ),
    "duplicação",
    class = "datajud_erro_paginacao"
  )
})

test_that("página vazia encerra normalmente sem nova requisição", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  resposta_vazia <- criar_pagina_paginacao(character(), list(), 0L)
  chamadas <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamadas <<- chamadas + 1L
      resposta_vazia
    },
    .package = "datajud"
  )

  pagina <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )
  seguinte <- datajud::datajud_pesquisar_proxima_pagina(
    pagina, pausa = 0, cliente = cliente
  )

  expect_identical(chamadas, 1L)
  expect_null(pagina$metadados$proximo_cursor)
  expect_null(seguinte)
})

test_that("página final vazia é retornada e depois encerra", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  respostas <- list(
    criar_pagina_paginacao("id-1", list(list(1000, "id-1")), 1L),
    criar_pagina_paginacao(character(), list(), 1L)
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

  pagina_1 <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )
  pagina_2 <- datajud::datajud_pesquisar_proxima_pagina(
    pagina_1, pausa = 0, cliente = cliente
  )
  fim <- datajud::datajud_pesquisar_proxima_pagina(
    pagina_2, pausa = 0, cliente = cliente
  )

  expect_s3_class(pagina_2, "datajud_resultado")
  expect_length(pagina_2$hits, 0L)
  expect_identical(pagina_2$metadados$pagina, 2L)
  expect_null(pagina_2$metadados$proximo_cursor)
  expect_null(fim)
  expect_identical(chamada, 2L)
})

test_that("cursores malformados falham antes da rede", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  invalidos <- list(
    list(),
    list(1000),
    list(1000, "id", "extra"),
    list("1000", "id"),
    list(1000, ""),
    list(1000, NA_character_),
    environment()
  )

  for (cursor in invalidos) {
    expect_error(
      criar(assunto_codigo = 899, cursor = cursor),
      "timestamp numérico e o id textual"
    )
  }
})

test_that("cursor ausente no hit é erro de conteúdo", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  resposta <- criar_pagina_paginacao("id-1", list(list(1000, "id-1")))
  resposta$hits$hits[[1]]$sort <- NULL
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) resposta,
    .package = "datajud"
  )

  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP", assunto_codigo = 899, cliente = cliente
    ),
    "cursor de paginação",
    class = "datajud_erro_conteudo"
  )

  resposta$hits$hits[[1]]$sort <- list("1000", "id-1")
  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP", assunto_codigo = 899, cliente = cliente
    ),
    "cursor de paginação",
    class = "datajud_erro_conteudo"
  )
})

test_that("valida resultado, pausa e cliente antes de continuar", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  pagina <- structure(list(
    hits = list(),
    consulta = list(),
    metadados = list(tribunal = "TJSP", proximo_cursor = NULL, pagina = 1L)
  ), class = "datajud_resultado")

  expect_error(
    datajud::datajud_pesquisar_proxima_pagina(list()),
    "datajud_pesquisar_processos"
  )
  expect_error(
    datajud::datajud_pesquisar_proxima_pagina(pagina, pausa = -1),
    "pausa"
  )
  expect_error(
    datajud::datajud_pesquisar_proxima_pagina(
      pagina, pausa = 0, cliente = list()
    ),
    "datajud_cliente"
  )
  expect_null(datajud::datajud_pesquisar_proxima_pagina(
    pagina, pausa = 0, cliente = cliente
  ))
})
