resposta_http_pesquisa <- function(corpo) {
  httr2::response(
    status_code = 200,
    headers = list(`Content-Type` = "application/json"),
    body = charToRaw(jsonlite::toJSON(corpo, auto_unbox = TRUE, null = "null"))
  )
}

test_that("pesquisa simulada aceita cada filtro e todas as combinações", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  consultas <- list()
  httr2::local_mocked_responses(function(req) {
    consultas[[length(consultas) + 1L]] <<- jsonlite::fromJSON(
      rawToChar(req$body$data),
      simplifyVector = FALSE
    )
    resposta_http_pesquisa(corpo)
  })
  combinacoes <- expand.grid(
    assunto = c(FALSE, TRUE),
    classe = c(FALSE, TRUE),
    orgao = c(FALSE, TRUE)
  )
  combinacoes <- combinacoes[rowSums(combinacoes) > 0L, , drop = FALSE]

  resultados <- lapply(seq_len(nrow(combinacoes)), function(i) {
    linha <- combinacoes[i, ]
    datajud::datajud_pesquisar_processos(
      tribunal = "tjsp",
      assunto_codigo = if (linha$assunto) c(899, 900) else NULL,
      classe_codigo = if (linha$classe) 1116 else NULL,
      orgao_codigo = if (linha$orgao) c(13597, 13598) else NULL,
      cliente = cliente
    )
  })

  expect_length(resultados, 7L)
  expect_true(all(vapply(resultados, inherits, logical(1), "datajud_resultado")))
  for (i in seq_along(consultas)) {
    expect_length(
      consultas[[i]]$query$bool$filter,
      sum(combinacoes[i, ])
    )
  }
  expect_identical(
    consultas[[7]]$query$bool$filter[[1]]$terms$`assuntos.codigo`,
    list(899L, 900L)
  )
  expect_identical(
    consultas[[7]]$query$bool$filter[[2]]$terms$`classe.codigo`,
    list(1116L)
  )
  expect_identical(
    consultas[[7]]$query$bool$filter[[3]]$terms$`orgaoJulgador.codigo`,
    list(13597L, 13598L)
  )
})

test_that("resultado preserva total eq, hits e próximo cursor", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))

  resultado <- datajud::datajud_pesquisar_processos(
    " TJSP ",
    assunto_codigo = 899,
    size = 2,
    cliente = cliente
  )

  expect_s3_class(resultado, "datajud_resultado")
  expect_length(resultado$hits, 2L)
  expect_identical(resultado$metadados$tribunal, "TJSP")
  expect_identical(resultado$metadados$total_valor, 2L)
  expect_identical(resultado$metadados$total_relacao, "eq")
  expect_identical(resultado$metadados$quantidade_recebida, 2L)
  expect_identical(
    resultado$metadados$proximo_cursor,
    list(1681366085560, "processo-exemplo-2")
  )
})

test_that("total gte é preservado sem ser tratado como valor exato", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  corpo$hits$total <- list(value = 10000L, relation = "gte")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))

  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP", classe_codigo = 1116, cliente = cliente
  )

  expect_identical(resultado$metadados$total_valor, 10000L)
  expect_identical(resultado$metadados$total_relacao, "gte")
  saida <- testthat::capture_messages(print(resultado))
  expect_true(any(grepl("pelo menos 10000", saida, fixed = TRUE)))
})

test_that("resposta vazia retorna objeto válido com zero hits", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_vazia.json")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))

  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP", orgao_codigo = 13597, cliente = cliente
  )
  tabela <- tibble::as_tibble(resultado)

  expect_s3_class(resultado, "datajud_resultado")
  expect_length(resultado$hits, 0L)
  expect_identical(resultado$metadados$total_valor, 0L)
  expect_identical(resultado$metadados$quantidade_recebida, 0L)
  expect_null(resultado$metadados$proximo_cursor)
  expect_s3_class(tabela, "tbl_df")
  expect_named(tabela, c("id", "numero_processo", "dados"))
  expect_equal(nrow(tabela), 0L)
})

test_that("impressão é compacta e conversão usa id como chave", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))
  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )

  saida <- testthat::capture_messages(retorno <- print(resultado))
  tabela <- tibble::as_tibble(resultado)

  expect_identical(retorno, resultado)
  expect_lte(length(saida), 4L)
  expect_true(any(grepl("Resultados recebidos: 2", saida, fixed = TRUE)))
  expect_identical(tabela$id, c("processo-exemplo-1", "processo-exemplo-2"))
  expect_identical(
    tabela$numero_processo,
    c("1000001-23.2024.8.26.0100", "1000002-34.2024.8.26.0100")
  )
  expect_type(tabela$dados, "list")

  resultado_sem_pagina <- resultado
  resultado_sem_pagina$metadados$pagina <- NULL
  saida_sem_pagina <- testthat::capture_messages(print(resultado_sem_pagina))
  expect_true(any(grepl("Página: não determinada", saida_sem_pagina)))

  resultado$hits[[1]]$`_source`$numeroProcesso <- 123
  expect_true(is.na(tibble::as_tibble(resultado)$numero_processo[[1]]))
})

test_that("resultado não contém credenciais nem altera o ambiente global", {
  chave <- chave_publica_teste(2)
  cliente <- datajud::datajud_cliente(chave)
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))
  antes <- ls(envir = .GlobalEnv, all.names = TRUE)

  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP", assunto_codigo = 899, cliente = cliente
  )
  depois <- ls(envir = .GlobalEnv, all.names = TRUE)
  serializado <- jsonlite::toJSON(unclass(resultado), auto_unbox = TRUE)
  nomes_recursivos <- function(x) {
    if (!is.list(x)) return(character())
    c(names(x), unlist(lapply(x, nomes_recursivos), use.names = FALSE))
  }

  expect_identical(depois, antes)
  expect_false(grepl(chave, serializado, fixed = TRUE))
  expect_false(any(grepl(
    "authorization|api[_-]?key|token|senha|secret|credencial",
    nomes_recursivos(resultado),
    ignore.case = TRUE
  )))
})

test_that("sanitização remove campos sensíveis recursivamente", {
  sanitizar <- getFromNamespace("sanitizar_consulta_datajud", "datajud")
  chave <- chave_publica_teste()
  consulta <- list(
    query = list(term = list(campo = 1)),
    Authorization = chave,
    opcoes = list(api_key = chave, seguro = TRUE)
  )

  resultado <- sanitizar(consulta)

  expect_null(resultado$Authorization)
  expect_null(resultado$opcoes$api_key)
  expect_true(resultado$opcoes$seguro)
  expect_false(grepl(
    chave,
    jsonlite::toJSON(resultado, auto_unbox = TRUE),
    fixed = TRUE
  ))
})

test_that("consulta armazena cursor e filtros, mas não o cliente", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  httr2::local_mocked_responses(function(req) resposta_http_pesquisa(corpo))
  cursor <- list(1681366085550, "processo-anterior")

  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP",
    assunto_codigo = c(899, 900),
    cursor = cursor,
    exigir_todos_assuntos = TRUE,
    cliente = cliente
  )

  expect_identical(unclass(resultado$consulta$search_after), cursor)
  expect_length(resultado$consulta$query$bool$filter, 2L)
  expect_false("cliente" %in% names(resultado))
})

test_that("respostas inconsistentes falham com erro de conteúdo", {
  criar_resultado <- getFromNamespace("novo_datajud_resultado", "datajud")
  consulta <- list(query = list())
  resposta <- carregar_fixture("resposta_pesquisa_processos.json")

  sem_hits <- resposta
  sem_hits$hits$hits <- "inválido"
  expect_error(
    criar_resultado(sem_hits, "TJSP", consulta),
    class = "datajud_erro_conteudo"
  )

  sem_total <- resposta
  sem_total$hits$total <- NULL
  expect_error(
    criar_resultado(sem_total, "TJSP", consulta),
    class = "datajud_erro_conteudo"
  )

  sem_id <- resposta
  sem_id$hits$hits[[1]]$`_source`$id <- NULL
  expect_error(
    criar_resultado(sem_id, "TJSP", consulta),
    "campo único id"
  )

  id_duplicado <- resposta
  id_duplicado$hits$hits[[2]]$`_source`$id <- "processo-exemplo-1"
  expect_error(
    criar_resultado(id_duplicado, "TJSP", consulta),
    "duplicados"
  )
})

test_that("pesquisa valida tribunal e cliente antes da rede", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())

  expect_error(
    datajud::datajud_pesquisar_processos(
      "", assunto_codigo = 1, cliente = cliente
    ),
    "tribunal"
  )
  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP", assunto_codigo = 1, cliente = list()
    ),
    "datajud_cliente"
  )
  expect_error(
    datajud::datajud_pesquisar_processos("TJSP", cliente = cliente),
    "ao menos um filtro"
  )
  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP", classe_codigo = c(1, 2), cliente = cliente
    ),
    "único código"
  )
})
