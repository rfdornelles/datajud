test_that("construtor retorna lista e preserva arrays JSON", {
  montar <- getFromNamespace("monta_consulta_elasticsearch", "datajud")
  serializar <- getFromNamespace("serializar_query_datajud", "datajud")

  consulta <- montar(
    classe_codigo = 1116,
    orgao_codigo = c(123, 456),
    size = 25
  )
  json <- jsonlite::fromJSON(serializar(consulta), simplifyVector = FALSE)

  expect_type(consulta, "list")
  expect_equal(json$size, 25)
  expect_equal(json$query$bool$filter[[1]]$terms$`classe.codigo`, list(1116))
  expect_equal(json$query$bool$filter[[2]]$terms$`orgaoJulgador.codigo`, list(123, 456))

  unitario <- montar(classe_codigo = 1116)
  json_unitario <- jsonlite::fromJSON(serializar(unitario), simplifyVector = FALSE)
  expect_identical(json_unitario$query$bool$filter[[1]]$terms$`classe.codigo`, list(1116L))
})

test_that("construtor aceita cada filtro isoladamente", {
  montar <- getFromNamespace("monta_consulta_elasticsearch", "datajud")

  expect_no_error(montar(classe_codigo = 1))
  expect_no_error(montar(orgao_codigo = 2))
  expect_error(montar(), "ao menos um filtro")
})

test_that("pesquisa valida argumentos antes da rede", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())

  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente
  ), "Nenhum")
  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente, classe_codigo = 1, size = 0
  ), "entre 1 e 10000")
  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente, classe_codigo = 1, size = 1.5
  ), "inteiro")
  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = list(), classe_codigo = 1
  ), "datajud_cliente")
})

test_that("pesquisa usa transporte comum e não devolve credenciais", {
  chave <- chave_publica_teste(2)
  cliente <- datajud::datajud_cliente(chave)
  requisicao_capturada <- NULL
  corpo <- jsonlite::toJSON(list(
    hits = list(hits = list(list(`_source` = list(id = "x"))))
  ), auto_unbox = TRUE)

  httr2::local_mocked_responses(function(req) {
    requisicao_capturada <<- req
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(corpo)
    )
  })

  resultado <- datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente, classe_codigo = 1116
  )

  expect_length(resultado, 1L)
  expect_identical(requisicao_capturada$method, "POST")
  expect_match(requisicao_capturada$url, "api_publica_tjsp/_search$")
  expect_identical(requisicao_capturada$body$content_type, "application/json")
  expect_identical(requisicao_capturada$policies$retry_max_tries, 3L)

  corpo_enviado <- jsonlite::fromJSON(
    rawToChar(requisicao_capturada$body$data), simplifyVector = FALSE
  )
  expect_identical(
    corpo_enviado$query$bool$filter[[1]]$terms$`classe.codigo`, list(1116L)
  )
  expect_false(grepl(chave, jsonlite::toJSON(resultado), fixed = TRUE))
  expect_false(any(grepl(
    chave,
    capture.output(print(requisicao_capturada)),
    fixed = TRUE
  )))
})

test_that("consulta por processo usa resposta HTTP simulada", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  processo <- "0000102-03.2004.8.26.0000"
  corpo <- jsonlite::toJSON(list(
    hits = list(hits = list(list(`_source` = list(
      numeroProcesso = gsub("[^0-9]", "", processo)
    ))))
  ), auto_unbox = TRUE)

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(corpo)
    )
  })

  requisitar <- getFromNamespace("datajud_requisition", "datajud")
  resultado <- requisitar(processo, cliente, "TJSP")

  expect_equal(resultado$`_source`$numeroProcesso, gsub("[^0-9]", "", processo))
})

test_that("transporte valida consultas e corpos JSON", {
  serializar <- getFromNamespace("serializar_query_datajud", "datajud")
  requisitar <- getFromNamespace("requisitar_api_datajud", "datajud")
  cliente <- datajud::datajud_cliente(chave_publica_teste())

  expect_error(serializar("JSON montado manualmente"), "lista R")

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw("não é JSON")
    )
  })

  expect_error(
    requisitar(cliente, "https://exemplo.org", list(query = list())),
    "JSON inválido"
  )
})

test_that("obtenção da chave também usa o transporte comum", {
  requisicao_capturada <- NULL
  chave <- "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo="

  httr2::local_mocked_responses(function(req) {
    requisicao_capturada <<- req
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html; charset=UTF-8"),
      body = charToRaw(paste0(
        "<html><strong>APIKey atual</strong>",
        "<span>Authorization: APIKey <strong>", chave, "</strong></span></html>"
      ))
    )
  })

  expect_identical(datajud::obter_chave_publica_cnj(), chave)
  expect_identical(requisicao_capturada$method, "GET")
  expect_false("Authorization" %in% names(requisicao_capturada$headers))
})

test_that("extração alternativa da chave não captura texto da navegação", {
  chave <- "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo="

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html; charset=UTF-8"),
      body = charToRaw(paste0(
        "<html>Authorization: APIKey ", chave, "AnteriorTermo</html>"
      ))
    )
  })

  expect_identical(datajud::obter_chave_publica_cnj(), chave)
})

test_that("identificação de tribunal cobre ramos e estados", {
  estados <- sprintf("%02d", 1:27)
  processos <- paste0("0000000-00.2024.8.", estados, ".0000")
  resultados <- lapply(processos, datajud::aux_identifica_tribunal)

  expect_length(resultados, 27L)
  expect_true(all(vapply(resultados, length, integer(1)) == 2L))
  invisible(try(datajud::aux_identifica_tribunal("0000000-00.2024.1.00.0000"), silent = TRUE))
  invisible(try(datajud::aux_identifica_tribunal("0000000-00.2024.2.00.0000"), silent = TRUE))
  invisible(try(datajud::aux_identifica_tribunal("0000000-00.2024.3.00.0000"), silent = TRUE))
  invisible(try(datajud::aux_identifica_tribunal("0000000-00.2024.4.01.0000"), silent = TRUE))
  invisible(try(datajud::aux_identifica_tribunal("0000000-00.2024.5.01.0000"), silent = TRUE))
})
