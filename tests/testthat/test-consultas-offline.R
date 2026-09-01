test_that("construtor de consulta combina filtros", {
  consulta <- datajud:::monta_consulta_elasticsearch(
    assunto_codigo = c(1116, 9999),
    orgao_codigo = c(123, 456),
    size = 25
  )
  json <- jsonlite::fromJSON(consulta, simplifyVector = FALSE)

  expect_equal(json$size, 25)
  expect_length(json$query$bool$should, 2L)
  expect_length(json$query$bool$filter[[1]]$bool$should, 2L)
})

test_that("construtor aceita cada filtro isoladamente", {
  expect_no_error(datajud:::monta_consulta_elasticsearch(assunto_codigo = 1))
  expect_no_error(datajud:::monta_consulta_elasticsearch(orgao_codigo = 2))
  expect_no_error(datajud:::monta_consulta_elasticsearch())
})

test_that("pesquisa valida argumentos antes da rede", {
  cliente <- datajud::datajud_cliente("chave-teste")

  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente
  ), "Nenhum")
  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente, classe_codigo = 1, size = 0
  ), "entre 1 e 10000")
  expect_error(datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = list(), classe_codigo = 1
  ), "datajud_cliente")
})

test_that("pesquisa processa resposta HTTP simulada", {
  cliente <- datajud::datajud_cliente("chave-teste")
  resposta <- list(
    status_code = 200L,
    corpo = list(hits = list(hits = list(list(`_source` = list(id = "x")))) )
  )

  testthat::local_mocked_bindings(
    POST = function(...) resposta,
    content = function(x, ...) x$corpo,
    .package = "httr"
  )
  resultado <- datajud::datajud_pesquisar_classe_orgao(
    tribunal = "TJSP", cliente = cliente, classe_codigo = 1116
  )

  expect_length(resultado, 1L)
})

test_that("consulta por processo usa resposta HTTP simulada", {
  cliente <- datajud::datajud_cliente("chave-teste")
  processo <- "0000102-03.2004.8.26.0000"
  resposta <- list(
    status_code = 200L,
    corpo = list(hits = list(hits = list(list(`_source` = list(
      numeroProcesso = gsub("[^0-9]", "", processo)
    )))))
  )

  testthat::local_mocked_bindings(
    POST = function(...) resposta,
    content = function(x, ...) x$corpo,
    .package = "httr"
  )
  resultado <- datajud:::datajud_requisition(processo, cliente, "TJSP")

  expect_equal(resultado$`_source`$numeroProcesso, gsub("[^0-9]", "", processo))
})
