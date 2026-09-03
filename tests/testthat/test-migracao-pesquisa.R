test_that("pesquisa legada não permanece exportada ou documentada", {
  expect_false(
    "datajud_pesquisar_classe_orgao" %in% getNamespaceExports("datajud")
  )
  expect_false(exists(
    "datajud_pesquisar_classe_orgao",
    envir = asNamespace("datajud"),
    inherits = FALSE
  ))
  expect_false(exists(
    "monta_consulta_elasticsearch",
    envir = asNamespace("datajud"),
    inherits = FALSE
  ))
  expect_false(file.exists(testthat::test_path(
    "..", "..", "man", "datajud_pesquisar_classe_orgao.Rd"
  )))
})

test_that("exemplo de migração executa offline com os mesmos filtros", {
  withr::local_envvar(
    DATAJUD_API_KEY = chave_publica_teste(),
    DATAJUD_EMAIL = ""
  )
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  consulta_capturada <- NULL
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      consulta_capturada <<- query
      corpo
    },
    .package = "datajud"
  )

  resultado <- datajud::datajud_pesquisar_processos(
    tribunal = "TJRJ",
    classe_codigo = 1116,
    orgao_codigo = 13597,
    size = 500
  )

  filtros <- consulta_capturada$query$bool$filter
  campos <- unlist(lapply(filtros, function(filtro) {
    names(filtro$terms)
  }))
  expect_s3_class(resultado, "datajud_resultado")
  expect_length(resultado$hits, 2L)
  expect_identical(consulta_capturada$size, 500L)
  expect_equal(unclass(filtros[[1]]$terms$`classe.codigo`), 1116)
  expect_equal(unclass(filtros[[2]]$terms$`orgaoJulgador.codigo`), 13597)
  expect_false("assuntos.codigo" %in% campos)
  expect_identical(resultado$hits, resultado[["hits"]])
})
