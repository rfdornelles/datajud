test_that("cliente é o último argumento opcional das consultas públicas", {
  funcoes <- c(
    "datajud_consultar_processo",
    "datajud_pesquisar_processos",
    "datajud_pesquisar_classe_orgao"
  )

  for (nome in funcoes) {
    argumentos <- formals(getExportedValue("datajud", nome))
    expect_identical(tail(names(argumentos), 1L), "cliente", info = nome)
    expect_true("cliente" %in% names(argumentos), info = nome)
    expect_null(argumentos$cliente, info = nome)
  }
})

test_that("pesquisa geral cria cliente transitório quando omitido", {
  chave <- chave_publica_teste()
  withr::local_envvar(DATAJUD_API_KEY = chave, DATAJUD_EMAIL = "")
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  cliente_capturado <- NULL
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      cliente_capturado <<- cliente
      corpo
    },
    .package = "datajud"
  )

  resultado <- datajud::datajud_pesquisar_processos(
    "TJSP",
    assunto_codigo = 899
  )

  expect_s3_class(resultado, "datajud_resultado")
  expect_s3_class(cliente_capturado, "datajud_cliente")
  expect_identical(cliente_capturado$api_key, chave)
  expect_false("cliente" %in% names(resultado))
})

test_that("consulta vetorial resolve o cliente implícito somente uma vez", {
  chave <- chave_publica_teste()
  withr::local_envvar(DATAJUD_API_KEY = chave, DATAJUD_EMAIL = "")
  processos <- rep("0000102-03.2004.8.26.0000", 2L)
  chamadas_cliente <- 0L
  cliente_original <- datajud::datajud_cliente

  testthat::local_mocked_bindings(
    datajud_cliente = function(...) {
      chamadas_cliente <<- chamadas_cliente + 1L
      cliente_original(...)
    },
    datajud_requisition = function(processo, cliente, tribunal) {
      list(`_source` = list(
        id = paste0("id-", tribunal),
        numeroProcesso = gsub("[^0-9]", "", processo)
      ))
    },
    .package = "datajud"
  )

  resultado <- datajud::datajud_consultar_processo(
    processos,
    tribunal = "TJSP",
    sleep = 0
  )

  expect_identical(chamadas_cliente, 1L)
  expect_length(resultado, 2L)
})

test_that("cliente explícito prevalece e não chama a resolução automática", {
  cliente <- datajud::datajud_cliente(chave_publica_teste(2))
  processo <- "0000102-03.2004.8.26.0000"

  testthat::local_mocked_bindings(
    datajud_cliente = function(...) {
      stop("cliente automático não deveria ser criado")
    },
    datajud_requisition = function(processo, cliente, tribunal) {
      list(`_source` = list(
        id = "id-explicito",
        numeroProcesso = gsub("[^0-9]", "", processo)
      ))
    },
    .package = "datajud"
  )

  resultado <- datajud::datajud_consultar_processo(
    processo,
    tribunal = "TJSP",
    sleep = 0,
    cliente = cliente
  )

  expect_length(resultado, 1L)
  expect_identical(resultado[[1]]$`_source`$id, "id-explicito")
})

test_that("posição antiga do cliente funciona com aviso de depreciação", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) corpo,
    datajud_requisition = function(processo, cliente, tribunal) {
      list(`_source` = list(
        id = "id-legado",
        numeroProcesso = gsub("[^0-9]", "", processo)
      ))
    },
    .package = "datajud"
  )

  expect_warning(
    resultado <- datajud::datajud_pesquisar_processos(
      "TJSP",
      cliente,
      assunto_codigo = 899
    ),
    class = "datajud_aviso_cliente_posicional"
  )
  expect_s3_class(resultado, "datajud_resultado")

  expect_warning(
    legado <- datajud::datajud_pesquisar_classe_orgao(
      "TJSP",
      cliente,
      classe_codigo = 1116
    ),
    class = "datajud_aviso_cliente_posicional"
  )
  expect_length(legado, 2L)

  expect_warning(
    processo <- datajud::datajud_consultar_processo(
      "0000102-03.2004.8.26.0000",
      cliente,
      tribunal = "TJSP",
      sleep = 0
    ),
    class = "datajud_aviso_cliente_posicional"
  )
  expect_length(processo, 1L)
})

test_that("argumentos posicionais ambíguos produzem erro acionável", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())

  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP",
      899,
      assunto_codigo = 899,
      cliente = cliente
    ),
    "devem ser nomeados"
  )
  expect_error(
    datajud::datajud_pesquisar_processos(
      "TJSP",
      cliente,
      assunto_codigo = 899,
      cliente = cliente
    ),
    "último argumento"
  )
})

test_that("cliente implícito não cria objetos no ambiente global", {
  withr::local_envvar(
    DATAJUD_API_KEY = chave_publica_teste(),
    DATAJUD_EMAIL = ""
  )
  corpo <- carregar_fixture("resposta_pesquisa_processos.json")
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) corpo,
    .package = "datajud"
  )
  antes <- ls(envir = .GlobalEnv, all.names = TRUE)

  datajud::datajud_pesquisar_processos("TJSP", assunto_codigo = 899)

  depois <- ls(envir = .GlobalEnv, all.names = TRUE)
  expect_identical(depois, antes)
})
