test_that("cliente usa argumento antes da variável de ambiente", {
  withr::local_envvar(DATAJUD_API_KEY = "chave-do-ambiente", DATAJUD_EMAIL = "ambiente@exemplo.org")

  cliente <- datajud::datajud_cliente(
    api_key = "chave-do-argumento",
    email = "argumento@exemplo.org",
    timeout = 10
  )

  expect_s3_class(cliente, "datajud_cliente")
  expect_identical(cliente$api_key, "chave-do-argumento")
  expect_identical(cliente$email, "argumento@exemplo.org")
  expect_identical(cliente$timeout, 10)
})

test_that("cliente sem e-mail não adiciona e-mail ao User-Agent", {
  cliente <- datajud::datajud_cliente("chave-teste", email = "")
  expect_identical(cliente$email, "")
  expect_false(any(grepl("e-mail", as.character(datajud:::cliente_user_agent(cliente)))))
})

test_that("impressão do cliente não expõe a chave", {
  cliente <- datajud::datajud_cliente("segredo-nao-imprimir", email = "a@b.org")
  saida <- testthat::capture_messages(print(cliente))
  expect_true(any(grepl("a@b.org", saida, fixed = TRUE)))
  expect_false(any(grepl("segredo-nao-imprimir", saida, fixed = TRUE)))
})

test_that("leitores retornam valores sem criar objetos globais", {
  dados <- carregar_fixture("resposta_processo_valida.json")
  antes <- ls(envir = .GlobalEnv, all.names = TRUE)

  datajud::datajud_ler_processo(list(dados))
  datajud::datajud_ler_movimentacoes(list(dados))

  depois <- ls(envir = .GlobalEnv, all.names = TRUE)
  expect_identical(depois, antes)
})
