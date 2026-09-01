test_that("fixtures de respostas são válidas e sanitizadas", {
  arquivos <- list.files(
    testthat::test_path("fixtures"),
    pattern = "\\.json$",
    full.names = TRUE
  )

  expect_gte(length(arquivos), 5L)
  expect_true(all(file.info(arquivos)$size < 100000))

  texto <- paste(vapply(arquivos, function(arquivo) {
    paste(readLines(arquivo, encoding = "UTF-8"), collapse = "\n")
  }, character(1)), collapse = "\n")

  expect_false(grepl("APIKey\\s+\\S+", texto, ignore.case = TRUE))
  expect_false(grepl("gho_[A-Za-z0-9]+", texto))
  expect_false(grepl("Bearer\\s+\\S+", texto, ignore.case = TRUE))
})

test_that("fixtures cobrem respostas válidas, vazias e erros", {
  valida <- carregar_fixture("resposta_processo_valida.json")
  multipla <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  vazia <- carregar_fixture("resposta_vazia.json")
  erro <- carregar_fixture("erro_elasticsearch.json")

  expect_type(valida, "list")
  expect_length(valida$`_source`$assuntos, 1L)
  expect_length(multipla$`_source`$assuntos, 2L)
  expect_length(vazia$hits$hits, 0L)
  expect_equal(erro$status, 400)
  expect_equal(erro$error$type, "x_content_parse_exception")
})
