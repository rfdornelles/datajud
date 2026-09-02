executar_probe_contrato <- getFromNamespace("executar_probe_contrato", "datajud")
imprimir_probe_contrato <- getFromNamespace("imprimir_probe_contrato", "datajud")

test_that("probe registra somente a estrutura sanitizada do contrato", {
  chave <- chave_publica_teste()
  cliente <- structure(
    list(api_key = chave, timeout = 5),
    class = "datajud_cliente"
  )
  chamadas <- 0L
  requisicoes <- list()
  sucesso <- jsonlite::toJSON(list(
    hits = list(
      total = list(value = 10000L, relation = "gte"),
      hits = list(list(
        `_id` = "TJSP_1116_G1_1_00000000000000000000",
        `_source` = list(id = "TJSP_1116_G1_1_00000000000000000000"),
        sort = list("TJSP_1116_G1_1_00000000000000000000")
      ))
    )
  ), auto_unbox = TRUE)
  falha <- jsonlite::toJSON(list(
    error = list(type = "parsing_exception"),
    status = 400L
  ), auto_unbox = TRUE)

  httr2::local_mocked_responses(function(req) {
    chamadas <<- chamadas + 1L
    requisicoes[[chamadas]] <<- req
    if (chamadas <= 2L) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/json"),
        body = charToRaw(sucesso)
      ))
    }
    httr2::response(
      status_code = 400,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(falha)
    )
  })

  resultado <- executar_probe_contrato("https://api.exemplo/_search", cliente)
  saida <- testthat::capture_messages(imprimir_probe_contrato(resultado))

  expect_s3_class(resultado, "tbl_df")
  expect_identical(resultado$status_id_keyword, 200L)
  expect_identical(resultado$status_timestamp, 200L)
  expect_identical(resultado$total_campos, "value,relation")
  expect_identical(resultado$total_relation, "gte")
  expect_true(resultado$id_unico_confere)
  expect_identical(resultado$status_erro, 400L)
  expect_identical(resultado$erro_campos, "error,status")
  expect_false(any(grepl(chave, saida, fixed = TRUE)))
  expect_false(any(vapply(
    requisicoes,
    function(req) grepl(chave, paste(capture.output(print(req)), collapse = ""), fixed = TRUE),
    logical(1)
  )))
})

test_that("probe exige cliente explícito", {
  expect_error(
    executar_probe_contrato("https://api.exemplo/_search", list()),
    "datajud_cliente"
  )
})
