test_that("normalização rejeita CNJs ausentes e com tamanho inválido", {
  expect_equal(
    datajud:::normalizar_numero_cnj("0000102-03.2004.8.26.0000"),
    "00001020320048260000"
  )
  expect_error(datajud:::normalizar_numero_cnj(NA_character_), "inválido")
  expect_error(datajud:::normalizar_numero_cnj(""), "inválido")
  expect_error(datajud:::normalizar_numero_cnj("123"), "inválido")
})

test_that("consulta vetorial valida tribunal e pausa antes da rede", {
  cliente <- datajud::datajud_cliente("chave-teste")
  processos <- c("0000102-03.2004.8.26.0000", "0000102-03.2004.8.26.0000")

  expect_error(
    datajud::datajud_consultar_processo(processos, cliente, tribunal = c("TJSP", NA)),
    "mesmo tamanho"
  )
  expect_error(
    datajud::datajud_consultar_processo(processos, cliente, tribunal = "TJSP", sleep = c(0, 1)),
    "sleep"
  )
  expect_error(
    datajud::datajud_consultar_processo(processos, cliente, tribunal = "TJSP", sleep = 61),
    "sleep"
  )
})
