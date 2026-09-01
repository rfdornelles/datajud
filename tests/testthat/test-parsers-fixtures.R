test_that("fixture válida pode ser lida por processo e movimentos", {
  dados <- carregar_fixture("resposta_processo_valida.json")

  processo <- datajud:::ler_processo(dados)
  movimentos <- datajud:::ler_movimentos(dados)

  expect_s3_class(processo, "tbl_df")
  expect_equal(nrow(processo), 1L)
  expect_s3_class(movimentos, "tbl_df")
  expect_equal(nrow(movimentos), 1L)
})

test_that("fixtures de cardinalidade e campos opcionais ficam disponíveis", {
  multipla <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  opcionais <- carregar_fixture("resposta_processo_campos_opcionais_ausentes.json")

  expect_equal(length(multipla$`_source`$assuntos), 2L)
  expect_false("movimentos" %in% names(opcionais$`_source`))
  expect_true("assuntos" %in% names(opcionais$`_source`))
})
