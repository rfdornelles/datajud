test_that("fixture válida pode ser lida por processo e movimentos", {
  dados <- carregar_fixture("resposta_processo_valida.json")

  processo <- NULL
  movimentos <- NULL
  capture.output(processo <- datajud::datajud_ler_processo(list(dados)))
  capture.output(movimentos <- datajud::datajud_ler_movimentacoes(list(dados)))

  expect_s3_class(processo, "tbl_df")
  expect_equal(nrow(processo), 1L)
  expect_s3_class(movimentos, "tbl_df")
  expect_equal(nrow(movimentos), 1L)
  expect_equal(processo$orgao_julgador_codigo, 1234)
  expect_equal(processo$orgao_julgador_nome, "1ª Vara Cível")
  expect_equal(processo$id, "processo-exemplo-1")
  expect_equal(movimentos$codigo_tpu, 51)
  expect_equal(movimentos$nome_orgao_julgador, "1ª Vara Cível")
})

test_that("fixtures de cardinalidade e campos opcionais ficam disponíveis", {
  multipla <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  opcionais <- carregar_fixture("resposta_processo_campos_opcionais_ausentes.json")

  expect_equal(length(multipla$`_source`$assuntos), 2L)
  expect_false("movimentos" %in% names(opcionais$`_source`))
  expect_true("assuntos" %in% names(opcionais$`_source`))
})
