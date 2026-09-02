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
  expect_equal(processo$assuntos_resumo, "899 / Indenização por dano moral")
  expect_equal(movimentos$codigo_tpu, 51)
  expect_equal(movimentos$nome_orgao_julgador, "1ª Vara Cível")
})

test_that("assuntos múltiplos não duplicam processos e podem ser desaninados", {
  dados <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  processos <- datajud::datajud_ler_processo(list(dados))
  assuntos <- datajud::datajud_desaninhar_assuntos(processos)

  expect_equal(nrow(processos), 1L)
  expect_s3_class(processos$assuntos[[1]], "tbl_df")
  expect_equal(nrow(processos$assuntos[[1]]), 2L)
  expect_equal(nrow(assuntos), 2L)
  expect_identical(unique(assuntos$id), "processo-exemplo-2")
})

test_that("processo sem movimentos retorna esquema vazio", {
  dados <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  movimentos <- datajud::datajud_ler_movimentacoes(list(dados))

  expect_s3_class(movimentos, "tbl_df")
  expect_equal(nrow(movimentos), 0L)
  expect_true(all(c("tribunal", "numero_processo", "datahora_movimento") %in% names(movimentos)))
})

test_that("movimentos com campos opcionais ausentes mantêm o esquema", {
  dados <- carregar_fixture("resposta_processo_valida.json")
  dados$`_source`$movimentos <- list(list(codigo = 7L, nome = "Distribuição"))

  movimentos <- datajud::datajud_ler_movimentacoes(list(dados))

  expect_equal(nrow(movimentos), 1L)
  expect_true(all(c("datahora_movimento", "codigo_tabelado",
                    "nome_orgao_julgador") %in% names(movimentos)))
  expect_true(is.na(movimentos$datahora_movimento))
})

test_that("id ausente é rejeitado pelo parser de processos", {
  dados <- carregar_fixture("resposta_processo_valida.json")
  dados$`_source`$id <- NULL

  expect_error(datajud::datajud_ler_processo(list(dados)), "campo id é obrigatório")
})

test_that("fixtures de cardinalidade e campos opcionais ficam disponíveis", {
  multipla <- carregar_fixture("resposta_processo_multiplos_assuntos.json")
  opcionais <- carregar_fixture("resposta_processo_campos_opcionais_ausentes.json")

  expect_equal(length(multipla$`_source`$assuntos), 2L)
  expect_false("movimentos" %in% names(opcionais$`_source`))
  expect_true("assuntos" %in% names(opcionais$`_source`))
})
