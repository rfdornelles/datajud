test_that("helper seleciona cada grupo de tribunais", {
  esperados <- c(
    todos = 91L,
    justica_comum = 33L,
    estadual = 27L,
    federal = 6L,
    eleitoral = 27L,
    superiores = 4L,
    trabalho = 24L,
    militar_estadual = 3L
  )

  for (grupo in names(esperados)) {
    resultado <- datajud::datajud_listar_tribunais(grupo)
    expect_s3_class(resultado, "tbl_df")
    expect_named(resultado, c("tribunal", "sigla", "tipo", "url"))
    expect_equal(nrow(resultado), esperados[[grupo]], info = grupo)
  }
})

test_that("justiça comum reúne apenas tribunais estaduais e federais", {
  resultado <- datajud::datajud_listar_tribunais("justica_comum")

  expect_setequal(unique(resultado$tipo), c("Estadual", "Federal"))
  expect_true(all(grepl("^(tj|trf)", resultado$sigla)))
})

test_that("eleitoral usa TRE e trabalho usa TRT", {
  eleitorais <- datajud::datajud_listar_tribunais("eleitoral")
  trabalho <- datajud::datajud_listar_tribunais("trabalho")

  expect_true(all(grepl("^tre-", eleitorais$sigla)))
  expect_true(all(grepl("^trt", trabalho$sigla)))
})

test_that("múltiplos grupos são unidos sem duplicação", {
  resultado <- datajud::datajud_listar_tribunais(c("estadual", "federal"))
  comum <- datajud::datajud_listar_tribunais("justica_comum")

  expect_identical(resultado, comum)
  expect_identical(anyDuplicated(resultado$sigla), 0L)
})

test_that("grupos inválidos falham com mensagem acionável", {
  expect_error(datajud::datajud_listar_tribunais(character()), "grupo")
  expect_error(datajud::datajud_listar_tribunais(NA_character_), "grupo")
  expect_error(datajud::datajud_listar_tribunais(""), "grupo")
  expect_error(
    datajud::datajud_listar_tribunais("tributaria"),
    "desconhecido"
  )
  expect_error(
    datajud::datajud_listar_tribunais(c("todos", "estadual")),
    "não pode ser combinado"
  )
})
