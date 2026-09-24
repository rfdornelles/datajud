fixture_busca_tpu <- function() {
  x <- datajud_assuntos[seq_len(8), ]
  x$codigo <- c(100L, 40L, 30L, 20L, 10L, 50L, 60L, 1000000000L)
  x$nome <- c("Assunto diverso", "ação", "Áção tributária", "Ação civil",
    "Revisão de ação", "literal .*", "Classe 100", "Bilhao")
  x$codigo_pai <- c(NA_integer_, 100L, 100L, 100L, 40L, NA_integer_, 100L, NA_integer_)
  x$ativo <- c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE)
  x
}

test_that("relevância prioriza código, exato, prefixo e trecho com desempate estável", {
  x <- fixture_busca_tpu()
  buscar <- function(termo) buscar_tpu(x, termo, NULL, NULL, Inf)$codigo
  expect_identical(buscar("acao"), c(40L, 20L, 30L, 10L))
  expect_identical(buscar(100), c(100L, 60L))
  expect_identical(buscar("00100"), 100L)
  expect_identical(buscar(1000000000), 1000000000L)
  expect_identical(buscar("10"), c(10L, 60L))
  expect_identical(buscar(".*"), 50L)
  y <- buscar_tpu(x[8:1, ], "acao", NULL, NULL, Inf)
  expect_identical(y, buscar_tpu(x, "acao", NULL, NULL, Inf))
  expect_identical(buscar(NULL), sort(x$codigo))
})

test_that("caixa, acentos compostos e decompostos têm resultados idênticos", {
  x <- fixture_busca_tpu()
  esperado <- buscar_tpu(x, "acao", NULL, NULL, Inf)
  for (termo in c("AÇÃO", "  Ação  ", "ac\u0327a\u0303o")) {
    expect_identical(buscar_tpu(x, termo, NULL, NULL, Inf), esperado)
  }
  expect_identical(esperado$nome[1], "ação")
  locale <- stringi::stri_locale_get()
  withr::defer(stringi::stri_locale_set(locale))
  stringi::stri_locale_set("tr_TR")
  expect_identical(normalizar_busca_tpu("INDENIZAÇÃO"), "indenizacao")
})

test_that("filtros e limite preservam status, filhos diretos e raízes", {
  x <- fixture_busca_tpu()
  expect_identical(buscar_tpu(x, "acao", TRUE, NULL, Inf)$codigo, c(40L, 30L, 10L))
  expect_identical(buscar_tpu(x, "acao", FALSE, NULL, Inf)$codigo, 20L)
  filhos <- buscar_tpu(x, NULL, NULL, 100L, Inf)
  expect_identical(filhos$codigo, c(20L, 30L, 40L, 60L))
  expect_identical(filhos$ativo, c(FALSE, TRUE, TRUE, TRUE))
  expect_identical(buscar_tpu(x, NULL, TRUE, 100L, 1L)$codigo, 30L)
  expect_identical(buscar_tpu(x, "acao", NULL, NULL, 2L)$codigo, c(40L, 20L))
  expect_identical(buscar_tpu(x, NULL, NULL, NA, Inf)$codigo, c(50L, 100L, 1000000000L))
  expect_equal(nrow(buscar_tpu(x, NULL, NULL, 999L, Inf)), 0L)
})

test_that("ausência de resultado preserva esquema e proveniência", {
  x <- fixture_busca_tpu()
  vazio <- buscar_tpu(x, "inexistente", NULL, NULL, Inf)
  expect_identical(vazio, x[integer(), ])
  expect_identical(attr(vazio, "tpu_fontes"), attr(x, "tpu_fontes"))
})

test_that("argumentos inválidos falham de forma explícita", {
  x <- fixture_busca_tpu()
  for (termo in list("", "   ", "\t", NA, character(), c("a", "b"),
                     0, -1, 1.5, Inf, NaN, 2147483648, TRUE, list("a"), factor("a"))) {
    expect_error(buscar_tpu(x, termo, TRUE, NULL, 20L), "termo")
  }
  for (ativos in list(NA, 1, "TRUE", c(TRUE, FALSE), logical())) {
    expect_error(buscar_tpu(x, NULL, ativos, NULL, 20L), "ativos")
  }
  for (pai in list(0, -1, 1.5, Inf, "1", NA_character_, NaN, c(1, 1), numeric())) {
    expect_error(buscar_tpu(x, NULL, TRUE, pai, 20L), "codigo_pai")
  }
  for (limite in list(0, -1, 1.5, NA, NaN, -Inf, "1", TRUE, c(1, 2), numeric())) {
    expect_error(buscar_tpu(x, NULL, TRUE, NULL, limite), "limite")
  }
})

test_that("funções públicas usam bases locais e retornam códigos aceitos pela consulta", {
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("A busca local não deve acessar a rede"),
    .package = "httr2"
  )
  assunto <- datajud_buscar_assunto(899, ativos = NULL)
  classe <- datajud_buscar_classe("procedimento comum civel", limite = 1)
  expect_identical(assunto$codigo[1], 899L)
  expect_equal(nrow(classe), 1L)
  consulta <- criar_query_datajud(assunto_codigo = assunto$codigo, classe_codigo = classe$codigo)
  expect_type(consulta, "list")
  expect_identical(datajud_buscar_assunto("zzzinexistente"), datajud_assuntos[integer(), ])
  expect_identical(datajud_buscar_classe("zzzinexistente"), datajud_classes[integer(), ])
  expect_equal(nrow(datajud_buscar_assunto()), 20L)
  expect_true(all(datajud_buscar_assunto()$ativo))
  expect_true(all(!datajud_buscar_classe(ativos = FALSE)$ativo))
})
