serializar_e_ler_consulta <- function(consulta) {
  serializar <- getFromNamespace("serializar_query_datajud", "datajud")
  jsonlite::fromJSON(serializar(consulta), simplifyVector = FALSE)
}

test_that("cada categoria gera um filtro terms isolado", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  casos <- list(
    list(argumentos = list(assunto_codigo = 899), campo = "assuntos.codigo"),
    list(argumentos = list(classe_codigo = 1116), campo = "classe.codigo"),
    list(argumentos = list(orgao_codigo = 13597), campo = "orgaoJulgador.codigo")
  )

  for (caso in casos) {
    consulta <- do.call(criar, caso$argumentos)
    filtro <- consulta$query$bool$filter[[1]]

    expect_named(filtro$terms, caso$campo)
    expect_identical(unclass(filtro$terms[[caso$campo]]),
                     unname(unlist(caso$argumentos)))
  }
})

test_that("todas as combinações preservam AND entre categorias", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  combinacoes <- expand.grid(
    assunto = c(FALSE, TRUE),
    classe = c(FALSE, TRUE),
    orgao = c(FALSE, TRUE)
  )
  combinacoes <- combinacoes[rowSums(combinacoes) > 0L, , drop = FALSE]

  for (i in seq_len(nrow(combinacoes))) {
    linha <- combinacoes[i, ]
    argumentos <- list(
      assunto_codigo = if (linha$assunto) 899 else NULL,
      classe_codigo = if (linha$classe) 1116 else NULL,
      orgao_codigo = if (linha$orgao) 13597 else NULL
    )
    filtros <- do.call(criar, argumentos)$query$bool$filter

    expect_length(filtros, sum(linha))
    expect_true(all(vapply(filtros, function(filtro) {
      identical(names(filtro), "terms")
    }, logical(1))))
  }
})

test_that("um ou vários códigos permanecem arrays no JSON", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  consulta <- criar(
    assunto_codigo = c(899, 900),
    classe_codigo = 1116,
    orgao_codigo = c(13597, 13598)
  )
  json <- serializar_e_ler_consulta(consulta)
  filtros <- json$query$bool$filter

  expect_identical(filtros[[1]]$terms$`assuntos.codigo`, list(899L, 900L))
  expect_identical(filtros[[2]]$terms$`classe.codigo`, list(1116L))
  expect_identical(
    filtros[[3]]$terms$`orgaoJulgador.codigo`,
    list(13597L, 13598L)
  )
})

test_that("assuntos usam OR por padrão e podem exigir todos", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")

  qualquer <- criar(assunto_codigo = c(899, 900))$query$bool$filter
  todos <- criar(
    assunto_codigo = c(899, 900),
    classe_codigo = 1116,
    exigir_todos_assuntos = TRUE
  )$query$bool$filter

  expect_length(qualquer, 1L)
  expect_identical(unclass(qualquer[[1]]$terms$`assuntos.codigo`), c(899, 900))
  expect_length(todos, 3L)
  expect_identical(unclass(todos[[1]]$terms$`assuntos.codigo`), 899)
  expect_identical(unclass(todos[[2]]$terms$`assuntos.codigo`), 900)
  expect_identical(unclass(todos[[3]]$terms$`classe.codigo`), 1116)
})

test_that("consulta inclui ordenação estável e cursor opaco", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  cursor <- list(1681366085550, "identificador-unico")
  consulta <- criar(assunto_codigo = 899, cursor = cursor)
  json <- serializar_e_ler_consulta(consulta)

  expect_identical(
    names(json$sort[[1]]),
    "@timestamp"
  )
  expect_identical(json$sort[[1]]$`@timestamp`$order, "asc")
  expect_identical(names(json$sort[[2]]), "id.keyword")
  expect_identical(json$sort[[2]]$`id.keyword`$order, "asc")
  expect_identical(json$search_after, cursor)
})

test_that("construtor é determinístico e não altera entradas", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  argumentos <- list(
    assunto_codigo = c(segundo = 900, primeiro = 899, repetido = 900),
    classe_codigo = 1116,
    size = 25L,
    cursor = list(123, "id")
  )
  primeira <- do.call(criar, argumentos)
  segunda <- do.call(criar, argumentos)

  expect_identical(primeira, segunda)
  expect_identical(argumentos$assunto_codigo,
                   c(segundo = 900, primeiro = 899, repetido = 900))
  expect_identical(
    unclass(primeira$query$bool$filter[[1]]$terms$`assuntos.codigo`),
    c(900, 899)
  )
})

test_that("size aceita somente inteiros entre 1 e 10000", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")

  expect_identical(criar(assunto_codigo = 1, size = 1)$size, 1L)
  expect_identical(criar(assunto_codigo = 1, size = 10000)$size, 10000L)

  invalidos <- list(0, 10001, 1.5, NA_real_, Inf, c(1, 2), "100", TRUE)
  for (size in invalidos) {
    expect_error(criar(assunto_codigo = 1, size = size), "size")
  }
})

test_that("códigos inválidos são rejeitados em todas as categorias", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  invalidos <- list(numeric(), NA_real_, c(1, NA), 0, -1, 1.2, Inf, "1", TRUE)
  categorias <- c("assunto_codigo", "classe_codigo", "orgao_codigo")

  for (categoria in categorias) {
    for (valor in invalidos) {
      argumentos <- stats::setNames(list(valor), categoria)
      expect_error(do.call(criar, argumentos), categoria, fixed = TRUE)
    }
  }
})

test_that("demais argumentos inválidos produzem erros claros", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")

  expect_error(criar(), "ao menos um filtro")
  expect_error(
    criar(assunto_codigo = 1, exigir_todos_assuntos = NA),
    "exigir_todos_assuntos",
    fixed = TRUE
  )
  expect_error(
    criar(assunto_codigo = 1, exigir_todos_assuntos = c(TRUE, FALSE)),
    "exigir_todos_assuntos",
    fixed = TRUE
  )
  expect_error(criar(assunto_codigo = 1, cursor = list()), "cursor")
  expect_error(criar(assunto_codigo = 1, cursor = environment()), "cursor")
  expect_error(criar(assunto_codigo = 1, ordenacao = "@timestamp"), "ordenacao")
  expect_error(
    criar(assunto_codigo = 1, ordenacao = c("id.keyword", "@timestamp")),
    "ordenacao"
  )
})

test_that("códigos long do Elasticsearch não sofrem coerção para integer", {
  criar <- getFromNamespace("criar_query_datajud", "datajud")
  codigo_longo <- 3000000000
  consulta <- criar(assunto_codigo = codigo_longo)
  json <- serializar_e_ler_consulta(consulta)

  expect_identical(
    json$query$bool$filter[[1]]$terms$`assuntos.codigo`,
    list(codigo_longo)
  )
})
