resposta_classes_leitura <- function(ids, inicio, total = 4L) {
  hits <- lapply(seq_along(ids), function(indice) {
    id <- ids[[indice]]
    list(
      `_source` = list(
        id = id,
        tribunal = "TJSP",
        numeroProcesso = paste0("numero-", id),
        assuntos = list(),
        movimentos = list()
      ),
      sort = list(inicio + indice, id)
    )
  })
  list(hits = list(
    total = list(value = total, relation = "eq"),
    hits = hits
  ))
}

criar_coleta_classes_leitura <- function(diretorio) {
  respostas <- list(
    resposta_classes_leitura(c("id-1", "id-2"), 1000L),
    resposta_classes_leitura(c("id-3", "id-4"), 2000L)
  )
  chamada <- 0L
  testthat::local_mocked_bindings(
    requisitar_api_datajud = function(cliente, endpoint, query) {
      chamada <<- chamada + 1L
      respostas[[chamada]]
    },
    .package = "datajud",
    .env = parent.frame()
  )
  datajud::datajud_coletar_processos(
    "TJSP",
    diretorio,
    assunto_codigo = 899,
    size = 2,
    pausa = 0,
    cliente = datajud::datajud_cliente(chave_publica_teste())
  )
}

test_that("coleta pode ser reaberta sem materializar hits", {
  diretorio <- withr::local_tempdir()
  criada <- criar_coleta_classes_leitura(diretorio)
  aberta <- datajud::datajud_abrir_coleta(diretorio)

  expect_s3_class(aberta, "datajud_coleta")
  expect_false("hits" %in% names(aberta))
  expect_identical(aberta$arquivos, criada$arquivos)
  expect_identical(aberta$metadados$paginas, 2L)
  expect_identical(aberta$metadados$registros, 4L)
  expect_identical(aberta$metadados$tribunal, "TJSP")
  expect_false("cliente" %in% names(aberta))
  expect_false(grepl(
    chave_publica_teste(),
    jsonlite::toJSON(unclass(aberta), auto_unbox = TRUE),
    fixed = TRUE
  ))
})

test_that("imprimir coleta não abre arquivos de hits", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  testthat::local_mocked_bindings(
    ler_pagina_ndjson = function(caminho) {
      stop("o arquivo de hits não deveria ser aberto")
    },
    .package = "datajud"
  )

  saida <- testthat::capture_messages(retorno <- print(coleta))

  expect_identical(retorno, coleta)
  expect_lte(length(saida), 4L)
  expect_true(any(grepl("Páginas: 2 | Registros: 4", saida, fixed = TRUE)))
})

test_that("ler página acessa somente o NDJSON solicitado", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  leitura_original <- getFromNamespace("ler_pagina_ndjson", "datajud")
  acessados <- character()
  testthat::local_mocked_bindings(
    ler_pagina_ndjson = function(caminho) {
      acessados <<- c(acessados, basename(caminho))
      leitura_original(caminho)
    },
    .package = "datajud"
  )

  pagina <- datajud::datajud_ler_pagina(coleta, 2)

  expect_s3_class(pagina, "datajud_resultado")
  expect_identical(acessados, "pagina-000002.ndjson")
  expect_identical(pagina$metadados$pagina, 2L)
  expect_identical(pagina$metadados$origem, "coleta")
  expect_identical(
    vapply(pagina$hits, function(hit) hit$`_source`$id, character(1)),
    c("id-3", "id-4")
  )
})

test_that("resultado em memória e página têm conversões equivalentes", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  pagina <- datajud::datajud_ler_pagina(coleta, 1)
  construir <- getFromNamespace("construir_datajud_resultado", "datajud")
  resultado <- construir(
    pagina$hits,
    pagina$consulta,
    pagina$metadados
  )

  expect_identical(
    tibble::as_tibble(pagina),
    tibble::as_tibble(resultado)
  )
  expect_identical(
    datajud::datajud_ler_processo(pagina),
    datajud::datajud_ler_processo(resultado)
  )
  expect_identical(
    datajud::datajud_ler_movimentacoes(pagina),
    datajud::datajud_ler_movimentacoes(resultado)
  )
})

test_that("leitores não materializam uma coleta inteira implicitamente", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)

  expect_error(
    datajud::datajud_ler_processo(coleta),
    "não é materializada implicitamente"
  )
  expect_error(
    datajud::datajud_ler_movimentacoes(coleta),
    "datajud_ler_pagina"
  )
})

test_that("abertura e seleção de página rejeitam entradas inválidas", {
  inexistente <- file.path(withr::local_tempdir(), "inexistente")
  expect_error(
    datajud::datajud_abrir_coleta(inexistente),
    "coleta existente"
  )

  sem_manifesto <- withr::local_tempdir()
  expect_error(
    datajud::datajud_abrir_coleta(sem_manifesto),
    "manifesto.json",
    class = "datajud_erro_coleta_integridade"
  )

  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  expect_error(datajud::datajud_ler_pagina(coleta, 0), "inteiro positivo")
  expect_error(
    datajud::datajud_ler_pagina(coleta, 3),
    "página 3 não existe",
    class = "datajud_erro_coleta_pagina"
  )
  expect_error(
    datajud::datajud_ler_pagina(unclass(coleta), 1),
    "datajud_coleta.*inválido"
  )

  paginas_incompativeis <- coleta
  paginas_incompativeis$paginas <- list()
  expect_error(
    print(paginas_incompativeis),
    "páginas.*incompatíveis"
  )

  consulta_sensivel <- coleta
  consulta_sensivel$consulta$api_key <- chave_publica_teste()
  expect_error(
    print(consulta_sensivel),
    "dados sensíveis"
  )

  metadados_incompativeis <- coleta
  metadados_incompativeis$paginas[[1]]$primeiro_id <- "outro-id"
  expect_error(
    datajud::datajud_ler_pagina(metadados_incompativeis, 1),
    "não corresponde aos metadados"
  )
})

test_that("página recuperada usa total conservador da coleta", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  coleta$paginas[[1]]["total_valor"] <- list(NULL)
  coleta$paginas[[1]]["total_relacao"] <- list(NULL)

  pagina <- datajud::datajud_ler_pagina(coleta, 1)

  expect_identical(pagina$metadados$total_valor, 4L)
  expect_identical(pagina$metadados$total_relacao, "eq")
})

test_that("invariantes de resultado rejeitam objetos adulterados", {
  diretorio <- withr::local_tempdir()
  pagina <- datajud::datajud_ler_pagina(
    criar_coleta_classes_leitura(diretorio),
    1
  )
  construir <- getFromNamespace("construir_datajud_resultado", "datajud")

  expect_error(
    construir("hits", pagina$consulta, pagina$metadados),
    "hits.*lista"
  )
  expect_error(
    construir(pagina$hits, "consulta", pagina$metadados),
    "consulta.*lista"
  )
  expect_error(
    construir(pagina$hits, pagina$consulta, list()),
    "metadados.*incompletos"
  )

  contagem <- pagina
  contagem$metadados$quantidade_recebida <- 1L
  expect_error(print(contagem), "contagens.*inválidas")

  tribunal <- pagina
  tribunal$metadados$tribunal <- ""
  expect_error(print(tribunal), "tribunal, página ou total")

  cursor <- pagina
  cursor$metadados$proximo_cursor <- list(9999, "outro-id")
  expect_error(print(cursor), "não corresponde ao último hit")

  sensivel <- pagina
  sensivel$consulta$token <- chave_publica_teste()
  expect_error(print(sensivel), "dados sensíveis")

  expect_error(
    datajud::datajud_ler_processo("inválido"),
    "lista ou um objeto"
  )
})

test_that("impressões das classes permanecem compactas", {
  diretorio <- withr::local_tempdir()
  coleta <- criar_coleta_classes_leitura(diretorio)
  pagina <- datajud::datajud_ler_pagina(coleta, 1)
  coleta$diretorio <- "/dados/coleta-datajud"
  coleta$manifesto <- "/dados/coleta-datajud/manifesto.json"
  coleta$arquivos <- file.path(
    coleta$diretorio,
    basename(coleta$arquivos)
  )

  expect_snapshot(print(coleta))
  expect_snapshot(print(pagina))
})
