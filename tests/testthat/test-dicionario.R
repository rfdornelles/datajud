test_that("dicionário cobre nomes e tipos dos retornos e fixtures", {
  d <- utils::read.csv(system.file("extdata", "dicionario-retornos.csv", package = "datajud"),
    encoding = "UTF-8")
  expect_false(anyDuplicated(paste(d$objeto, d$campo)) > 0L)
  expect_true(all(nzchar(d$descricao) & nzchar(d$cardinalidade)))
  conferir <- function(objeto, x) {
    linhas <- d[d$objeto == objeto, ]
    expect_true(nrow(linhas) > 0L, info = objeto)
    expect_true(all(names(x) %in% linhas$campo), info = objeto)
    if (is.data.frame(x)) expect_setequal(names(x), linhas$campo)
    obrigatorios <- !grepl("^(0|opcional|ausente)", linhas$cardinalidade)
    expect_true(all(linhas$campo[obrigatorios] %in% names(x)), info = objeto)
    for (campo in names(x)) {
      tipos <- strsplit(linhas$tipo[match(campo, linhas$campo)], "/", fixed = TRUE)[[1]]
      valor <- x[[campo]]
      compativel <- vapply(tipos, function(tipo) switch(tipo,
        numeric = is.numeric(valor), integer = is.integer(valor),
        character = is.character(valor), logical = is.logical(valor),
        list = is.list(valor), `NULL` = is.null(valor),
        POSIXct = inherits(valor, "POSIXct"), Date = inherits(valor, "Date"),
        data.frame = is.data.frame(valor), FALSE), logical(1))
      expect_true(any(compativel), info = paste(objeto, campo, paste(class(valor), collapse = "/")))
    }
  }
  resultado <- readRDS(system.file("extdata", "exemplo-resultado.rds", package = "datajud"))
  conferir("resultado", resultado)
  conferir("resultado_metadados", resultado$metadados)
  conferir("cursor", stats::setNames(resultado$metadados$proximo_cursor, c("timestamp", "id")))
  conferir("resultado_tibble", tibble::as_tibble(resultado))
  conferir("consulta", resultado$consulta)
  conferir("consulta_query", resultado$consulta$query)
  conferir("consulta_bool", resultado$consulta$query$bool)
  for (filtro in resultado$consulta$query$bool$filter) conferir("consulta_filtro", filtro)
  for (ordem in resultado$consulta$sort) conferir("consulta_ordem", ordem[[1]])
  for (hit in resultado$hits) {
    conferir("hit", hit)
    conferir("fonte", hit$`_source`)
    fonte <- hit$`_source`
    for (campo in c("formato", "sistema")) {
      if (!is.null(fonte[[campo]])) conferir("fonte_nome", fonte[[campo]])
    }
    conferir("fonte_codigo_nome", fonte$classe)
    for (assunto in fonte$assuntos) conferir("fonte_codigo_nome", assunto)
    if (!is.null(fonte$orgaoJulgador)) conferir("fonte_orgao", fonte$orgaoJulgador)
    for (movimento in fonte$movimentos) {
      conferir("fonte_movimento", movimento)
      for (complemento in movimento$complementosTabelados) conferir("fonte_complemento", complemento)
      for (orgao in movimento$orgaoJulgador) conferir("fonte_orgao_movimento", orgao)
    }
  }
  processos <- datajud_ler_processo(resultado)
  conferir("processos", processos)
  for (assuntos in processos$assuntos) conferir("assuntos", assuntos)
  conferir("assuntos_desaninhados", datajud_desaninhar_assuntos(processos))
  conferir("movimentos", datajud_ler_movimentacoes(resultado))
  for (arquivo in c("resposta_processo_valida.json", "resposta_processo_multiplos_assuntos.json",
                    "resposta_processo_campos_opcionais_ausentes.json")) {
    hits <- list(carregar_fixture(arquivo))
    conferir("processos", datajud_ler_processo(hits))
    conferir("movimentos", datajud_ler_movimentacoes(hits))
  }
  coleta <- datajud_abrir_coleta(system.file("extdata", "exemplo-coleta", package = "datajud"))
  conferir("coleta", coleta)
  conferir("coleta_metadados", coleta$metadados)
  manifesto <- jsonlite::read_json(coleta$manifesto)
  conferir("manifesto", manifesto)
  conferir("limites", manifesto$limites)
  conferir("contagens", manifesto$contagens)
  for (pagina in coleta$paginas) conferir("pagina", pagina)
  conferir("resultado", datajud_ler_pagina(coleta, 2))
  conferir("falha", list(pagina = 3L, mensagem = "Exemplo sintético", classes = c("error", "condition")))
  for (x in list(datajud_assuntos, datajud_classes)) {
    conferir("tpu", x)
    conferir("tpu_atributos", attributes(x)[c("tpu_fontes", "tpu_publicacoes_divergentes", "tpu_ancestrais_sql")])
    conferir("tpu_fontes", attr(x, "tpu_fontes"))
    conferir("tpu_publicacoes", attr(x, "tpu_publicacoes_divergentes"))
  }
  conferir("cliente", datajud_cliente(chave_publica_teste()))
  conferir("tribunais", tribunais)
})

test_that("exemplos distribuídos são utilizáveis sem rede e não contêm cliente", {
  testthat::local_mocked_bindings(req_perform = function(...) stop("Rede proibida"), .package = "httr2")
  resultado <- readRDS(system.file("extdata", "exemplo-resultado.rds", package = "datajud"))
  expect_false("cliente" %in% names(resultado))
  expect_equal(nrow(datajud_ler_processo(resultado)), 2L)
  coleta <- datajud_abrir_coleta(system.file("extdata", "exemplo-coleta", package = "datajud"))
  expect_identical(coleta$metadados$estado, "completa")
  expect_length(coleta$arquivos, 2L)
  expect_equal(nrow(datajud_ler_processo(datajud_ler_pagina(coleta, 1))), 1L)
})
