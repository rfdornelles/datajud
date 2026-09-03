baixar_wiki_cnj <- getFromNamespace("baixar_wiki_cnj", "datajud")
verificar_cache_wiki <- getFromNamespace("verificar_cache_wiki", "datajud")

sitemap_reduzido <- function(urls) {
  entradas <- paste0("<url><loc>", urls, "</loc></url>", collapse = "")
  paste0(
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
    "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">",
    entradas,
    "</urlset>"
  )
}

test_that("downloader cria cache e manifesto reproduzíveis", {
  diretorio <- withr::local_tempdir()
  manifesto_versionado <- tempfile(fileext = ".csv")
  urls <- c("https://wiki.exemplo/pagina-a", "https://wiki.exemplo/pagina-b")
  sitemap <- sitemap_reduzido(urls)

  httr2::local_mocked_responses(function(req) {
    if (grepl("sitemap.xml", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/xml"),
        body = charToRaw(sitemap)
      ))
    }
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html; charset=UTF-8"),
      body = charToRaw(paste("conteúdo de", req$url))
    )
  })

  manifesto <- baixar_wiki_cnj(
    sitemap_url = "https://wiki.exemplo/sitemap.xml",
    diretorio = diretorio,
    manifesto_versionado = manifesto_versionado,
    pausa = 0
  )

  expect_s3_class(manifesto, "tbl_df")
  expect_equal(nrow(manifesto), 3L)
  expect_true(all(manifesto$resultado == "ok"))
  expect_true(all(file.exists(file.path(diretorio, manifesto$arquivo))))
  expect_true(file.exists(manifesto_versionado))
  expect_identical(
    unname(tools::md5sum(file.path(diretorio, manifesto$arquivo))),
    manifesto$hash_md5
  )
  expect_true(all(verificar_cache_wiki(manifesto, diretorio)$situacao == "ok"))
})

test_that("verificador detecta página ausente ou alterada", {
  diretorio <- withr::local_tempdir()
  urls <- c("https://wiki.exemplo/a", "https://wiki.exemplo/b")
  sitemap <- sitemap_reduzido(urls)
  httr2::local_mocked_responses(function(req) {
    if (grepl("sitemap.xml", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/xml"),
        body = charToRaw(sitemap)
      ))
    }
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html"),
      body = charToRaw(req$url)
    )
  })
  manifesto <- baixar_wiki_cnj(
    "https://wiki.exemplo/sitemap.xml",
    diretorio,
    pausa = 0
  )

  caminho_a <- file.path(diretorio, manifesto$arquivo[manifesto$url == urls[[1]]])
  caminho_b <- file.path(diretorio, manifesto$arquivo[manifesto$url == urls[[2]]])
  writeBin(charToRaw("conteúdo alterado"), caminho_a)
  expect_true(file.remove(caminho_b))
  verificacao <- verificar_cache_wiki(manifesto, diretorio)

  expect_identical(verificacao$situacao[verificacao$url == urls[[1]]], "alterada")
  expect_identical(verificacao$situacao[verificacao$url == urls[[2]]], "ausente")
})

test_that("downloader registra página remota ausente", {
  diretorio <- withr::local_tempdir()
  url_pagina <- "https://wiki.exemplo/ausente"
  sitemap <- sitemap_reduzido(url_pagina)
  httr2::local_mocked_responses(function(req) {
    if (grepl("sitemap.xml", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/xml"),
        body = charToRaw(sitemap)
      ))
    }
    httr2::response(
      status_code = 404,
      headers = list(`Content-Type` = "text/html")
    )
  })

  manifesto <- baixar_wiki_cnj(
    "https://wiki.exemplo/sitemap.xml",
    diretorio,
    pausa = 0
  )
  pagina <- manifesto[manifesto$tipo == "pagina", ]

  expect_identical(pagina$status_http, 404L)
  expect_identical(pagina$resultado, "http_404")
  expect_identical(
    verificar_cache_wiki(manifesto, diretorio)$situacao[[2]],
    "download_falhou"
  )
})

test_that("downloader rejeita sitemap inválido", {
  diretorio <- withr::local_tempdir()
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html"),
      body = charToRaw("não é sitemap")
    )
  })

  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = 0),
    class = "datajud_erro_cache_wiki"
  )
})

test_that("downloader não segue URL externa ao host do sitemap", {
  diretorio <- withr::local_tempdir()
  sitemap <- sitemap_reduzido("https://host-externo.exemplo/pagina")
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/xml"),
      body = charToRaw(sitemap)
    )
  })

  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = 0),
    "URL externa"
  )
})

test_that("downloader registra conexão e tipo de página inválido", {
  diretorio <- withr::local_tempdir()
  urls <- c("https://wiki.exemplo/conexao", "https://wiki.exemplo/json")
  sitemap <- sitemap_reduzido(urls)
  httr2::local_mocked_responses(function(req) {
    if (grepl("sitemap.xml", req$url, fixed = TRUE)) {
      return(httr2::response(
        status_code = 200,
        headers = list(`Content-Type` = "application/xml"),
        body = charToRaw(sitemap)
      ))
    }
    if (grepl("conexao", req$url, fixed = TRUE)) {
      stop("falha simulada")
    }
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw("{}")
    )
  })

  manifesto <- baixar_wiki_cnj(
    "https://wiki.exemplo/sitemap.xml",
    diretorio,
    pausa = 0
  )

  expect_identical(
    manifesto$resultado[manifesto$url == urls[[1]]],
    "erro_conexao"
  )
  expect_identical(
    manifesto$resultado[manifesto$url == urls[[2]]],
    "tipo_invalido"
  )
})

test_that("downloader valida resposta e conteúdo do sitemap", {
  diretorio <- withr::local_tempdir()

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 404,
      headers = list(`Content-Type` = "application/xml")
    )
  })
  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = 0),
    "HTTP 404"
  )

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/xml"),
      body = charToRaw("<xml")
    )
  })
  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = 0),
    "XML inválido"
  )

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/xml"),
      body = charToRaw("<urlset></urlset>")
    )
  })
  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = 0),
    "não contém páginas"
  )
})

test_that("cache valida argumentos e formato do manifesto", {
  diretorio <- withr::local_tempdir()

  expect_error(
    baixar_wiki_cnj("https://wiki.exemplo/sitemap.xml", diretorio, pausa = -1),
    "pausa"
  )
  expect_error(
    baixar_wiki_cnj("", diretorio, pausa = 0),
    "sitemap_url"
  )
  expect_error(
    verificar_cache_wiki(data.frame(url = "x"), diretorio),
    "formato esperado"
  )
})

test_that("fixture registra o contrato sem depender da rede", {
  caminho <- system.file(
    "extdata",
    "contrato-api-publica.json",
    package = "datajud"
  )
  contrato <- jsonlite::read_json(caminho, simplifyVector = TRUE)

  expect_identical(contrato$campos$`assuntos.codigo`, "long")
  expect_identical(contrato$campos$`classe.codigo`, "long")
  expect_identical(contrato$campos$`orgaoJulgador.codigo`, "long")
  expect_identical(contrato$identificadores$consulta_processo, "numeroProcesso")
  expect_identical(contrato$identificadores$chave_unica_pacote, "id")
  expect_equal(contrato$size$maximo_documentado, 10000)
  expect_identical(contrato$paginacao$mecanismo_documentado, "search_after")
  expect_identical(
    contrato$paginacao$ordenacao_composta_pacote,
    c("@timestamp", "id.keyword")
  )
  expect_identical(
    contrato$paginacao$cursor_formato_pacote,
    c("timestamp numerico", "id textual")
  )
  expect_true(contrato$paginacao$protecao_cursor_repetido)
  expect_true(contrato$paginacao$protecao_id_repetido_entre_paginas)
  expect_identical(contrato$total$campos, c("value", "relation"))
  expect_identical(contrato$erros$campos_topo_observados, c("error", "status"))
})
