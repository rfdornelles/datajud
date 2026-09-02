# Utilitários internos do cache local da Wiki do CNJ.

wiki_validar_texto <- function(x, nome) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    cli::cli_abort("{nome} deve ser um texto n\u00E3o vazio.")
  }
  invisible(x)
}

wiki_requisitar <- function(url, timeout = 30) {
  tryCatch(
    httr2::request(url) |>
      httr2::req_user_agent("datajud-wiki-cache/1.0") |>
      httr2::req_timeout(seconds = timeout) |>
      httr2::req_retry(
        max_tries = 3,
        is_transient = function(resp) {
          httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
        },
        after = httr2::resp_retry_after
      ) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(),
    error = identity
  )
}

wiki_nomes_arquivo <- function(urls) {
  caminhos <- sub("^https?://[^/]+/?", "", urls)
  caminhos <- utils::URLdecode(caminhos)
  caminhos[caminhos == ""] <- "inicio"
  caminhos <- iconv(caminhos, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  nomes <- tolower(gsub("[^A-Za-z0-9]+", "-", caminhos))
  nomes <- gsub("(^-+|-+$)", "", nomes)
  nomes[nomes == ""] <- "pagina"
  paste0(make.unique(nomes, sep = "-"), ".html")
}

wiki_gravar_binario <- function(conteudo, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  conexao <- file(caminho, open = "wb")
  on.exit(close(conexao), add = TRUE)
  writeBin(conteudo, conexao)
  invisible(caminho)
}

wiki_hash_md5 <- function(caminho) {
  unname(tools::md5sum(caminho))
}

wiki_url_canonica <- function(url) {
  if (grepl("/$|\\.[A-Za-z0-9]+$", url)) url else paste0(url, "/")
}

wiki_registro <- function(url, url_obtida, tipo, baixado_em_utc,
                          status_http = NA_integer_,
                          tipo_conteudo = "", arquivo = "", bytes = 0,
                          hash_md5 = "", resultado) {
  tibble::tibble(
    url = url,
    url_obtida = url_obtida,
    tipo = tipo,
    baixado_em_utc = baixado_em_utc,
    status_http = as.integer(status_http),
    tipo_conteudo = tipo_conteudo,
    arquivo = arquivo,
    bytes = as.numeric(bytes),
    hash_md5 = hash_md5,
    resultado = resultado
  )
}

wiki_baixar_pagina <- function(url, arquivo, diretorio, baixado_em_utc,
                               timeout = 30) {
  url_obtida <- wiki_url_canonica(url)
  resposta <- wiki_requisitar(url_obtida, timeout = timeout)
  if (inherits(resposta, "condition")) {
    return(wiki_registro(
      url, url_obtida, "pagina", baixado_em_utc,
      resultado = "erro_conexao"
    ))
  }

  status <- httr2::resp_status(resposta)
  tipo_conteudo <- httr2::resp_header(resposta, "content-type", default = "")
  if (status < 200L || status >= 300L) {
    return(wiki_registro(
      url, url_obtida, "pagina", baixado_em_utc,
      status_http = status,
      tipo_conteudo = tipo_conteudo,
      resultado = paste0("http_", status)
    ))
  }
  if (!grepl("text/html", tipo_conteudo, ignore.case = TRUE)) {
    return(wiki_registro(
      url, url_obtida, "pagina", baixado_em_utc,
      status_http = status,
      tipo_conteudo = tipo_conteudo,
      resultado = "tipo_invalido"
    ))
  }

  conteudo <- httr2::resp_body_raw(resposta)
  caminho <- file.path(diretorio, arquivo)
  wiki_gravar_binario(conteudo, caminho)
  wiki_registro(
    url, url_obtida, "pagina", baixado_em_utc,
    status_http = status,
    tipo_conteudo = tipo_conteudo,
    arquivo = arquivo,
    bytes = length(conteudo),
    hash_md5 = wiki_hash_md5(caminho),
    resultado = "ok"
  )
}

wiki_gravar_manifesto <- function(manifesto, caminho) {
  dir.create(dirname(caminho), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    manifesto,
    file = caminho,
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  invisible(caminho)
}

baixar_wiki_cnj <- function(
  sitemap_url = "https://datajud-wiki.cnj.jus.br/sitemap.xml",
  diretorio = ".cache/datajud-wiki",
  manifesto_versionado = NULL,
  pausa = 0.05,
  timeout = 30
) {
  wiki_validar_texto(sitemap_url, "sitemap_url")
  wiki_validar_texto(diretorio, "diretorio")
  if (!is.numeric(pausa) || length(pausa) != 1L || !is.finite(pausa) || pausa < 0) {
    cli::cli_abort("pausa deve ser um n\u00FAmero finito n\u00E3o negativo.")
  }

  dir.create(diretorio, recursive = TRUE, showWarnings = FALSE)
  baixado_em_utc <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  resposta_sitemap <- wiki_requisitar(sitemap_url, timeout = timeout)
  if (inherits(resposta_sitemap, "condition")) {
    cli::cli_abort(
      "N\u00E3o foi poss\u00EDvel baixar o sitemap da Wiki do CNJ.",
      class = "datajud_erro_cache_wiki"
    )
  }

  status_sitemap <- httr2::resp_status(resposta_sitemap)
  tipo_sitemap <- httr2::resp_header(
    resposta_sitemap,
    "content-type",
    default = ""
  )
  if (status_sitemap < 200L || status_sitemap >= 300L) {
    cli::cli_abort(
      "O sitemap da Wiki do CNJ retornou HTTP {status_sitemap}.",
      class = "datajud_erro_cache_wiki"
    )
  }
  if (!grepl("xml", tipo_sitemap, ignore.case = TRUE)) {
    cli::cli_abort(
      "O sitemap da Wiki do CNJ n\u00E3o retornou conte\u00FAdo XML.",
      class = "datajud_erro_cache_wiki"
    )
  }

  conteudo_sitemap <- httr2::resp_body_raw(resposta_sitemap)
  caminho_sitemap <- file.path(diretorio, "sitemap.xml")
  wiki_gravar_binario(conteudo_sitemap, caminho_sitemap)
  documento <- tryCatch(
    xml2::read_xml(conteudo_sitemap),
    error = function(cnd) {
      cli::cli_abort(
        "O sitemap da Wiki do CNJ cont\u00E9m XML inv\u00E1lido.",
        class = "datajud_erro_cache_wiki"
      )
    }
  )
  urls <- documento |>
    xml2::xml_find_all("//*[local-name()='loc']") |>
    xml2::xml_text(trim = TRUE) |>
    unique() |>
    sort()
  if (length(urls) == 0L) {
    cli::cli_abort(
      "O sitemap da Wiki do CNJ n\u00E3o cont\u00E9m p\u00E1ginas.",
      class = "datajud_erro_cache_wiki"
    )
  }
  origem <- httr2::url_parse(sitemap_url)
  mesma_origem <- vapply(
    urls,
    function(url) {
      pagina <- httr2::url_parse(url)
      identical(pagina$scheme, "https") &&
        identical(pagina$hostname, origem$hostname)
    },
    logical(1)
  )
  if (!all(mesma_origem)) {
    cli::cli_abort(
      "O sitemap cont\u00E9m URL externa ou sem HTTPS.",
      class = "datajud_erro_cache_wiki"
    )
  }

  registro_sitemap <- wiki_registro(
    sitemap_url, sitemap_url, "sitemap", baixado_em_utc,
    status_http = status_sitemap,
    tipo_conteudo = tipo_sitemap,
    arquivo = "sitemap.xml",
    bytes = length(conteudo_sitemap),
    hash_md5 = wiki_hash_md5(caminho_sitemap),
    resultado = "ok"
  )
  arquivos <- wiki_nomes_arquivo(urls)
  registros <- Map(
    function(url, arquivo) {
      registro <- wiki_baixar_pagina(
        url = url,
        arquivo = arquivo,
        diretorio = diretorio,
        baixado_em_utc = baixado_em_utc,
        timeout = timeout
      )
      if (pausa > 0) Sys.sleep(pausa)
      registro
    },
    urls,
    arquivos
  )
  manifesto <- dplyr::bind_rows(c(list(registro_sitemap), registros))
  wiki_gravar_manifesto(manifesto, file.path(diretorio, "manifesto.csv"))
  if (!is.null(manifesto_versionado)) {
    wiki_validar_texto(manifesto_versionado, "manifesto_versionado")
    wiki_gravar_manifesto(manifesto, manifesto_versionado)
  }
  manifesto
}

verificar_cache_wiki <- function(manifesto, diretorio) {
  wiki_validar_texto(diretorio, "diretorio")
  if (is.character(manifesto) && length(manifesto) == 1L) {
    manifesto <- utils::read.csv(
      manifesto,
      stringsAsFactors = FALSE,
      na.strings = ""
    )
  }
  colunas <- c("url", "arquivo", "hash_md5", "resultado")
  if (!is.data.frame(manifesto) || !all(colunas %in% names(manifesto))) {
    cli::cli_abort("manifesto n\u00E3o possui o formato esperado.")
  }

  situacao <- vapply(
    seq_len(nrow(manifesto)),
    function(indice) {
      if (!identical(manifesto$resultado[[indice]], "ok")) {
        return("download_falhou")
      }
      caminho <- file.path(diretorio, manifesto$arquivo[[indice]])
      if (!file.exists(caminho)) {
        return("ausente")
      }
      if (!identical(wiki_hash_md5(caminho), manifesto$hash_md5[[indice]])) {
        return("alterada")
      }
      "ok"
    },
    character(1)
  )
  dplyr::mutate(tibble::as_tibble(manifesto), situacao = situacao)
}
