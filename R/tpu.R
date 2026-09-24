# Infraestrutura interna da atualização explícita das TPU (data-raw/tpu.R).
tpu_texto <- function(x) {
  trimws(gsub("[[:space:]\u00a0]+", " ", xml2::xml_text(x)))
}

tpu_descobrir <- function(arquivo, tipo, base = "https://www.cnj.jus.br/sgt/") {
  stopifnot(tipo %in% c("A", "C"))
  doc <- xml2::read_html(arquivo, encoding = "ISO-8859-1")
  opcoes <- xml2::xml_find_all(doc, "//option[contains(@value, '.xls')]")
  caminhos <- xml2::xml_attr(opcoes, "value")
  tabela <- if (tipo == "A") "Assuntos" else "Classes"
  usar <- grepl(paste0("^[0-9]+_Tabela_", tabela, "_"), basename(caminhos)) &
    !grepl("_Impressao_", caminhos)
  opcoes <- opcoes[usar]
  caminhos <- caminhos[usar]
  if (!length(caminhos) || anyDuplicated(caminhos)) {
    stop("Cat\u00e1logo TPU ausente ou com URLs duplicadas.")
  }
  linhas <- xml2::xml_find_first(opcoes, "ancestor::tr[1]")
  versao <- as.Date(tpu_texto(xml2::xml_find_first(linhas, "./td[1]")), "%d/%m/%Y")
  segmento <- tpu_texto(xml2::xml_find_first(linhas, "./td[2]"))
  if (anyNA(versao) || length(unique(versao)) != 1L || any(!nzchar(segmento))) {
    stop("Vers\u00e3o ou segmentos inconsistentes no cat\u00e1logo TPU.")
  }
  tibble::tibble(tipo = tipo, versao = versao,
    segmento = segmento, grau = tpu_texto(opcoes),
    url = xml2::url_absolute(caminhos, base), arquivo = basename(caminhos), formato = "xls")
}

tpu_descobrir_sql <- function(arquivo, tipo, base = "https://www.cnj.jus.br/sgt/") {
  doc <- xml2::read_html(arquivo, encoding = "ISO-8859-1")
  no <- xml2::xml_find_all(doc, "//input[@id='dump_dados_oracle_postgres']")
  if (length(no) != 1L) stop("Cat\u00e1logo SQL ausente ou amb\u00edguo.")
  nome <- xml2::xml_attr(no, "value")
  if (!grepl("^[0-9]+_dump_dados_oracle_postgres\\.sql$", nome)) stop("Arquivo SQL inesperado.")
  linha <- xml2::xml_find_first(no, "ancestor::tr[1]/td[1]")
  versao <- as.Date(tpu_texto(linha), "%d/%m/%Y")
  if (is.na(versao)) stop("Vers\u00e3o SQL inv\u00e1lida.")
  tibble::tibble(tipo = tipo, versao = versao, segmento = "", grau = "",
    url = paste0(base, "enviarArquivo.php?url=", nome, "&nome=dump_dados_oracle_postgres.sql"),
    arquivo = nome, formato = "sql")
}

tpu_data <- function(x) {
  vazio <- !nzchar(x)
  formato <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}( [0-9]{2}:[0-9]{2}:[0-9]{2})?$", x)
  dia <- substr(x, 1, 10)
  out <- as.Date(dia, "%Y-%m-%d")
  if (any(!vazio & (!formato | is.na(out) | format(out, "%Y-%m-%d") != dia))) {
    stop("Data inv\u00e1lida na TPU.")
  }
  out[vazio] <- as.Date(NA)
  out
}

tpu_ler <- function(arquivo, segmento, grau) {
  doc <- xml2::read_html(arquivo, encoding = "ISO-8859-1")
  tabela <- xml2::xml_find_first(doc, "//table[not(ancestor::table)]")
  # Algumas raízes são células diretamente sob table, sem tr. Agrupá-las
  # preserva esses códigos; percorrer somente filhos evita o glossário aninhado.
  filhos <- xml2::xml_find_all(tabela, "./tr | ./td | ./th | ./tbody/tr")
  linhas <- list()
  soltas <- list()
  for (no in filhos) {
    if (xml2::xml_name(no) %in% c("td", "th")) {
      soltas[[length(soltas) + 1L]] <- no
    } else {
      if (length(soltas)) linhas[[length(linhas) + 1L]] <- structure(soltas, class = "xml_nodeset")
      soltas <- list()
      linhas[[length(linhas) + 1L]] <- as.list(xml2::xml_find_all(no, "./td | ./th", ns = character()))
    }
  }
  if (length(soltas)) linhas[[length(linhas) + 1L]] <- structure(soltas, class = "xml_nodeset")
  expandir <- function(celulas) {
    if (!length(celulas)) return(character())
    n <- xml2::xml_attr(celulas, "colspan")
    n[is.na(n)] <- "1"
    n <- suppressWarnings(as.integer(n))
    if (anyNA(n) || any(n < 1L | n > 100L)) stop("Colspan inv\u00e1lido na TPU.")
    out <- rep("", sum(n))
    out[cumsum(c(1L, utils::head(n, -1L)))] <- tpu_texto(celulas)
    out
  }
  valores <- lapply(linhas, expandir)
  cab <- which(vapply(valores, function(x) "C\u00f3digo" %in% x && "C\u00f3d. Pai" %in% x, logical(1)))
  if (length(cab) != 1L) stop("Cabe\u00e7alho TPU ausente ou amb\u00edguo.")
  nomes <- valores[[cab]]
  codigo <- match("C\u00f3digo", nomes)
  campos <- c("C\u00f3d. Pai", "Data de Publica\u00e7\u00e3o", "Data de Altera\u00e7\u00e3o",
    "Data de Inativa\u00e7\u00e3o", "Data de Reativa\u00e7\u00e3o")
  pos <- match(campos, nomes)
  if (anyNA(pos) || codigo < 2L) stop("Colunas obrigat\u00f3rias ausentes na TPU.")
  codigo_cab <- codigo
  pos_cab <- pos
  registros <- list()
  for (i in seq_along(valores)) {
    x <- valores[[i]]
    # Ramos com mais de cinco níveis ganham células de recuo adicionais,
    # embora o cabeçalho continue reservando apenas cinco colunas ao nome.
    deslocamento <- max(0L, length(x) - length(nomes))
    codigo <- codigo_cab + deslocamento
    pos <- pos_cab + deslocamento
    if (i == cab || length(x) < codigo) next
    # Linhas da legenda usam colspan abrangendo toda a tabela.
    if (!nzchar(x[codigo])) next
    if (!grepl("^[0-9]+$", x[codigo])) stop("C\u00f3digo inv\u00e1lido na TPU: ", x[codigo])
    length(x) <- max(length(x), max(pos))
    x[is.na(x)] <- ""
    pai <- x[pos[1]]
    if (nzchar(pai) && !grepl("^[0-9]+$", pai)) stop("Pai inv\u00e1lido na TPU.")
    nome <- paste(x[seq_len(codigo - 1L)][nzchar(x[seq_len(codigo - 1L)])], collapse = " ")
    if (!nzchar(nome)) {
      # O exportador insere separadores sem nome para ancestrais não
      # aplicáveis ao segmento. O registro real deve vir de outra fonte;
      # a validação final exige que todos os pais estejam presentes.
      if (!nzchar(pai)) next
      stop("Nome ausente na TPU: c\u00f3digo ", x[codigo])
    }
    # O SGT indica inatividade por texto riscado, inclusive em categorias
    # cujas datas não são preenchidas. Não inferir status apenas pelas datas.
    larguras <- xml2::xml_attr(linhas[[i]], "colspan")
    larguras[is.na(larguras)] <- "1"
    celulas_nome <- linhas[[i]][cumsum(as.integer(larguras)) < codigo]
    riscado <- length(xml2::xml_find_all(celulas_nome,
      ".//*[@style[contains(., 'line-through')]] | .//strike | .//s | self::*[@style[contains(., 'line-through')]]", ns = character())) > 0L
    datas <- x[pos[-1]]
    codigo_int <- suppressWarnings(as.integer(x[codigo]))
    pai_int <- if (pai %in% c("", "0")) NA_integer_ else suppressWarnings(as.integer(pai))
    if (is.na(codigo_int) || (!pai %in% c("", "0") && is.na(pai_int))) {
      stop("C\u00f3digo ou pai fora do intervalo inteiro na TPU.")
    }
    registros[[length(registros) + 1L]] <- list(
      codigo = codigo_int, nome = nome,
      codigo_pai = pai_int,
      ativo = !riscado, data_publicacao = datas[1], data_alteracao = datas[2],
      data_inativacao = datas[3], data_reativacao = datas[4],
      segmento = segmento, grau = grau)
  }
  if (!length(registros)) stop("Arquivo TPU sem registros.")
  out <- dplyr::bind_rows(registros)
  for (campo in c("data_publicacao", "data_alteracao", "data_inativacao", "data_reativacao")) {
    out[[campo]] <- tpu_data(out[[campo]])
  }
  if (anyNA(out$codigo)) stop("C\u00f3digo fora do intervalo inteiro.")
  out
}

tpu_ler_ancestrais_sql <- function(arquivo, tipo) {
  linhas <- readLines(arquivo, encoding = "latin1", warn = FALSE)
  linhas <- linhas[startsWith(linhas, "INSERT INTO ITENS (cod_item,cod_item_pai,tipo_item,nome,situacao,")]
  padrao <- " VALUES \\('([0-9]+)',(?:'([0-9]+)'|NULL),'([ACMD])','((?:[^']|'')*)','([AI])',"
  campos <- stringr::str_match(linhas, padrao)
  if (!length(linhas) || anyNA(campos[, 1])) stop("Formato SQL de ITENS n\u00e3o reconhecido.")
  campos <- campos[campos[, 4] == tipo, , drop = FALSE]
  tibble::tibble(codigo = as.integer(campos[, 2]),
    nome = enc2utf8(trimws(gsub("''", "'", campos[, 5], fixed = TRUE))),
    codigo_pai = as.integer(campos[, 3]), ativo = campos[, 6] == "A",
    data_publicacao = as.Date(NA), data_alteracao = as.Date(NA),
    data_inativacao = as.Date(NA), data_reativacao = as.Date(NA),
    segmento = NA_character_, grau = NA_character_)
}

tpu_consolidar <- function(fontes, ancestrais = NULL) {
  x <- dplyr::bind_rows(fontes)
  if (!nrow(x)) stop("Nenhuma fonte TPU.")
  complementados <- integer()
  repeat {
    ausentes <- setdiff(stats::na.omit(x$codigo_pai), x$codigo)
    if (!length(ausentes)) break
    if (is.null(ancestrais) || any(!ausentes %in% ancestrais$codigo)) stop("Pais ausentes na TPU.")
    complemento <- ancestrais[ancestrais$codigo %in% ausentes, ]
    if (anyDuplicated(complemento$codigo)) stop("Ancestrais SQL duplicados.")
    x <- dplyr::bind_rows(x, complemento)
    complementados <- c(complementados, complemento$codigo)
  }
  grupos <- split(seq_len(nrow(x)), x$codigo)
  campos <- c("nome", "codigo_pai", "ativo")
  datas <- c("data_publicacao", "data_alteracao", "data_inativacao", "data_reativacao")
  out <- lapply(grupos, function(idx) {
    y <- x[idx, ]
    for (campo in campos) {
      if (length(unique(y[[campo]])) != 1L) {
        stop(sprintf("Conflito TPU: c\u00f3digo %s, campo %s.", y$codigo[1], campo))
      }
    }
    linha <- y[1, c("codigo", campos, datas)]
    for (campo in datas) {
      valores <- unique(y[[campo]][!is.na(y[[campo]])])
      # A publicação pode ocorrer em datas diferentes por segmento. Guardar
      # a primeira publicação e preservar as variantes no atributo de auditoria.
      if (length(valores) > 1L && campo == "data_publicacao") valores <- min(valores)
      if (length(valores) > 1L) stop(sprintf("Conflito TPU: c\u00f3digo %s, campo %s.", y$codigo[1], campo))
      linha[[campo]] <- if (length(valores)) valores else as.Date(NA)
    }
    linha$segmentos <- list(sort(unique(y$segmento[!is.na(y$segmento)]), method = "radix"))
    linha$graus <- list(sort(unique(y$grau[!is.na(y$grau)]), method = "radix"))
    linha
  })
  out <- dplyr::bind_rows(out)
  out <- out[order(out$codigo), ]
  divergentes <- vapply(grupos, function(idx) {
    length(unique(x$data_publicacao[idx][!is.na(x$data_publicacao[idx])])) > 1L
  }, logical(1))
  publicacoes <- unique(x[x$codigo %in% names(grupos)[divergentes],
    c("codigo", "data_publicacao", "segmento", "grau")])
  publicacoes <- publicacoes[do.call(order, c(as.list(publicacoes), list(method = "radix"))), ]
  attr(out, "tpu_publicacoes_divergentes") <- publicacoes
  attr(out, "tpu_ancestrais_sql") <- sort(complementados)
  tpu_validar(out)
  out
}

tpu_validar <- function(x) {
  colunas <- c("codigo", "nome", "codigo_pai", "ativo", "data_publicacao",
    "data_alteracao", "data_inativacao", "data_reativacao", "segmentos", "graus")
  if (!is.data.frame(x) || !all(colunas %in% names(x))) {
    stop("Colunas obrigat\u00f3rias ausentes na TPU.")
  }
  if (!nrow(x) || !is.integer(x$codigo) || !is.integer(x$codigo_pai) ||
      anyNA(x$codigo) || any(x$codigo <= 0L) || anyDuplicated(x$codigo) ||
      !is.logical(x$ativo) || anyNA(x$ativo) ||
      !is.character(x$nome) || anyNA(x$nome) ||
      any(!nzchar(trimws(x$nome)))) stop("Tipos ou chaves TPU inv\u00e1lidos.")
  pais <- x$codigo_pai[!is.na(x$codigo_pai)]
  if (any(!pais %in% x$codigo)) stop("Pais ausentes na TPU.")
  for (campo in c("data_publicacao", "data_alteracao", "data_inativacao", "data_reativacao")) {
    if (!inherits(x[[campo]], "Date")) stop("Tipo de data TPU inv\u00e1lido.")
  }
  for (campo in c("segmentos", "graus")) {
    if (!is.list(x[[campo]]) || !all(vapply(x[[campo]], function(y) {
      is.character(y) && !anyNA(y) && all(nzchar(y)) && !anyDuplicated(y)
    }, logical(1)))) stop("Lista TPU inv\u00e1lida.")
  }
  # Remover folhas sucessivamente: qualquer resto denuncia um ciclo.
  pendentes <- x$codigo
  while (length(pendentes)) {
    proximos <- intersect(pendentes, x$codigo_pai[match(pendentes, x$codigo)])
    if (length(proximos) == length(pendentes)) stop("Ciclo na hierarquia TPU.")
    pendentes <- proximos
  }
  invisible(x)
}
