tpu_ler <- getFromNamespace("tpu_ler", "datajud")
tpu_consolidar <- getFromNamespace("tpu_consolidar", "datajud")
tpu_validar <- getFromNamespace("tpu_validar", "datajud")
tpu_descobrir <- getFromNamespace("tpu_descobrir", "datajud")
tpu_data <- getFromNamespace("tpu_data", "datajud")
tpu_ler_ancestrais_sql <- getFromNamespace("tpu_ler_ancestrais_sql", "datajud")
tpu_descobrir_sql <- getFromNamespace("tpu_descobrir_sql", "datajud")

ler_tpu_fixture <- function() {
  tpu_ler(test_path("fixtures", "tpu-legado.xls"), "Estadual", "Estadual 1º Grau")
}

test_that("HTML legado preserva raízes, acentos e status sem ler glossário como código", {
  x <- ler_tpu_fixture()
  expect_identical(x$codigo, 1:3)
  expect_identical(x$codigo_pai, c(NA_integer_, 1L, 1L))
  expect_identical(x$nome, c("Área pública", "Ação antiga", "Ação reativada"))
  expect_identical(x$ativo, c(TRUE, FALSE, TRUE))
  expect_s3_class(x$data_publicacao, "Date")
  expect_equal(x$data_inativacao[2], as.Date("2020-01-01"))
  expect_true(is.na(x$data_publicacao[1]))
})

test_that("duplicatas compatíveis agregam a aplicabilidade sem repetir códigos", {
  a <- ler_tpu_fixture()
  b <- a
  b$segmento <- "Federal"
  b$grau <- "Federal 2º Grau"
  x <- tpu_consolidar(list(a, b, a))
  expect_identical(x$codigo, 1:3)
  expect_identical(x$segmentos[[1]], c("Estadual", "Federal"))
  expect_identical(x$graus[[1]], c("Estadual 1º Grau", "Federal 2º Grau"))
  expect_identical(x, tpu_consolidar(list(b, a)))
  for (campo in c("nome", "codigo_pai", "ativo", "data_alteracao")) {
    b <- a
    b[[campo]][3] <- switch(campo, nome = "Outro", codigo_pai = 2L,
      ativo = FALSE, data_alteracao = as.Date("2010-01-01"))
    expect_error(tpu_consolidar(list(a, b)), paste0("Conflito TPU.*", campo))
  }
})

test_that("publicações por segmento preservam divergências e a primeira data", {
  a <- ler_tpu_fixture()
  b <- a
  b$segmento <- "Federal"
  b$grau <- "Federal 2º Grau"
  b$data_publicacao[2] <- as.Date("2010-01-01")
  b$data_publicacao[1] <- as.Date("2011-01-01")
  x <- tpu_consolidar(list(a, b, a))
  expect_equal(x$data_publicacao[2], as.Date("2008-09-30"))
  expect_equal(x$data_publicacao[1], as.Date("2011-01-01"))
  divergencias <- attr(x, "tpu_publicacoes_divergentes")
  expect_equal(nrow(divergencias), 2L)
  expect_identical(divergencias$codigo, c(2L, 2L))
  expect_setequal(divergencias$data_publicacao, c(a$data_publicacao[2], b$data_publicacao[2]))
  expect_identical(x, tpu_consolidar(list(b, a)))
})

test_that("validação rejeita pais ausentes, ciclos, chaves e tipos inválidos", {
  x <- tpu_consolidar(list(ler_tpu_fixture()))
  expect_error(tpu_validar(NULL), "Colunas obrigatórias")
  y <- x; y$codigo_pai <- NULL
  expect_error(tpu_validar(y), "Colunas obrigatórias")
  y <- x; y$nome <- seq_len(nrow(y))
  expect_error(tpu_validar(y), "Tipos ou chaves")
  y <- x; y$codigo_pai[2] <- 999L
  expect_error(tpu_validar(y), "Pais ausentes")
  y <- x; y$codigo_pai[1] <- 2L
  expect_error(tpu_validar(y), "Ciclo")
  y <- x; y$codigo[2] <- 1L
  expect_error(tpu_validar(y), "chaves")
  y <- x; y$data_publicacao <- as.character(y$data_publicacao)
  expect_error(tpu_validar(y), "Tipo de data")
  y <- x; y$graus[[1]] <- NA_character_
  expect_error(tpu_validar(y), "Lista")
  expect_error(tpu_data("2020-02-31"), "Data inválida")
  expect_error(tpu_data("2020-01-01 lixo"), "Data inválida")
})

test_that("ancestrais omitidos do XLS são completados recursivamente pelo SQL", {
  arquivo <- tempfile(fileext = ".sql")
  withr::defer(unlink(arquivo))
  writeLines(c(
    "INSERT INTO ITENS (cod_item,cod_item_pai,tipo_item,nome,situacao,dat_inclusao) VALUES ('1','4','A','Raiz d''origem','A',NULL);",
    "INSERT INTO ITENS (cod_item,cod_item_pai,tipo_item,nome,situacao,dat_inclusao) VALUES ('4',NULL,'A','Superior','I',NULL);",
    "INSERT INTO ITENS (cod_item,cod_item_pai,tipo_item,nome,situacao,dat_inclusao) VALUES ('1',NULL,'C','Outra tabela','A',NULL);"
  ), arquivo)
  ancestrais <- tpu_ler_ancestrais_sql(arquivo, "A")
  fontes <- ler_tpu_fixture()[-1, ]
  x <- tpu_consolidar(list(fontes), ancestrais)
  expect_identical(x$codigo, 1:4)
  expect_identical(x$nome[1], "Raiz d'origem")
  expect_false(x$ativo[4])
  expect_identical(x$codigo_pai, c(4L, 1L, 1L, NA_integer_))
  expect_identical(attr(x, "tpu_ancestrais_sql"), c(1L, 4L))
  expect_length(x$segmentos[[1]], 0L)
  expect_true(is.na(x$data_publicacao[1]))
  expect_error(tpu_consolidar(list(fontes), ancestrais[1, ]), "Pais ausentes")
  ancestrais$codigo_pai[2] <- 1L
  expect_error(tpu_consolidar(list(fontes), ancestrais), "Ciclo")
  writeLines("INSERT INTO ITENS (outro_esquema) VALUES (1);", arquivo)
  expect_error(tpu_ler_ancestrais_sql(arquivo, "A"), "Formato SQL")
})

test_that("código de pai fora do limite inteiro não se transforma em raiz", {
  doc <- xml2::read_html(test_path("fixtures", "tpu-legado.xls"), encoding = "ISO-8859-1")
  celula <- xml2::xml_find_first(doc, "//tr[td='2']/td[4]")
  xml2::xml_set_text(celula, "2147483648")
  arquivo <- tempfile(fileext = ".xls")
  withr::defer(unlink(arquivo))
  xml2::write_html(doc, arquivo, encoding = "ISO-8859-1")
  expect_error(tpu_ler(arquivo, "Estadual", "Estadual 1º Grau"), "fora do intervalo inteiro")
})

test_that("descoberta usa versão publicada e exclui planilhas de impressão", {
  arquivo <- tempfile(fileext = ".html")
  withr::defer(unlink(arquivo))
  writeLines(paste0('<table><tr><td>11/09/2026</td><td>Federal</td><td><select>',
    '<option value="versoes_tabelas/planilhas/82_Tabela_Assuntos_Federal.xls">Federal 1</option>',
    '<option value="versoes_tabelas/planilhas/82_Tabela_Assuntos_Impressao_Federal.xls">Federal 1</option>',
    '</select></td></tr></table>'), arquivo)
  x <- tpu_descobrir(arquivo, "A")
  expect_equal(nrow(x), 1L)
  expect_equal(x$versao, as.Date("2026-09-11"))
  expect_identical(x$segmento, "Federal")
  expect_match(x$url, "^https://www.cnj.jus.br/sgt/versoes_tabelas/")
  expect_error(tpu_descobrir(arquivo, "C"), "Catálogo TPU ausente")
  expect_error(tpu_descobrir_sql(arquivo, "A"), "Catálogo SQL ausente")
  writeLines(paste0('<table><tr><td>11/09/2026</td><td>',
    '<input id="dump_dados_oracle_postgres" value="82_dump_dados_oracle_postgres.sql">',
    '</td></tr></table>'), arquivo)
  sql <- tpu_descobrir_sql(arquivo, "A")
  expect_equal(sql$versao, as.Date("2026-09-11"))
  expect_identical(sql$formato, "sql")
  expect_match(sql$url, "enviarArquivo.php?", fixed = TRUE)
})

test_that("datasets distribuídos têm tipos, hierarquia e proveniência válidos", {
  for (nome in c("datajud_assuntos", "datajud_classes")) {
    env <- new.env()
    utils::data(list = nome, package = "datajud", envir = env)
    x <- env[[nome]]
    expect_silent(tpu_validar(x))
    expect_true(any(x$ativo))
    expect_true(any(!x$ativo))
    fontes <- attr(x, "tpu_fontes")
    expect_true(nrow(fontes) > 0L)
    expect_true(all(grepl("^[0-9a-f]{32}$", fontes$md5)))
    expect_true(all(grepl("^https://www.cnj.jus.br/sgt/", fontes$url)))
    expect_length(unique(fontes$versao), 1L)
  }
})
