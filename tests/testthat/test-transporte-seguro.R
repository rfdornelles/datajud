erro_http_simulado <- function(status, cliente = NULL, corpo = "detalhe secreto") {
  executar <- getFromNamespace("executar_requisicao_http", "datajud")
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = status, body = charToRaw(corpo))
  })

  tryCatch(
    executar("https://segredo.exemplo/token", cliente = cliente),
    error = identity
  )
}

test_that("status HTTP têm classes e mensagens acionáveis", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  casos <- list(
    `400` = c("datajud_erro_requisicao", "Revise os filtros"),
    `401` = c("datajud_erro_autenticacao", "Reconfigure DATAJUD_API_KEY"),
    `403` = c("datajud_erro_autenticacao", "Reconfigure DATAJUD_API_KEY"),
    `404` = c("datajud_erro_nao_encontrado", "não foi encontrado"),
    `429` = c("datajud_erro_limite", "limite de requisições"),
    `500` = c("datajud_erro_servidor", "temporariamente indisponível"),
    `502` = c("datajud_erro_servidor", "temporariamente indisponível"),
    `503` = c("datajud_erro_servidor", "temporariamente indisponível")
  )

  for (status in names(casos)) {
    erro <- erro_http_simulado(as.integer(status), cliente)
    expect_s3_class(erro, casos[[status]][[1]])
    expect_s3_class(erro, "datajud_erro_http")
    expect_match(conditionMessage(erro), casos[[status]][[2]], fixed = TRUE)
    expect_match(conditionMessage(erro), paste0("HTTP ", status), fixed = TRUE)
  }
})

test_that("mensagens HTTP sanitizadas permanecem estáveis", {
  cliente <- datajud::datajud_cliente(chave_publica_teste())

  expect_snapshot({
    for (status in c(400L, 401L, 403L, 404L, 429L, 500L, 502L, 503L)) {
      erro <- erro_http_simulado(status, cliente)
      cat(
        paste(status, class(erro)[[1]], conditionMessage(erro), sep = " | "),
        "\n",
        sep = ""
      )
    }
  })
})

test_that("política repete apenas respostas transitórias", {
  criar <- getFromNamespace("criar_requisicao_http", "datajud")
  cliente <- datajud::datajud_cliente(
    chave_publica_teste(),
    max_tentativas = 4
  )
  requisicao <- criar("https://exemplo.org", cliente = cliente)
  transitoria <- requisicao$policies$retry_is_transient

  status <- c(400L, 401L, 403L, 404L, 429L, 500L, 501L, 502L, 503L, 504L)
  resultado <- vapply(
    status,
    function(codigo) transitoria(httr2::response(status_code = codigo)),
    logical(1)
  )

  expect_identical(
    resultado,
    c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)
  )
  expect_identical(requisicao$policies$retry_max_tries, 4L)
})

test_that("política respeita Retry-After", {
  criar <- getFromNamespace("criar_requisicao_http", "datajud")
  cliente <- datajud::datajud_cliente(chave_publica_teste())
  requisicao <- criar("https://exemplo.org", cliente = cliente)
  resposta <- httr2::response(
    status_code = 429,
    headers = list(`Retry-After` = "7")
  )

  expect_identical(requisicao$policies$retry_after(resposta), 7)
})

test_that("falhas de autenticação não iniciam renovação nem nova tentativa", {
  cliente <- datajud::datajud_cliente(
    chave_publica_teste(2),
    max_tentativas = 5
  )
  executar <- getFromNamespace("executar_requisicao_http", "datajud")
  chamadas <- 0L
  httr2::local_mocked_responses(function(req) {
    chamadas <<- chamadas + 1L
    httr2::response(status_code = 401)
  })

  expect_error(
    executar("https://exemplo.org", cliente = cliente),
    class = "datajud_erro_autenticacao"
  )
  expect_identical(chamadas, 1L)
})

test_that("erros não expõem URL, corpo nem credencial", {
  chave <- chave_publica_teste(3)
  cliente <- datajud::datajud_cliente(chave)
  segredo_corpo <- "corpo-reservado-123"
  erro_status <- erro_http_simulado(400L, cliente, segredo_corpo)

  httr2::local_mocked_responses(function(req) {
    stop(paste(req$url, chave, segredo_corpo))
  })
  executar <- getFromNamespace("executar_requisicao_http", "datajud")
  erro_conexao <- tryCatch(
    executar("https://segredo.exemplo/token", cliente = cliente),
    error = identity
  )

  mensagens <- c(conditionMessage(erro_status), conditionMessage(erro_conexao))
  expect_s3_class(erro_conexao, "datajud_erro_conexao")
  expect_false(any(grepl("segredo.exemplo", mensagens, fixed = TRUE)))
  expect_false(any(grepl(segredo_corpo, mensagens, fixed = TRUE)))
  expect_false(any(grepl(chave, mensagens, fixed = TRUE)))
})

test_that("API exige JSON e não reproduz o corpo inválido", {
  chave <- chave_publica_teste()
  cliente <- datajud::datajud_cliente(chave)
  requisitar <- getFromNamespace("requisitar_api_datajud", "datajud")

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "text/html"),
      body = charToRaw(paste("segredo", chave))
    )
  })
  erro_tipo <- expect_error(
    requisitar(cliente, "https://exemplo.org", list()),
    class = "datajud_erro_conteudo"
  )

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw(paste("JSON inválido", chave))
    )
  })
  erro_json <- expect_error(
    requisitar(cliente, "https://exemplo.org", list()),
    class = "datajud_erro_conteudo"
  )

  expect_false(grepl(chave, conditionMessage(erro_tipo), fixed = TRUE))
  expect_false(grepl(chave, conditionMessage(erro_json), fixed = TRUE))
})

test_that("extração da chave valida contexto e formato", {
  extrair <- getFromNamespace("extrair_chave_publica_cnj", "datajud")
  chave <- chave_publica_teste()

  html_contexto <- paste0(
    "<html><strong>APIKey atual</strong><p><strong>",
    chave,
    "</strong></p></html>"
  )
  html_fallback <- paste0(
    "<html><p>Authorization: APIKey ",
    chave,
    "</p></html>"
  )

  expect_identical(extrair(html_contexto), chave)
  expect_identical(extrair(html_fallback), chave)
  expect_error(
    extrair("<html><strong>APIKey antiga</strong><strong>sem-chave</strong></html>"),
    class = "datajud_erro_credencial"
  )
})

test_that("obtenção da chave rejeita conteúdo e status inesperados", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      headers = list(`Content-Type` = "application/json"),
      body = charToRaw("{}")
    )
  })
  expect_error(
    datajud::obter_chave_publica_cnj(),
    class = "datajud_erro_conteudo"
  )

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 404,
      headers = list(`Content-Type` = "text/html")
    )
  })
  expect_error(
    datajud::obter_chave_publica_cnj(),
    class = "datajud_erro_nao_encontrado"
  )
})

test_that("cliente rejeita credenciais malformadas", {
  withr::local_envvar(DATAJUD_API_KEY = "não-é-chave")

  expect_error(
    datajud::datajud_cliente(),
    class = "datajud_erro_credencial"
  )
  expect_error(
    datajud::datajud_cliente(api_key = character()),
    class = "datajud_erro_credencial"
  )
  expect_error(
    datajud::datajud_cliente(api_key = NA_character_),
    class = "datajud_erro_credencial"
  )
})
