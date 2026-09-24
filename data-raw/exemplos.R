# Execute na raiz: Rscript data-raw/exemplos.R
# Usa apenas fixtures sintéticas; nenhuma requisição real é realizada.
pkgload::load_all(quiet = TRUE)
local({
  ler <- function(nome) jsonlite::read_json(file.path("tests/testthat/fixtures", nome))
  hits <- list(ler("resposta_processo_valida.json"),
    ler("resposta_processo_multiplos_assuntos.json"))
  for (i in seq_along(hits)) hits[[i]]$sort <- list(1705320000000 + i, hits[[i]]$`_source`$id)
  testthat::local_mocked_bindings(
    agora_coleta_datajud = function() "2024-01-15T12:00:00Z",
    requisitar_api_datajud = function(cliente, endpoint, query) {
      selecionados <- if (query$size == 1L) {
        if (is.null(query$search_after)) hits[1] else hits[2]
      } else hits
      list(hits = list(total = list(value = 2L, relation = "eq"), hits = selecionados))
    }, .package = "datajud"
  )
  testthat::local_mocked_bindings(
    req_perform = function(...) stop("Rede proibida na geração de exemplos"),
    .package = "httr2"
  )
  cliente <- datajud_cliente(api_key = "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=")
  resultado <- datajud_pesquisar_processos("TJSP", assunto_codigo = 899,
    size = 2, cliente = cliente)
  saveRDS(resultado, "inst/extdata/exemplo-resultado.rds", version = 2)
  temporario <- withr::local_tempdir()
  coleta <- datajud_coletar_processos("TJSP", temporario, assunto_codigo = 899,
    size = 1, pausa = 0, cliente = cliente)
  destino <- "inst/extdata/exemplo-coleta"
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  stopifnot(all(file.copy(c(coleta$manifesto, coleta$arquivos), destino, overwrite = TRUE)))
})
