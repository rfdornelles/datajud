# Orquestração pública da coleta incremental.

#' Coletar processos incrementalmente em arquivos NDJSON
#'
#' Executa a pesquisa página a página e grava cada página concluída em um
#' arquivo NDJSON independente. A função mantém no máximo uma página de hits
#' em memória e retorna somente caminhos e metadados. Se uma requisição falhar,
#' as páginas anteriores permanecem disponíveis e a coleta pode ser retomada
#' com os mesmos filtros e o mesmo diretório.
#'
#' @param tribunal Sigla do tribunal a consultar.
#' @param diretorio Diretório obrigatório e exclusivo da coleta.
#' @param assunto_codigo Vetor opcional de códigos de assunto.
#' @param classe_codigo Código opcional de uma única classe processual.
#' @param orgao_codigo Vetor opcional de códigos de órgão julgador.
#' @param size Quantidade de resultados solicitada por página, entre 1 e
#'   10.000.
#' @param limite_registros Máximo de registros a gravar. O padrão é 10.000.
#' @param limite_paginas Máximo de páginas com dados a gravar. O padrão é 100.
#' @param pausa Segundos entre requisições consecutivas, entre 0 e 60.
#' @param retomar Se `TRUE`, retoma uma coleta compatível existente. Se
#'   `FALSE`, exige que ainda não exista manifesto no diretório.
#' @param exigir_todos_assuntos Se `TRUE`, exige todos os assuntos informados;
#'   por padrão, qualquer assunto satisfaz o filtro.
#' @param cliente Objeto opcional criado por [datajud_cliente()]. Quando `NULL`,
#'   um cliente transitório é criado automaticamente.
#'
#' @return Objeto `datajud_coleta` com caminhos e metadados, sem carregar hits.
#' @export
#'
#' @examples
#' \dontrun{
#' coleta <- datajud_coletar_processos(
#'   tribunal = "TJSP",
#'   diretorio = "coleta-tjsp-899",
#'   assunto_codigo = 899,
#'   limite_registros = 1000,
#'   limite_paginas = 20
#' )
#' coleta$arquivos
#' }
datajud_coletar_processos <- function(
    tribunal,
    diretorio,
    assunto_codigo = NULL,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100L,
    limite_registros = 10000L,
    limite_paginas = 100L,
    pausa = 0.1,
    retomar = TRUE,
    exigir_todos_assuntos = FALSE,
    cliente = NULL) {
  tribunal <- validar_tribunal_pesquisa(tribunal)
  limite_registros <- validar_limite_coleta(
    limite_registros,
    "limite_registros"
  )
  limite_paginas <- validar_limite_coleta(limite_paginas, "limite_paginas")
  pausa <- validar_pausa_paginacao(pausa)
  retomar <- validar_retomar_coleta(retomar)
  if (!is.null(cliente)) {
    validar_cliente(cliente)
  }

  consulta <- criar_query_datajud(
    assunto_codigo = assunto_codigo,
    classe_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = size,
    exigir_todos_assuntos = exigir_todos_assuntos
  )
  consulta <- sanitizar_consulta_datajud(consulta)
  consulta_hash <- hash_consulta_coleta(consulta)
  diretorio <- preparar_diretorio_coleta(diretorio)
  caminho_manifesto <- file.path(
    diretorio,
    NOME_MANIFESTO_COLETA_DATAJUD
  )

  if (file.exists(caminho_manifesto)) {
    if (!retomar) {
      abortar_coleta_datajud(
        paste0(
          "O diret\u00F3rio j\u00E1 possui uma coleta. Use {.arg retomar} = TRUE ",
          "ou escolha outro diret\u00F3rio."
        ),
        "datajud_erro_coleta_incompativel"
      )
    }
    manifesto <- ler_manifesto_coleta(caminho_manifesto)
    manifesto <- validar_manifesto_coleta(
      manifesto,
      diretorio,
      tribunal,
      consulta_hash
    )
    manifesto <- reconciliar_paginas_orfas(
      manifesto,
      diretorio,
      caminho_manifesto
    )
  } else {
    existentes <- setdiff(
      list.files(diretorio, all.files = TRUE),
      c(".", "..")
    )
    if (length(existentes) > 0L) {
      abortar_coleta_datajud(
        paste0(
          "O diret\u00F3rio n\u00E3o est\u00E1 vazio e n\u00E3o possui um manifesto ",
          "compat\u00EDvel. Use outro diret\u00F3rio."
        ),
        "datajud_erro_coleta_incompativel"
      )
    }
    manifesto <- novo_manifesto_coleta(
      tribunal,
      consulta,
      consulta_hash,
      limite_registros,
      limite_paginas,
      pausa
    )
    salvar_manifesto_coleta(manifesto, caminho_manifesto)
  }

  manifesto$limites <- list(
    registros = limite_registros,
    paginas = limite_paginas,
    pausa_segundos = pausa
  )
  if (identical(manifesto$estado, "completa")) {
    return(resultado_coleta_datajud(
      manifesto,
      diretorio,
      caminho_manifesto
    ))
  }
  if (manifesto$contagens$registros >= limite_registros) {
    manifesto <- finalizar_manifesto_coleta(
      manifesto,
      "limite_registros",
      "limite_registros",
      caminho_manifesto
    )
    return(resultado_coleta_datajud(manifesto, diretorio, caminho_manifesto))
  }
  if (manifesto$contagens$paginas >= limite_paginas) {
    manifesto <- finalizar_manifesto_coleta(
      manifesto,
      "limite_paginas",
      "limite_paginas",
      caminho_manifesto
    )
    return(resultado_coleta_datajud(manifesto, diretorio, caminho_manifesto))
  }

  cliente <- resolver_cliente_datajud(cliente)
  ids_pagina_anterior <- ids_ultima_pagina_coleta(manifesto, diretorio)
  repeat {
    numero <- length(manifesto$paginas) + 1L
    cursor_utilizado <- manifesto$proximo_cursor
    consulta_pagina <- consulta
    if (!is.null(cursor_utilizado)) {
      consulta_pagina$search_after <- I(cursor_utilizado)
    }
    if (manifesto$contagens$requisicoes > 0L ||
        manifesto$contagens$paginas > 0L) {
      aguardar_proxima_pagina(pausa)
    }
    manifesto$estado <- "em_andamento"
    manifesto$falha <- NULL
    manifesto$pagina_em_processamento <- as.integer(numero)
    manifesto$atualizado_em <- agora_coleta_datajud()
    salvar_manifesto_coleta(manifesto, caminho_manifesto)

    tentativa <- tryCatch(
      executar_pesquisa_datajud(
        tribunal = tribunal,
        consulta = consulta_pagina,
        cliente = cliente,
        pagina = as.integer(numero)
      ),
      error = identity
    )
    if (inherits(tentativa, "condition")) {
      manifesto <- registrar_falha_coleta(
        manifesto,
        caminho_manifesto,
        numero,
        tentativa,
        cliente
      )
      abortar_coleta_datajud(
        paste0(
          "A coleta foi interrompida na p\u00E1gina ", numero,
          ". As p\u00E1ginas conclu\u00EDdas foram preservadas em ", diretorio, "."
        ),
        "datajud_erro_coleta_interrompida",
        parent = tentativa
      )
    }

    resultado <- tentativa
    manifesto$contagens$requisicoes <- as.integer(
      manifesto$contagens$requisicoes + 1L
    )
    if (length(resultado$hits) == 0L) {
      manifesto["proximo_cursor"] <- list(NULL)
      manifesto$pagina_terminal_vazia <- as.integer(numero)
      manifesto <- finalizar_manifesto_coleta(
        manifesto,
        "completa",
        "pagina_vazia",
        caminho_manifesto
      )
      return(resultado_coleta_datajud(
        manifesto,
        diretorio,
        caminho_manifesto
      ))
    }

    restantes <- limite_registros - manifesto$contagens$registros
    quantidade <- min(length(resultado$hits), restantes)
    hits <- resultado$hits[seq_len(quantidade)]
    ids_pagina_atual <- extrair_ids_hits(hits)
    if (length(intersect(ids_pagina_anterior, ids_pagina_atual)) > 0L) {
      erro_ids <- rlang::error_cnd(
        "datajud_erro_paginacao",
        message = paste0(
          "A p\u00E1gina recebida repetiu processo da p\u00E1gina anterior. ",
          "A coleta foi interrompida para evitar duplica\u00E7\u00E3o."
        )
      )
      manifesto <- registrar_falha_coleta(
        manifesto,
        caminho_manifesto,
        numero,
        erro_ids,
        cliente
      )
      abortar_coleta_datajud(
        conditionMessage(erro_ids),
        "datajud_erro_coleta_interrompida",
        parent = erro_ids
      )
    }
    proximo_cursor <- extrair_cursor_resultado(hits[[length(hits)]])
    if (cursor_ja_gravado(proximo_cursor, manifesto)) {
      erro_cursor <- rlang::error_cnd(
        "datajud_erro_paginacao",
        message = paste0(
          "A API retornou um cursor j\u00E1 gravado. A coleta foi interrompida ",
          "para evitar um ciclo de pagina\u00E7\u00E3o."
        )
      )
      manifesto <- registrar_falha_coleta(
        manifesto,
        caminho_manifesto,
        numero,
        erro_cursor,
        cliente
      )
      abortar_coleta_datajud(
        conditionMessage(erro_cursor),
        "datajud_erro_coleta_interrompida",
        parent = erro_cursor
      )
    }

    arquivo <- nome_arquivo_pagina(numero)
    caminho_pagina <- file.path(diretorio, arquivo)
    checksum <- tryCatch(
      escrever_pagina_ndjson(hits, caminho_pagina),
      error = identity
    )
    if (inherits(checksum, "condition")) {
      manifesto <- registrar_falha_coleta(
        manifesto,
        caminho_manifesto,
        numero,
        checksum,
        cliente
      )
      abortar_coleta_datajud(
        paste0("A grava\u00E7\u00E3o da p\u00E1gina ", numero, " falhou."),
        "datajud_erro_coleta_interrompida",
        parent = checksum
      )
    }
    pagina <- metadados_pagina_coleta(
      numero = numero,
      arquivo = arquivo,
      hits = hits,
      cursor_utilizado = cursor_utilizado,
      proximo_cursor = proximo_cursor,
      checksum = checksum,
      recuperada = FALSE,
      total_valor = resultado$metadados$total_valor,
      total_relacao = resultado$metadados$total_relacao
    )
    manifesto$paginas <- append(manifesto$paginas, list(pagina))
    manifesto$proximo_cursor <- proximo_cursor
    manifesto <- atualizar_contagens_manifesto(manifesto)
    manifesto$pagina_em_processamento <- NULL
    manifesto$atualizado_em <- agora_coleta_datajud()

    atingiu_total <- identical(resultado$metadados$total_relacao, "eq") &&
      manifesto$contagens$registros >= resultado$metadados$total_valor
    if (atingiu_total) {
      manifesto["proximo_cursor"] <- list(NULL)
      manifesto <- finalizar_manifesto_coleta(
        manifesto,
        "completa",
        "total_informado",
        caminho_manifesto
      )
      return(resultado_coleta_datajud(
        manifesto,
        diretorio,
        caminho_manifesto
      ))
    }
    if (manifesto$contagens$registros >= limite_registros) {
      manifesto <- finalizar_manifesto_coleta(
        manifesto,
        "limite_registros",
        "limite_registros",
        caminho_manifesto
      )
      return(resultado_coleta_datajud(
        manifesto,
        diretorio,
        caminho_manifesto
      ))
    }
    if (manifesto$contagens$paginas >= limite_paginas) {
      manifesto <- finalizar_manifesto_coleta(
        manifesto,
        "limite_paginas",
        "limite_paginas",
        caminho_manifesto
      )
      return(resultado_coleta_datajud(
        manifesto,
        diretorio,
        caminho_manifesto
      ))
    }
    salvar_manifesto_coleta(manifesto, caminho_manifesto)
    ids_pagina_anterior <- ids_pagina_atual
    resultado <- NULL
    hits <- NULL
  }
}
