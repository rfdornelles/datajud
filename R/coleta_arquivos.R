# Arquivos, reconciliação e resultados da coleta incremental.

nome_arquivo_pagina <- function(numero) {
  sprintf("pagina-%06d.ndjson", as.integer(numero))
}

escrever_pagina_ndjson <- function(hits, caminho) {
  if (file.exists(caminho)) {
    abortar_coleta_datajud(
      "O arquivo {.file {basename(caminho)}} j\u00E1 existe e n\u00E3o ser\u00E1 sobrescrito.",
      "datajud_erro_coleta_integridade"
    )
  }
  temporario <- tempfile(".pagina-", tmpdir = dirname(caminho))
  on.exit(unlink(temporario), add = TRUE)
  conexao <- file(temporario, open = "wb")
  tryCatch(
    {
      for (hit in hits) {
        linha <- paste0(
          enc2utf8(as.character(serializar_objeto_coleta(hit))),
          "\n"
        )
        writeBin(charToRaw(linha), conexao)
      }
    },
    finally = close(conexao)
  )
  if (!file.rename(temporario, caminho)) {
    abortar_coleta_datajud(
      "N\u00E3o foi poss\u00EDvel concluir atomicamente a p\u00E1gina da coleta."
    )
  }
  unname(tools::md5sum(caminho))
}

ler_pagina_ndjson <- function(caminho) {
  linhas <- readLines(caminho, warn = FALSE, encoding = "UTF-8")
  if (length(linhas) == 0L || any(!nzchar(linhas))) {
    abortar_coleta_datajud(
      "Uma p\u00E1gina NDJSON \u00F3rf\u00E3 est\u00E1 vazia ou malformada.",
      "datajud_erro_coleta_integridade"
    )
  }
  tryCatch(
    lapply(
      linhas,
      jsonlite::fromJSON,
      simplifyVector = FALSE
    ),
    error = function(cnd) {
      abortar_coleta_datajud(
        "Uma p\u00E1gina NDJSON \u00F3rf\u00E3 cont\u00E9m JSON inv\u00E1lido.",
        "datajud_erro_coleta_integridade",
        parent = cnd
      )
    }
  )
}

ids_ultima_pagina_coleta <- function(manifesto, diretorio) {
  if (length(manifesto$paginas) == 0L) {
    return(character())
  }
  ultima <- manifesto$paginas[[length(manifesto$paginas)]]$arquivo
  hits <- ler_pagina_ndjson(file.path(diretorio, ultima))
  ids <- extrair_ids_hits(hits)
  hits <- NULL
  ids
}

cursores_ja_gravados <- function(manifesto) {
  if (length(manifesto$paginas) == 0L) {
    return(list())
  }
  lapply(manifesto$paginas, function(pagina) pagina$proximo_cursor)
}

cursor_ja_gravado <- function(cursor, manifesto) {
  any(vapply(
    cursores_ja_gravados(manifesto),
    identical,
    logical(1),
    y = cursor
  ))
}

metadados_pagina_coleta <- function(numero, arquivo, hits, cursor_utilizado,
                                     proximo_cursor, checksum, recuperada,
                                     total_valor = NULL,
                                     total_relacao = NULL) {
  ids <- extrair_ids_hits(hits)
  list(
    numero = as.integer(numero),
    arquivo = arquivo,
    registros = as.integer(length(hits)),
    primeiro_id = ids[[1]],
    ultimo_id = ids[[length(ids)]],
    cursor_utilizado = cursor_utilizado,
    proximo_cursor = proximo_cursor,
    checksum_md5 = checksum,
    recuperada = recuperada,
    total_valor = total_valor,
    total_relacao = total_relacao
  )
}

atualizar_contagens_manifesto <- function(manifesto) {
  manifesto$contagens$paginas <- as.integer(length(manifesto$paginas))
  manifesto$contagens$registros <- as.integer(sum(vapply(
    manifesto$paginas,
    function(pagina) as.integer(pagina$registros),
    integer(1)
  )))
  manifesto
}

listar_paginas_orfas <- function(manifesto, diretorio) {
  arquivos <- list.files(
    diretorio,
    pattern = "^pagina-[0-9]{6,}\\.ndjson$",
    full.names = FALSE
  )
  referenciados <- vapply(
    manifesto$paginas,
    function(pagina) pagina$arquivo,
    character(1)
  )
  setdiff(arquivos, referenciados)
}

reconciliar_paginas_orfas <- function(manifesto, diretorio,
                                       caminho_manifesto) {
  orfaos <- listar_paginas_orfas(manifesto, diretorio)
  if (length(orfaos) > 0L && identical(manifesto$estado, "completa")) {
    abortar_coleta_datajud(
      "Uma coleta completa n\u00E3o pode conter p\u00E1ginas \u00F3rf\u00E3s.",
      "datajud_erro_coleta_integridade"
    )
  }
  ids_anteriores <- ids_ultima_pagina_coleta(manifesto, diretorio)
  while (length(orfaos) > 0L) {
    numero <- length(manifesto$paginas) + 1L
    esperado <- nome_arquivo_pagina(numero)
    if (!identical(sort(orfaos), esperado)) {
      abortar_coleta_datajud(
        "A coleta cont\u00E9m p\u00E1ginas \u00F3rf\u00E3s que n\u00E3o podem ser reconciliadas.",
        "datajud_erro_coleta_integridade"
      )
    }
    caminho <- file.path(diretorio, esperado)
    hits <- ler_pagina_ndjson(caminho)
    ids <- extrair_ids_hits(hits)
    if (length(intersect(ids_anteriores, ids)) > 0L) {
      abortar_coleta_datajud(
        "Uma p\u00E1gina \u00F3rf\u00E3 repetiu processo da p\u00E1gina anterior.",
        "datajud_erro_coleta_integridade"
      )
    }
    proximo_cursor <- extrair_cursor_resultado(hits[[length(hits)]])
    if (cursor_ja_gravado(proximo_cursor, manifesto)) {
      abortar_coleta_datajud(
        "Uma p\u00E1gina \u00F3rf\u00E3 repetiu um cursor j\u00E1 gravado.",
        "datajud_erro_coleta_integridade"
      )
    }
    pagina <- metadados_pagina_coleta(
      numero = numero,
      arquivo = esperado,
      hits = hits,
      cursor_utilizado = manifesto$proximo_cursor,
      proximo_cursor = proximo_cursor,
      checksum = unname(tools::md5sum(caminho)),
      recuperada = TRUE
    )
    manifesto$paginas <- append(manifesto$paginas, list(pagina))
    manifesto$proximo_cursor <- proximo_cursor
    manifesto <- atualizar_contagens_manifesto(manifesto)
    manifesto$contagens$requisicoes <- as.integer(
      manifesto$contagens$requisicoes + 1L
    )
    manifesto$estado <- "em_andamento"
    manifesto$atualizado_em <- agora_coleta_datajud()
    salvar_manifesto_coleta(manifesto, caminho_manifesto)
    ids_anteriores <- ids
    orfaos <- setdiff(orfaos, esperado)
  }
  manifesto
}

novo_manifesto_coleta <- function(tribunal, consulta, consulta_hash,
                                   limite_registros, limite_paginas, pausa) {
  instante <- agora_coleta_datajud()
  list(
    versao_esquema = VERSAO_ESQUEMA_COLETA_DATAJUD,
    pacote_versao = as.character(utils::packageVersion("datajud")),
    criado_em = instante,
    atualizado_em = instante,
    estado = "em_andamento",
    tribunal = tribunal,
    consulta = sanitizar_consulta_datajud(consulta),
    consulta_hash = consulta_hash,
    limites = list(
      registros = limite_registros,
      paginas = limite_paginas,
      pausa_segundos = pausa
    ),
    paginas = list(),
    contagens = list(registros = 0L, paginas = 0L, requisicoes = 0L),
    proximo_cursor = NULL,
    falha = NULL
  )
}

resultado_coleta_datajud <- function(manifesto, diretorio,
                                      caminho_manifesto) {
  arquivos <- vapply(
    manifesto$paginas,
    function(pagina) file.path(diretorio, pagina$arquivo),
    character(1)
  )
  structure(
    list(
      diretorio = diretorio,
      manifesto = caminho_manifesto,
      arquivos = unname(arquivos),
      consulta = sanitizar_consulta_datajud(manifesto$consulta),
      paginas = manifesto$paginas,
      metadados = list(
        tribunal = manifesto$tribunal,
        versao_esquema = as.integer(manifesto$versao_esquema),
        estado = manifesto$estado,
        registros = as.integer(manifesto$contagens$registros),
        paginas = as.integer(manifesto$contagens$paginas),
        requisicoes = as.integer(manifesto$contagens$requisicoes),
        proximo_cursor = manifesto$proximo_cursor,
        falha = manifesto$falha,
        criado_em = manifesto$criado_em,
        atualizado_em = manifesto$atualizado_em
      )
    ),
    class = "datajud_coleta"
  )
}

mensagem_falha_coleta <- function(cnd, cliente) {
  mensagem <- conditionMessage(cnd)
  chave <- cliente$api_key
  if (is.character(chave) && length(chave) == 1L && nzchar(chave)) {
    mensagem <- gsub(chave, "[REMOVIDO]", mensagem, fixed = TRUE)
  }
  mensagem
}

registrar_falha_coleta <- function(manifesto, caminho_manifesto, pagina,
                                    cnd, cliente) {
  manifesto$estado <- "parcial"
  manifesto$falha <- list(
    pagina = as.integer(pagina),
    mensagem = mensagem_falha_coleta(cnd, cliente),
    classes = class(cnd)
  )
  manifesto$atualizado_em <- agora_coleta_datajud()
  salvar_manifesto_coleta(manifesto, caminho_manifesto)
  manifesto
}

finalizar_manifesto_coleta <- function(manifesto, estado, motivo,
                                        caminho_manifesto) {
  manifesto$estado <- estado
  manifesto$motivo_termino <- motivo
  manifesto$atualizado_em <- agora_coleta_datajud()
  salvar_manifesto_coleta(manifesto, caminho_manifesto)
  manifesto
}
