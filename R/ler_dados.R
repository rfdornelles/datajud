## Funções auxiliares e de alto nível para leitura dos dados do Datajud

normalizar_base_leitura <- function(base) {
  if (inherits(base, "datajud_resultado")) {
    validar_datajud_resultado(base)
    return(base$hits)
  }
  if (inherits(base, "datajud_coleta")) {
    cli::cli_abort(c(
      "Uma coleta completa n\u00E3o \u00E9 materializada implicitamente.",
      "i" = paste0(
        "Use {.fun datajud_ler_pagina} e forne\u00E7a a p\u00E1gina ao leitor."
      )
    ))
  }
  if (!is.list(base)) {
    cli::cli_abort(
      "{.arg base} deve ser uma lista ou um objeto `datajud_resultado`."
    )
  }
  base
}

esquema_movimentos_vazio <- function() {
  tibble::tibble(
    tribunal = character(), numero_processo = character(),
    datahora_movimento = as.POSIXct(character(), tz = "UTC"),
    codigo_tpu = integer(), nome_movimento = character(),
    codigo_tabelado = integer(), descricao_tabelado = character(),
    valor_tabelado = character(), nome_tabelado = character(),
    codigo_orgao_julgador = integer(), nome_orgao_julgador = character()
  )
}

ler_movimentos <- function(item) {

  item <- purrr::pluck(item, "_source", .default = list())

  tribunal <- purrr::pluck(item, "tribunal", .default = NA_character_)
  numero_processo <- purrr::pluck(item, "numeroProcesso", .default = NA_character_)
  # data_ajuizamento <- purrr::pluck(item, "dataAjuizamento")
  # data_atualizacao <- purrr::pluck(item, "dataHoraUltimaAtualizacao")

  movimento <- purrr::pluck(item, "movimentos", .default = list())

  if (length(movimento) == 0L) {
    return(esquema_movimentos_vazio())
  }

  #print(movimento)
  # print(tribunal)
  # print(numero_processo)

  tabela_movimentos <- purrr::map_df(
    movimento,
    .f = ~{

  tibble::tibble(
    codigo_tpu = purrr::pluck(.x, "codigo", .default = NA_integer_),
    nome_movimento = purrr::pluck(.x, "nome", .default = NA_character_),
    datahora_movimento = purrr::pluck(.x, "dataHora", .default = NA_character_),
    codigo_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "codigo", .default = NA_integer_),
    descricao_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "descricao", .default = NA_character_),
    valor_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "valor", .default = NA_character_),
    nome_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "nome", .default = NA_character_),
    codigo_orgao_julgador = purrr::pluck(.x, "orgaoJulgador", 1, "codigoOrgao", .default = NA_integer_),
    nome_orgao_julgador = purrr::pluck(.x, "orgaoJulgador", 1, "nomeOrgao", .default = NA_character_)
  )
  })

  tabela_movimentos <- tabela_movimentos |>
    dplyr::mutate(
      tribunal = tribunal,
      numero_processo = numero_processo,
      datahora_movimento = lubridate::as_datetime(
        .data$datahora_movimento,
        tz = "UTC")
    ) |>
    dplyr::arrange(.data$datahora_movimento) |>
    dplyr::relocate(dplyr::all_of(c(
      "tribunal", "numero_processo", "datahora_movimento"
    )))

    return(tabela_movimentos)
}

###
# Função para ler os dados de um processo
ler_processo <- function(dados) {

  # Extrair o item do objeto de dados
  item <- purrr::pluck(dados, "_source", .default = list())

  id <- purrr::pluck(item, "id", .default = NA_character_)

  if (length(id) != 1L || is.na(id) || !nzchar(id)) {
    rlang::abort("O campo id \u00e9 obrigat\u00f3rio para identificar o processo.")
  }

  ## Extrair os dados do processo
  tribunal <- purrr::pluck(item, "tribunal", .default = NA_character_)
  numero_processo <- purrr::pluck(item, "numeroProcesso", .default = NA_character_)
  data_ajuizamento <- purrr::pluck(item, "dataAjuizamento", .default = NA_character_)
  data_atualizacao <- purrr::pluck(item, "dataHoraUltimaAtualizacao", .default = NA_character_)
  grau <- purrr::pluck(item, "grau", .default = NA_character_)
  nivel_sigilo <- purrr::pluck(item, "nivelSigilo", .default = NA_integer_)
  formato <- purrr::pluck(item, "formato", "nome", .default = NA_character_)
  sistema <- purrr::pluck(item, "sistema", "nome", .default = NA_character_)
  classe_tpu <- purrr::pluck(item, "classe", "codigo", .default = NA_integer_)
  classe_nome <- purrr::pluck(item, "classe", "nome", .default = NA_character_)
  assuntos <- purrr::pluck(item, "assuntos", .default = list())
  assuntos_tbl <- purrr::map_dfr(assuntos, ~tibble::tibble(
    codigo = purrr::pluck(.x, "codigo", .default = NA_integer_),
    nome = purrr::pluck(.x, "nome", .default = NA_character_)
  ))
  assuntos_resumo <- if (nrow(assuntos_tbl) == 0L) NA_character_ else
    paste0(assuntos_tbl$codigo, " / ", assuntos_tbl$nome, collapse = " | ")
  orgao_julgador_codigo <- purrr::pluck(item, "orgaoJulgador", "codigo", .default = NA_integer_)
  orgao_julgador_nome <- purrr::pluck(item, "orgaoJulgador", "nome", .default = NA_character_)
  orgao_julgador_ibge <- purrr::pluck(item, "orgaoJulgador", "codigoMunicipioIBGE", .default = NA_integer_)

  # Criar um tibble com os dados do processo
  processo <- tibble::tibble(
    id = id,
    tribunal = tribunal,
    numero_processo = numero_processo,
    data_ajuizamento = data_ajuizamento,
    data_atualizacao = data_atualizacao,
    grau = grau,
    nivel_sigilo = nivel_sigilo,
    formato = formato,
    sistema = sistema,
    classe_tpu = classe_tpu,
    classe_nome = classe_nome,
    assuntos = list(assuntos_tbl),
    assuntos_resumo = assuntos_resumo,
    orgao_julgador_codigo = orgao_julgador_codigo,
    orgao_julgador_nome = orgao_julgador_nome,
    orgao_julgador_ibge = orgao_julgador_ibge
  )

  # Converter as colunas de data para formato datetime
  processo <- processo |>
    dplyr::mutate(
      data_ajuizamento = lubridate::as_datetime(data_ajuizamento),
      data_atualizacao = lubridate::as_datetime(data_atualizacao)
    )

  return(processo)
}

#' Desaninha os assuntos estruturados dos processos
#'
#' @param dados Resultado de `datajud_ler_processo()`.
#' @return Tibble com uma linha por processo e assunto.
#' @export
datajud_desaninhar_assuntos <- function(dados) {
  if (!is.data.frame(dados) || !"assuntos" %in% names(dados)) {
    stop("dados deve conter a coluna assuntos")
  }
  purrr::map_dfr(seq_len(nrow(dados)), function(i) {
    assuntos <- dados$assuntos[[i]]
    if (!is.data.frame(assuntos) || nrow(assuntos) == 0L) {
      return(tibble::tibble(
        id = character(), codigo = integer(), nome = character()
      ))
    }
    dplyr::mutate(assuntos, id = dados$id[[i]], .before = 1L)
  })
}

### Função para ler os dados de um processo

#' Lê os dados de processos retornados pelo Datajud
#'
#' Lê a lista retornada por [datajud_consultar_processo()] ou um objeto
#' `datajud_resultado` criado por [datajud_pesquisar_processos()] ou
#' [datajud_ler_pagina()]. Uma `datajud_coleta` inteira não é materializada
#' implicitamente.
#' @param base Lista de respostas retornadas pela API ou objeto
#'   `datajud_resultado`.
#'
#' @return Um tibble contendo os metadados dos processos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cliente <- datajud_cliente()
#' resposta <- datajud_consultar_processo(
#'   "0000001-89.2020.8.26.0000",
#'   tribunal = "TJSP",
#'   cliente = cliente
#' )
#' datajud_ler_processo(resposta)
#'
#' pesquisa <- datajud_pesquisar_processos("TJSP", classe_codigo = 1116)
#' datajud_ler_processo(pesquisa)
#' }


datajud_ler_processo <- function(base) {
  base <- normalizar_base_leitura(base)

# retornando os metadados do processo
  resposta <- purrr::map_df(
    base,
    ler_processo,
    .progress = TRUE
  )

  # evitar duplicação de resposta
  resposta <- dplyr::distinct(resposta)


  resposta
}

### funcao para ler movimentações de processo

#' Lê as movimentações de processos retornadas pelo Datajud
#'
#' Esta função é utilizada para extrair e processar as movimentações dos processos
#' judiciais obtidos a partir de uma pesquisa no Datajud. Ela opera sobre uma lista
#' de processos fornecida diretamente.
#' É ideal para análises detalhadas das etapas processuais e suas características.
#'
#' @param base Lista contendo os dados retornados pela API ou objeto
#'   `datajud_resultado` criado por uma pesquisa ou pela leitura de uma página.
#'
#' @return Um tibble consolidado com uma linha por movimentação. Quando nenhum
#'   processo possui movimentações, retorna um tibble vazio com esquema estável.
#' @importFrom rlang .data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cliente <- datajud_cliente()
#' resposta <- datajud_consultar_processo(
#'   "0000001-89.2020.8.26.0000",
#'   tribunal = "TJSP",
#'   cliente = cliente
#' )
#' datajud_ler_movimentacoes(resposta)
#' }

datajud_ler_movimentacoes <- function(base) {
  base <- normalizar_base_leitura(base)

  # retornando os metadados do processo
  resposta <- purrr::map_df(
    base,
    ler_movimentos,
    .progress = TRUE
  )

  # evitar duplicação de resposta
  resposta <- dplyr::distinct(resposta)

  # saída
  resposta
}
