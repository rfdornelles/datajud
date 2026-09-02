## Funções auxiliares e de alto nível para leitura dos dados já baixados do Datajud

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
        datahora_movimento,
        tz = "UTC")
    ) |>
    dplyr::arrange(datahora_movimento) |>
    dplyr::relocate(tribunal,
                    numero_processo,
                    datahora_movimento)

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
#' Após realizar uma pesquisa de processos com `datajud_pesquisar_classe_orgao`,
#' esta função permite ler e manipular os dados dos processos retornados.
#' @param base Lista de respostas de processos retornadas pela API.
#'
#' @return Um tibble contendo os metadados dos processos.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' resposta <- datajud_consultar_processo(processo, cliente)
#' datajud_ler_processo(resposta)
#' }


datajud_ler_processo <- function(base) {

  if (!is.list(base)) {
    stop("base deve ser uma lista de respostas do Datajud")
  }

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
#' @param base Lista contendo os dados dos processos retornados pela API.
#'
#' @return Imprime e retorna um data frame consolidado com as movimentações de todos os
#'         processos fornecidos. Cada linha representa uma movimentação específica, incluindo
#'         metadados relevantes para análises subsequentes.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' datajud_ler_movimentacoes(resposta)
#' }

datajud_ler_movimentacoes <- function(base) {

  if (!is.list(base)) {
    stop("base deve ser uma lista de respostas do Datajud")
  }

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
