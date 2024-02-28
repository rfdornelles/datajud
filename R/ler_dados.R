## Funções auxiliares e de alto nível para leitura dos dados já baixados do Datajud

ler_movimentos <- function(item) {

  item <- purrr::pluck(item,
                            # "hits",
                            # "hits",
                            # 1,
                            "_source")

  tribunal <- purrr::pluck(item, "tribunal")
  numero_processo <- purrr::pluck(item, "numeroProcesso")
  # data_ajuizamento <- purrr::pluck(item, "dataAjuizamento")
  # data_atualizacao <- purrr::pluck(item, "dataHoraUltimaAtualizacao")

  movimento <- purrr::pluck(item, "movimentos")

  if (is.null(movimento)) {
    return(NULL)
  }

  #print(movimento)
  # print(tribunal)
  # print(numero_processo)

  tabela_movimentos <- purrr::map_df(
    movimento,
    .f = ~{

  tibble::tibble(
    codigo_tpu = purrr::pluck(.x, "codigo"),
    nome_movimento = purrr::pluck(.x, "nome"),
    datahora_movimento = purrr::pluck(.x, "dataHora"),
    codigo_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "codigo"),
    descricao_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "descricao"),
    valor_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "valor"),
    nome_tabelado = purrr::pluck(.x, "complementosTabelados", 1, "nome"),
    codigo_orgao_julgador = purrr::pluck(.x, "orgaoJulgador", 1, "codigoOrgao"),
    nome_orgao_julgador = purrr::pluck(.x, "orgaoJulgador", 1, "nomeOrgao")
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ifelse(is.null(.x), NA, .x)
      )
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
  item <- purrr::pluck(dados,
                       "_source")

  # Extrair o ID do processo
  id <- purrr::pluck(item, "id")

  # Verificar se o ID é nulo e retornar NULL se for
  if(is.null(id)) {
    return(NULL)
  }

  ## Extrair os dados do processo
  tribunal <- purrr::pluck(item, "tribunal")
  numero_processo <- purrr::pluck(item, "numeroProcesso")
  data_ajuizamento <- purrr::pluck(item, "dataAjuizamento")
  data_atualizacao <- purrr::pluck(item, "dataHoraUltimaAtualizacao")
  grau <- purrr::pluck(item, "grau")
  nivel_sigilo <- purrr::pluck(item, "nivelSigilo")
  formato <- purrr::pluck(item, "formato", "nome")
  sistema <- purrr::pluck(item, "sistema", "nome")
  classe_tpu <- purrr::pluck(item, "classe", "codigo")
  classe_nome <- purrr::pluck(item, "classe", "nome")
  assuntos <- purrr::pluck(item, "assuntos") |>
    purrr::map_chr(
      ~paste0( .x$codigo, " / ", .x$nome,
               collapse = " | ")
    )
  orgao_julgador_codigo <- purrr::pluck(item, "orgaoJulgador", "codigo")
  orgao_julgador_nome <- purrr::pluck(item, "orgaoJulgador", "nome")
  orgao_julgador_ibge <- purrr::pluck(item, "orgaoJulgador", "codigoMunicipioIBGE")

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
    assuntos = assuntos,
    orgao_julgador_codigo = orgao_julgador_codigo,
    orgao_julgador_nome = orgao_julgador_nome,
    orgao_julgador_ibge = orgao_julgador_ibge
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ifelse(is.null(.x), NA, .x)
      )
    )

  # Converter as colunas de data para formato datetime
  processo <- processo |>
    dplyr::mutate(
      data_ajuizamento = lubridate::as_datetime(data_ajuizamento),
      data_atualizacao = lubridate::as_datetime(data_atualizacao)
    )

  return(processo)
}

### Função para ler os dados de um processo

#' Lê os dados de processos retornados pelo Datajud
#'
#' Após realizar uma pesquisa de processos com `datajud_pesquisar_classe_orgao`,
#' esta função permite ler e manipular os dados dos processos retornados.
#' Aceita tanto uma lista diretamente quanto o nome de uma variável global que contém a lista de processos.
#'
#' @param base Lista de processos ou o nome de uma variável global que contém os dados dos processos.
#'             O padrão é "datajud_resposta", assumindo que os dados foram armazenados com esse nome.
#'
#' @return Imprime e retorna um data frame contendo os metadados dos processos.
#'
#' @export
#'
#' @examples
#' # Após realizar uma pesquisa e armazenar os resultados em 'datajud_resposta':
#' datajud_ler_processo()
#' # Ou, se a lista de processos estiver armazenada em uma variável customizada:
#' datajud_ler_processo(base = minha_lista_processos)


datajud_ler_processo <- function(base = "datajud_resposta") {

  if (!is.list(base)) {
    if (!exists(base, envir = .GlobalEnv)) {
      stop("Base de dados não encontrada")
    }
    base <- get(base, envir = .GlobalEnv)
  }

  if (!is.list(base)) {
    stop("Base não é uma lista ou o nome de uma lista existente")
  }

# retornando os metadados do processo
  resposta <- purrr::map_df(
    base,
    ler_processo,
    .progress = TRUE
  )

  # evitar duplicação de resposta
  resposta <- dplyr::distinct(resposta)


  print(resposta)
}

### funcao para ler movimentações de processo

#' Lê as movimentações de processos retornadas pelo Datajud
#'
#' Esta função é utilizada para extrair e processar as movimentações dos processos
#' judiciais obtidos a partir de uma pesquisa no Datajud. Ela pode operar diretamente
#' sobre uma lista de processos ou sobre o nome de uma variável global que contém essa lista.
#' É ideal para análises detalhadas das etapas processuais e suas características.
#'
#' @param base Lista contendo os dados dos processos ou o nome de uma variável global que
#'             armazena esses dados. Por padrão, utiliza "datajud_resposta", assumindo que
#'             os dados foram previamente armazenados com esse nome.
#'
#' @return Imprime e retorna um data frame consolidado com as movimentações de todos os
#'         processos fornecidos. Cada linha representa uma movimentação específica, incluindo
#'         metadados relevantes para análises subsequentes.
#'
#' @export
#'
#' @examples
#' # Após realizar uma pesquisa e armazenar os resultados em 'datajud_resposta':
#' datajud_ler_movimentacoes()
#'
#' # Se os dados das movimentações estiverem armazenados em uma variável customizada:
#' datajud_ler_movimentacoes(base = minha_lista_movimentacoes)

datajud_ler_movimentacoes <- function(base = "datajud_resposta") {

  if (!is.list(base)) {
    if (!exists(base, envir = .GlobalEnv)) {
      stop("Base de dados não encontrada")
    }
    base <- get(base, envir = .GlobalEnv)
  }

  if (!is.list(base)) {
    stop("Base não é uma lista ou o nome de uma lista existente")
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
  print(resposta)
}
