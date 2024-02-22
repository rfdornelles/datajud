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
ler_processo <- function(dados) {

# cabecalho
  item <- purrr::pluck(dados,
                       # "hits",
                       # "hits",
                       # 1,
                       "_source")

  id <- purrr::pluck(item, "id")

  if(is.null(id)) {
    return(NULL)
  }

## dados
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

orgao_jogador_codigo <- purrr::pluck(item, "orgaoJulgador", "codigo")
orgao_jogador_nome <- purrr::pluck(item, "orgaoJulgador", "nome")
orgao_jogador_ibge <- purrr::pluck(item, "orgaoJulgador", "codigoMunicipioIBGE")

#
# movimentos <- purrr::pluck(item, "movimentos") |>
#   purrr::map_df(read_movimentos)

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
  orgao_jogador_codigo = orgao_jogador_codigo,
  orgao_jogador_nome = orgao_jogador_nome,
  orgao_jogador_ibge = orgao_jogador_ibge,
  # movimentos = movimentos
) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
      ~ifelse(is.null(.x), NA, .x)
    ))
  # ) |>
  # tidyr::nest(data = movimentos)

  processo <- processo |>
    dplyr::mutate(
      data_ajuizamento = lubridate::as_datetime(data_ajuizamento),
      data_atualizacao = lubridate::as_datetime(data_atualizacao)
    )

#cli::cli_alert_success(glue::glue("Processo {id} lido com sucesso!"))

return(processo)
}

### funcao para ler dados de processo

#' Title
#'
#' @param base
#'
#' @return
#' @export
#'
#' @examples
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
    ler_processo
  )

  print(resposta)
}

### funcao para ler movimentações de processo

#' Title
#'
#' @param base
#'
#' @return
#' @export
#'
#' @examples
#'
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
    ler_movimentos
  )

  print(resposta)
}
