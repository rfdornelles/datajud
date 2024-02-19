read_movimentos <- function(movimento) {

  tibble::tibble(
    codigo_tpu = purrr::pluck(movimento, "codigo"),
    nome_movimento = purrr::pluck(movimento, "nome"),
    datahora_movimento = purrr::pluck(movimento, "dataHora"),
    codigo_tabelado = purrr::pluck(movimento, "complementosTabelados", 1, "codigo"),
    descricao_tabelado = purrr::pluck(movimento, "complementosTabelados", 1, "descricao"),
    valor_tabelado = purrr::pluck(movimento, "complementosTabelados", 1, "valor"),
    nome_tabelado = purrr::pluck(movimento, "complementosTabelados", 1, "nome"),
    codigo_orgao_julgador = purrr::pluck(movimento, "orgaoJulgador", 1, "codigoOrgao"),
    nome_orgao_julgador = purrr::pluck(movimento, "orgaoJulgador", 1, "nomeOrgao")
  ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::everything(),
        ~ifelse(is.null(.x), NA, .x)
      )
    ) |>
    dplyr::mutate(
      datahora_movimento = lubridate::as_datetime(datahora_movimento)
    ) |>
    dplyr::arrange(datahora_movimento)

}

###
ler_processo <- function(dados) {

# cabecalho
item <- dados |>
  purrr::pluck(#"hits", "hits", 1,
    "_source")

# origem
# origem <- dados |>
#   purrr::pluck("hits", "hits", 1, "_index")

id <- purrr::pluck(item, "id")

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

cli::cli_alert_success(glue::glue("Processo {id} lido com sucesso!"))
return(processo)
}

#####
conteudo |>
  purrr::pluck("hits", "hits") |>
  purrr::map(~purrr::pluck(.x, "_source","id"))

x <- conteudo |>
  purrr::pluck("hits", "hits", 103)

id <- purrr::pluck(x, "_source", "id")

movimentos <- purrr::pluck(x, "_source", "movimentos")

movimentos |>
  purrr::map_df(read_movimentos) |>
  dplyr::mutate(id = id) |>
  dplyr::relocate(id)

tibble::tibble(
  id = id,

)
