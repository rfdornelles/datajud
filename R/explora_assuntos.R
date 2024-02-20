monta_consulta_elasticsearch <- function(codigos_assunto = NULL,
                                         unidades_judiciarias = NULL,
                                         size = 1000) {

  # Partes da consulta para assuntos, se fornecidos
  clausula_assuntos <- ""
  if (!is.null(codigos_assunto) && length(codigos_assunto) > 0) {
    partes_assuntos <- purrr::map_chr(codigos_assunto,
                                      ~glue::glue('{{"match": {{"classe.codigo": "{.x}"}}}}'))
    consulta_should_assuntos <- paste(partes_assuntos, collapse = ", ")
    clausula_assuntos <- glue::glue('"should": [{consulta_should_assuntos}], "minimum_should_match": 1')
  }

  # Partes da consulta para unidades judiciárias, se fornecidas
  clausula_unidades <- ""
  if (!is.null(unidades_judiciarias) && length(unidades_judiciarias) > 0) {
    partes_unidades <- purrr::map_chr(unidades_judiciarias,
                                      ~glue::glue('{{"match": {{"orgaoJulgador.codigo": "{.x}"}}}}'))
    consulta_filter_unidades <- paste(partes_unidades, collapse = ", ")
    clausula_unidades <- glue::glue('"filter": [{{"bool": {{"should": [{consulta_filter_unidades}]}}}}]')
  }

  # Combina as cláusulas na consulta, considerando a presença ou ausência delas
  clausulas <- c(clausula_assuntos, clausula_unidades) |>
    purrr::discard(~.x == "") |>
    paste(collapse = ", ")

  # Monta a consulta completa
  consulta_completa <- glue::glue('
    {{
      "size": {size},
      "query": {{
        "bool": {{
          {clausulas}
        }}
      }}
    }}
  ')

  return(consulta_completa)
}



## buscar assuntos

# httr::GET("https://gateway.cloud.pje.jus.br/tpu/api/v1/publico/consulta/assuntos?nome=consumidor") |> httr::content() -> b
# b |> purrr::map_df(dplyr::bind_rows)

# https://datajud-wiki.cnj.jus.br/api-publica/exemplos/exemplo2


### funcao para requisicao por assunto / codigo
datajud_pesquisar_assunto_orgao <- function(
    tribunal,
    lista_assuntos = NULL,
    lista_unidades = NULL,
    size = 1000) {

  # checa se há key definida
  key = get_key()

  # headers
  headers = c(
    'Authorization' = paste0('APIKey ', key),
    'Content-Type' = 'application/json'
  )


  # montar body
  body <- monta_consulta_elasticsearch(
    codigos_assunto = lista_assuntos,
    unidades_judiciarias = lista_unidades,
    size = size
  )


  # tribunal
  url = aux_retorna_endpoint(tribunal)

  # realizar requisicao
  requisicao <- httr::POST(
    url = url,
    body = body,
    httr::add_headers(headers),
    datajud:::user()
  )

  # extrair conteudo
  conteudo <- requisicao |>
    httr::content()

  # extrair metadados
  processos <- conteudo |>
    purrr::pluck("hits", "hits") |>
    purrr::map_df(purrr::possibly(ler_processo, quiet = FALSE),
                  .progress = TRUE)

  return(processos)
}

