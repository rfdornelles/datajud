## auxiliar para match de tribunal com o endpoint

#' Title
#'
#' @param tribunal
#'
#' @return
#'
#'
#' @examples

aux_retorna_endpoint <- function(tribunal) {

  # limpar o nome do tribunal
  tribunal_limpo <- stringr::str_remove_all(tribunal,"[^a-zA-Z0-9]") |>
    stringr::str_to_lower()

  # endpoint
  url_tribunal <- switch(
    tribunal_limpo,
    "tst" = "https://api-publica.datajud.cnj.jus.br/api_publica_tst/_search",
    "tse" = "https://api-publica.datajud.cnj.jus.br/api_publica_tse/_search",
    "stj" = "https://api-publica.datajud.cnj.jus.br/api_publica_stj/_search",
    "stm" = "https://api-publica.datajud.cnj.jus.br/api_publica_stm/_search",
    "trf1" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf1/_search",
    "trf2" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf2/_search",
    "trf3" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf3/_search",
    "trf4" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf4/_search",
    "trf5" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf5/_search",
    "trf6" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf6/_search",
    "tjac" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjac/_search",
    "tjal" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjal/_search",
    "tjam" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjam/_search",
    "tjap" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjap/_search",
    "tjba" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjba/_search",
    "tjce" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjce/_search",
    "tjdft" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search",
    "tjgo" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjgo/_search",
    "tjma" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjma/_search",
    "tjmg" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmg/_search",
    "tjms" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjms/_search",
    "tjmt" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmt/_search",
    "tjpa" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjpa/_search",
    "tjpb" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjpb/_search",
    "tjpe" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjpe/_search",
    "tjpi" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjpi/_search",
    "tjpr" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjpr/_search",
    "tjro" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjro/_search",
    "tjrr" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjrr/_search",
    "tjrs" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjrs/_search",
    "tjsc" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjsc/_search",
    "tjsp" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjsp/_search",
    "tjto" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjto/_search",
    "trt1" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt1/_search",
    "trt2" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt2/_search",
    "trt3" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt3/_search",
    "trt4" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt4/_search",
    "trt5" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt5/_search",
    "trt6" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt6/_search",
    "trt7" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt7/_search",
    "trt8" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt8/_search",
    "trt9" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt9/_search",
    "trt10" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt10/_search",
    "trt11" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt11/_search",
    "trt12" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt12/_search",
    "trt13" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt13/_search",
    "trt14" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt14/_search",
    "trt15" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt15/_search",
    "trt16" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt16/_search",
    "trt17" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt17/_search",
    "trt18" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt18/_search",
    "trt19" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt19/_search",
    "trt20" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt20/_search",
    "trt21" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt21/_search",
    "trt22" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt22/_search",
    "trt23" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt23/_search",
    "trt24" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt24/_search",
    "treac" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ac/_search",
    "treal" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-al/_search",
    "tream" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-am/_search",
    "treap" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ap/_search",
    "treba" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ba/_search",
    "trece" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ce/_search",
    "tredft" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-dft/_search",
    "trees" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-es/_search",
    "trego" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-go/_search",
    "trema" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ma/_search",
    "tremg" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-mg/_search",
    "trems" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ms/_search",
    "tremt" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-mt/_search",
    "trepa" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-pa/_search",
    "trepb" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-pb/_search",
    "trepe" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-pe/_search",
    "trepi" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-pi/_search",
    "trepr" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-pr/_search",
    "trerj" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-rj/_search",
    "trern" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-rn/_search",
    "trero" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-ro/_search",
    "trerr" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-rr/_search",
    "trers" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-rs/_search",
    "tresc" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-sc/_search",
    "trese" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-se/_search",
    "tresp" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-sp/_search",
    "treto" = "https://api-publica.datajud.cnj.jus.br/api_publica_tre-to/_search",
    "tjmmg" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmmg/_search",
    "tjmrs" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmrs/_search",
    "tjmsp" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmsp/_search",
    NULL
  )
  return(url_tribunal)
}


## requisicoes

#' Title
#'
#' @param tribunal
#'
#' @return
#' @export
#'
#' @examples

datajud_requisition <- function(tribunal, processo, sleep = 0.1) {

  url_tribunal <- aux_retorna_endpoint(tribunal)

  if(is.null(url_tribunal)) {
    stop("Tribunal não encontrado")
    return(FALSE)
  }

  # checa o numero do processo
  numero_cnj_limpo <- gsub("[^0-9]", "", processo)
  if(nchar(numero_cnj_limpo) != 20) {
    stop("Número do processo inválido")
    return(FALSE)
  }

  # checa se há key definida
  key = get_key()

  # headers
  headers = c(
    'Authorization' = paste0('APIKey ', key),
    'Content-Type' = 'application/json'
  )

  # body
  body = glue::glue(
    '{{
      "size": 10000,
      "query": {{
        "match": {{
          "numeroProcesso": "{numero_cnj_limpo}"
        }}
      }}
    }}'
  )

  # requisicao
  requisicao <- httr::POST(
    url = url_tribunal,
    body = body,
    httr::add_headers(headers),
    user()
  )

  if (requisicao$status_code != 200) {
    cat(glue::glue("Erro na requisição: {requisicao$status_code}\n
                   Processo: {numero_cnj_limpo}\n
                   Tribunal: {tribunal}\n"))

    stop("Erro na requisição")
    return(FALSE)
  }

  resposta <- httr::content(requisicao)
  # return(resposta)
  cnj_localizado = purrr::pluck(resposta,
                                "hits",
                                "hits",
                                1,
                                "_source",
                                "numeroProcesso")
  print(cnj_localizado)
  if(is.null(cnj_localizado)) {
    stop(glue::glue("Processo {processo} não encontrado no tribunal {tribunal}"))
    return(NULL)
  }

  if (cnj_localizado != numero_cnj_limpo) {
    cat(glue::glue("Processo {processo} NAO encontrado no tribunal {tribunal}\n"))
    return(NULL)
  }

  cat(glue::glue("Sucesso!! Processo {processo} encontrado no tribunal {tribunal}"))
  Sys.sleep(sleep)
  invisible(resposta)
}
