## auxiliar para match de tribunal com o endpoint

aux_retorna_endpoint <- function(tribunal) {

  # limpar o nome do tribunal
  tribunal_limpo <- stringr::str_remove_all(tribunal,"[^a-zA-Z0-9]") |>
    stringr::str_to_lower()

  # endpoint
  url_tribunal <- switch(
    tribunal_limpo,
    # superiores
    "tst" = "https://api-publica.datajud.cnj.jus.br/api_publica_tst/_search",
    "tse" = "https://api-publica.datajud.cnj.jus.br/api_publica_tse/_search",
    "stj" = "https://api-publica.datajud.cnj.jus.br/api_publica_stj/_search",
    "stm" = "https://api-publica.datajud.cnj.jus.br/api_publica_stm/_search",

    # federal comum
    "trf01" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf1/_search",
    "trf02" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf2/_search",
    "trf03" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf3/_search",
    "trf04" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf4/_search",
    "trf05" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf5/_search",
    "trf06" = "https://api-publica.datajud.cnj.jus.br/api_publica_trf6/_search",

    # estadual comum
    "tjac" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjac/_search",
    "tjal" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjal/_search",
    "tjam" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjam/_search",
    "tjap" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjap/_search",
    "tjba" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjba/_search",
    "tjce" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjce/_search",
    "tjdft" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjdft/_search",
    "tjes" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjes/_search",
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
    "tjse" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjse/_search",
    "tjsp" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjsp/_search",
    "tjto" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjto/_search",

    # trabalhista
    "trt01" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt1/_search",
    "trt02" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt2/_search",
    "trt03" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt3/_search",
    "trt04" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt4/_search",
    "trt05" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt5/_search",
    "trt06" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt6/_search",
    "trt07" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt7/_search",
    "trt08" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt8/_search",
    "trt09" = "https://api-publica.datajud.cnj.jus.br/api_publica_trt9/_search",
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

    # eleitoral
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

    # militar estadual
    "tjmmg" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmmg/_search",
    "tjmrs" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmrs/_search",
    "tjmsp" = "https://api-publica.datajud.cnj.jus.br/api_publica_tjmsp/_search",

      NULL
  )
  return(url_tribunal)
}

## auxiliar para identificar tribunal pelo CNJ

# conforme norma: https://atos.cnj.jus.br/files/compilado23285720221017634de539229ab.pdf
# RESOLUÇÃO Nº 65, DE 16 DE DEZEMBRO DE 2008

aux_identifica_tribunal <- function(cnj) {

  cnj_limpo <- gsub("[^0-9]", "", cnj)

  # validar se tem 20 dígitos
  if (nchar(cnj_limpo) != 20) {
    stop("Número do processo inválido")
    return(NULL)
  }

  # extrair campo J e campo TR
  campo_j <- substr(cnj_limpo, start = 14, stop = 14)
  campo_tr <- substr(cnj_limpo, start = 15, stop = 16)

  # separar o ramo da justiça
  segmento_justica <- dplyr::case_when(
    campo_j == "1" ~ "stf",
    campo_j == "2" ~ "cnj",
    campo_j == "3" ~ "stj",
    campo_j == "4" ~ "trf",
    campo_j == "5" ~ "trt",
    campo_j == "6" ~ "tre",
    campo_j == "7" ~ "jm",
    campo_j == "8" ~ "tj",
    campo_j == "9" ~ "tjm"
  )

  # separar o tribunal
  segmento_regional <- dplyr::case_when(
    campo_tr == "00" ~ "originario",
    segmento_justica %in% c("trf",
                            "tst",
                            "jm",
                            "trt") ~ campo_tr,
    # se for estadual, eleitoral, militar estadual será a sigla
    # do estado em ordem alfabetica
    TRUE ~ dplyr::case_match(
      campo_tr,
      "01" ~ "ac",
      "02" ~ "al",
      "03" ~ "ap",
      "04" ~ "am",
      "05" ~ "ba",
      "06" ~ "ce",
      "07" ~ "dft",
      "08" ~ "es",
      "09" ~ "go",
      "10" ~ "ma",
      "11" ~ "mt",
      "12" ~ "ms",
      "13" ~ "mg",
      "14" ~ "pa",
      "15" ~ "pb",
      "16" ~ "pr",
      "17" ~ "pe",
      "18" ~ "pi",
      "19" ~ "rj",
      "20" ~ "rn",
      "21" ~ "rs",
      "22" ~ "ro",
      "23" ~ "rr",
      "24" ~ "sc",
      "25" ~ "se",
      "26" ~ "sp",
      "27" ~ "to",
      .default = NA_character_
    )
  )

  # distribuir o endpoint de acordo com o tribunal

  if (segmento_regional == "originario") {

    resposta <-segmento_justica

  } else {

    resposta <- paste0(segmento_justica, segmento_regional)

  }

  return(c(resposta, aux_retorna_endpoint(resposta)))

}

datajud_requisition <- function(processo, tribunal = NA, sleep = 0.1) {

  if(is.na(tribunal)) {

    aux_identifica <- aux_identifica_tribunal(processo)
    tribunal <- aux_identifica[1]
    url_tribunal <- aux_identifica[2]

  } else {

  url_tribunal <- aux_retorna_endpoint(tribunal)

  }

  if(is.na(url_tribunal)) {
    cli::cli_alert_danger(glue::glue("Tribunal {tribunal} não encontrado ou não disponível no Datajud"))
    return(NULL)
  }

  # checa o numero do processo
  numero_cnj_limpo <- gsub("[^0-9]", "", processo)
  if(nchar(numero_cnj_limpo) != 20) {
    stop("Número do processo inválido")
    return(NULL)
  }

  # checa se há key definida
  key = datajud:::get_key()

  # headers
  headers = c(
    'Authorization' = paste0('APIKey ', key),
    'Content-Type' = 'application/json'
  )

  # body
  body = glue::glue(
    '{{
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
    return(NULL)
  }

  resposta <- httr::content(requisicao) |>
    purrr::pluck("hits", "hits", 1)
  # return(resposta)
  cnj_localizado = purrr::pluck(resposta,
                                # "hits",
                                # "hits",
                                # 1,
                                "_source",
                                "numeroProcesso")

  if(is.null(cnj_localizado)) {
    stop(glue::glue("Processo {processo} não encontrado no tribunal {tribunal}"))
    return(NULL)
  }

  if (cnj_localizado != numero_cnj_limpo) {
    cat(glue::glue("Processo {processo} NAO encontrado no tribunal {tribunal}\n"))
    return(NULL)
  }

  cli::cli_alert_success(glue::glue("Sucesso!! Processo {processo} encontrado no tribunal {tribunal}"))

  return(resposta)
}

## auxiliar para identificar variaveis "livres"

aux_nomeia_saida <- function(nome_inicial = "datajud_resposta") {

  nome <- nome_inicial

  # loop para verificar se existe no environment global
  i <- 1
  while(exists(nome, envir = .GlobalEnv) & i <= 20) {
    nome <- paste0("datajud_resposta_", i)
    i <- i + 1
  }

  # se já existir 20 variáveis com o mesmo nome, sobrescreve e avisa
  if(i > 20) {
    nome <- nome_inicial
    cli::cli_alert_info(glue::glue(
      "Já existem +20 variáveis com o nome {nome}, sobrescrevendo a primeira."
      )
    )
  }

  return(nome)
}
## pesquisar processos

#' Consulta processos judiciais no Datajud
#'
#' Esta função realiza consultas de processos judiciais no Datajud, permitindo aos usuários
#' buscar informações detalhadas por número de processo e tribunal específico. A função
#' também suporta um intervalo de espera (`sleep`) entre as requisições para evitar sobrecarga
#' no servidor. É necessário realizar identificação prévia através de `datajud_login` antes
#' de executar consultas.
#'
#' @param processo Número do processo ou vetor de números dos processos a serem consultados.
#'                 Deve ser fornecido como um valor ou vetor de caracteres.
#' @param tribunal Identificador do tribunal correspondente ao(s) processo(s) sendo consultado(s).
#'                 Se fornecido, deve ter o mesmo tamanho que o vetor `processo`.
#' @param sleep Tempo de espera (em segundos) entre as requisições, para evitar sobrecarga
#'              no servidor. O valor padrão é 0.1 segundos. Deve ser um número positivo.
#'
#' @return A função não retorna um valor diretamente ao ambiente de chamada, mas armazena
#'         os resultados da consulta em uma variável nomeada no ambiente global. Esta variável
#'         contém os detalhes dos processos consultados e pode ser acessada diretamente ou
#'         através de funções específicas como `datajud_ler_processo` ou `datajud_ler_movimentacoes`.
#'
#' @export
#'
#' @examples
#' # Após realizar o login com datajud_login():
#' datajud_consultar_processo(processo = "0000001-89.2020.8.26.0000", tribunal = "TJSP")
#' # Para consultar múltiplos processos com intervalo de espera customizado:
#' datajud_consultar_processo(processo = c("0000001-89.2020.8.26.0000", "0000002-30.2021.8.26.0000"),
#'                            tribunal = c("TJSP", "TJSP"),
#'                            sleep = 1)

datajud_consultar_processo <- function(processo,
                                       tribunal = NA,
                                       sleep = 0.1) {

  # checar se processo foi informado
  processo <- as.character(processo)

  if (length(processo) < 1 | any(processo == "")) {
    cli::cli_alert_danger("Número do processo não informado")
    return()
  }

  # checar se tribunal é null ou se têm o mesmo tamanho que processo
  if(!is.na(tribunal) & length(tribunal) != length(processo)) {
    cli::cli_alert_danger("O campo Tribunal não tem o mesmo tamanho que o campo processo.")
    cli::cli_alert_info("Informe listas do mesmo tamanho ou deixe o campo tribunal em branco.")
    return()
  }

  # checar se sleep é válido
  if(!is.numeric(sleep) | sleep < 0 | sleep > 10000) {
    cli::cli_alert_danger("Valor de sleep inválido. Informe número positivo inferior a 10.000.")
    return()
  }

  # checar se o login foi realizado
  if(datajud:::checar_identificacao_valida() == FALSE) {
    cli::cli_alert_danger("Você precisa se identificar para realizar a consulta.")
    cli::cli_alert_info("Use datajud::datajud_login()")
    return()
  }


  # informar que a requisição está sendo feita
  cli::cli_alert_info(
    glue::glue("Consultando processo {length(processo)} no Datajud!")
    )

  # chamada segura da funcao
  safe_requisition <- purrr::possibly(datajud_requisition,
                                      otherwise = NULL,
                                      quiet = FALSE)
  # rodar loop
  resposta <- purrr::map2(
    .x = processo,
    .y = tribunal,
    .f = ~ {

      resultado <- safe_requisition(.x, .y)

      Sys.sleep(sleep)

      return(resultado)
    },

    .progress = TRUE)

  # checar sucesso
  respostas_validas <- sum((resposta |> purrr::map_int(length)) > 0)

  if (respostas_validas == 0) {
    cli::cli_alert_danger("Nenhuma resposta válida encontrada.")
    return(NULL)
  }

  # informar que a requisição foi finalizada
  cli::cli_alert_info(
    glue::glue("Requisição finalizada! {respostas_validas}/{length(processo)} processos consultados com sucesso!")
  )

  # nomear a variável de saída
  nome_saida <- aux_nomeia_saida()

  cli::cli_alert_success(glue::glue("Variável de saída: {nome_saida}"))
  cli::cli_alert_info("Verifique a resposta da consulta com a função `datajud_ler_processo` ou `datajud_ler_movimentacoes`")


  assign(x = nome_saida,
         value = resposta,
         envir = .GlobalEnv)

  invisible(resposta)
}
