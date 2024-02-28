## Funções auxiliares e também de alto nível para buscar processos no Datajud
## as auxiliares ajudam a identificar o endpoint, a identificar o tribunal pelo código CNJ e
## o motor de consulta propriamente dito

# A função de alto nível é a datajud_consultar_processo

## auxiliar para match de tribunal com o endpoint

#' Retorna o endpoint correspondente ao tribunal informado
#'
#' Esta função auxiliar é destinada a identificar e retornar a URL do endpoint
#' específico de um tribunal para consulta na API pública do DataJud.
#'
#' @param tribunal Um caractere que representa o código ou sigla do tribunal.
#'
#' @return Caractere com a URL do endpoint correspondente ao tribunal informado.
#'
#' @examples
#' aux_retorna_endpoint("TST")
#' aux_retorna_endpoint("trf01")
#'
#' @export
aux_retorna_endpoint <- function(tribunal) {

    # Limpar o nome do tribunal
    tribunal_limpo <- stringr::str_remove_all(tribunal, "[^a-zA-Z0-9]")  |>
      stringr::str_to_lower()

    # Normalizar siglas que começam com trt ou trf
    if (stringr::str_detect(tribunal_limpo, "^(trf|trt)")) {

      # Extrai os dois dígitos seguintes, ajustando se começar com 0
      digitos <- stringr::str_extract(tribunal_limpo, "[0-9]{1,2}") |>
        as.numeric()

      # Reconstruir a sigla normalizada
      tribunal_limpo <- paste0(stringr::str_extract(tribunal_limpo, "^(trf|trt)"), digitos)
    }

    # Buscar o endpoint na tabela
    url_tribunal <- datajud::tribunais |>
      dplyr::filter(stringr::str_to_lower(sigla) == tribunal_limpo) |>
      dplyr::pull(url) |>
      unique()

    if (length(url_tribunal) == 0) {
      cli::cli_abort("Tribunal não encontrado ou não disponível no Datajud")
    } else if (length(url_tribunal) > 1) {
      cli::cli_abort("Múltiplas URLs encontradas para a sigla fornecida")
    }

    return(url_tribunal[1])
}


## auxiliar para identificar tribunal pelo CNJ

# conforme norma: https://atos.cnj.jus.br/files/compilado23285720221017634de539229ab.pdf
# RESOLUÇÃO Nº 65, DE 16 DE DEZEMBRO DE 2008

#' Identifica o tribunal com base no número CNJ de um processo
#'
#' A função analisa o número CNJ de um processo e retorna a sigla do tribunal correspondente,
#' bem como o endpoint para consulta na API pública do DataJud.
#'
#' @param cnj Um caractere que representa o número CNJ de um processo.
#'
#' @return Um vetor com duas posições: a sigla do tribunal e a URL do endpoint correspondente.
#'
#' @examples
#' aux_identifica_tribunal("0000102-03.2004.8.26.0000")
#'
#' @export

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
    glue::glue("Consultando {length(processo)} processo(s) no Datajud!")
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
