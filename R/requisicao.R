## Funções auxiliares e também de alto nível para buscar processos no Datajud
## as auxiliares ajudam a identificar o endpoint, a identificar o tribunal pelo código CNJ e
## o motor de consulta propriamente dito

# A função de alto nível é a datajud_consultar_processo

## auxiliar para match de tribunal com o endpoint

#' Retorna o endpoint correspondente ao tribunal informado
#'
#' Esta função auxiliar é destinada a identificar e retornar a URL do endpoint
#' específico de um tribunal para consulta na API pública do Datajud.
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

    # Buscar o endpoint com a mesma normalização aplicada aos dois lados.
    siglas_normalizadas <- datajud::tribunais$sigla |>
      stringr::str_remove_all("[^a-zA-Z0-9]") |>
      stringr::str_to_lower()
    indice <- siglas_normalizadas == tribunal_limpo
    url_tribunal <- unique(datajud::tribunais$url[indice])

    if (length(url_tribunal) == 0) {
      cli::cli_abort("Tribunal n\u00E3o encontrado ou n\u00E3o dispon\u00EDvel no Datajud")
    } else if (length(url_tribunal) > 1) {
      cli::cli_abort("M\u00FAltiplas URLs encontradas para a sigla fornecida")
    }

    return(url_tribunal[1])
}


## auxiliar para identificar tribunal pelo CNJ

# conforme norma: https://atos.cnj.jus.br/files/compilado23285720221017634de539229ab.pdf
# RESOLUÇÃO Nº 65, DE 16 DE DEZEMBRO DE 2008

#' Identifica o tribunal com base no número CNJ de um processo
#'
#' A função analisa o número CNJ de um processo e retorna a sigla do tribunal correspondente,
#' bem como o endpoint para consulta na API pública do Datajud.
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

  if (length(cnj) != 1L || is.na(cnj)) stop("N\u00FAmero do processo inv\u00E1lido")
  cnj_limpo <- normalizar_numero_cnj(cnj)

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

  siglas_uf <- c(
    "01" = "ac", "02" = "al", "03" = "ap", "04" = "am", "05" = "ba",
    "06" = "ce", "07" = "dft", "08" = "es", "09" = "go", "10" = "ma",
    "11" = "mt", "12" = "ms", "13" = "mg", "14" = "pa", "15" = "pb",
    "16" = "pr", "17" = "pe", "18" = "pi", "19" = "rj", "20" = "rn",
    "21" = "rs", "22" = "ro", "23" = "rr", "24" = "sc", "25" = "se",
    "26" = "sp", "27" = "to"
  )

  # separar o tribunal
  segmento_regional <- dplyr::case_when(
    campo_tr == "00" ~ "originario",
    segmento_justica %in% c("trf",
                            "tst",
                            "jm",
                            "trt") ~ campo_tr,
    # Na Justiça estadual, eleitoral e militar estadual, usa a sigla da UF.
    TRUE ~ unname(siglas_uf[campo_tr])
  )

  # distribuir o endpoint de acordo com o tribunal

  if (segmento_regional == "originario") {

    resposta <-segmento_justica

  } else {

    resposta <- paste0(segmento_justica, segmento_regional)

  }

  return(c(resposta, aux_retorna_endpoint(resposta)))

}

normalizar_numero_cnj <- function(processo) {
  if (length(processo) != 1L || is.na(processo) || !nzchar(trimws(processo))) {
    stop("N\u00FAmero do processo inv\u00E1lido")
  }
  limpo <- gsub("[^0-9]", "", processo)
  if (nchar(limpo) != 20L) stop("N\u00FAmero do processo inv\u00E1lido")
  limpo
}

datajud_requisition <- function(processo, cliente, tribunal = NA, sleep = 0.1) {

  validar_cliente(cliente)

  if(is.na(tribunal)) {

    aux_identifica <- aux_identifica_tribunal(processo)
    tribunal <- aux_identifica[1]
    url_tribunal <- aux_identifica[2]

  } else {

  url_tribunal <- aux_retorna_endpoint(tribunal)

  }

  if(is.na(url_tribunal)) {
    cli::cli_alert_danger(glue::glue("Tribunal {tribunal} n\u00E3o encontrado ou n\u00E3o dispon\u00EDvel no Datajud"))
    return(NULL)
  }

  # checa o numero do processo
  numero_cnj_limpo <- normalizar_numero_cnj(processo)

  query <- list(
    query = list(
      match = list(numeroProcesso = numero_cnj_limpo)
    )
  )

  resposta <- requisitar_api_datajud(cliente, url_tribunal, query) |>
    purrr::pluck("hits", "hits", 1, .default = NULL)
  # return(resposta)
  cnj_localizado <- purrr::pluck(
    resposta, "_source", "numeroProcesso", .default = NULL
  )

  if(is.null(cnj_localizado)) {
    stop(glue::glue("Processo {processo} n\u00E3o encontrado no tribunal {tribunal}"))
  }

  if (cnj_localizado != numero_cnj_limpo) {
    cat(glue::glue("Processo {processo} NAO encontrado no tribunal {tribunal}\n"))
    return(NULL)
  }

  cli::cli_alert_success(glue::glue("Sucesso!! Processo {processo} encontrado no tribunal {tribunal}"))

  return(resposta)
}

## pesquisar processos

#' Consulta processos judiciais no Datajud
#'
#' Esta função realiza consultas de processos judiciais no Datajud, permitindo aos usuários
#' buscar informações detalhadas por número de processo e tribunal específico. A função
#' também suporta um intervalo de espera (`sleep`) entre as requisições para evitar sobrecarga
#' no servidor. É necessário fornecer um cliente criado por `datajud_cliente()`.
#'
#' @param processo Número do processo ou vetor de números dos processos a serem consultados.
#'                 Deve ser fornecido como um valor ou vetor de caracteres.
#' @param cliente Objeto criado por `datajud_cliente()`.
#' @param tribunal Identificador do tribunal correspondente ao(s) processo(s) sendo consultado(s).
#'                 Pode ser um valor único, um valor por processo ou `NA`
#'                 escalar para inferir todos automaticamente. Não é permitido
#'                 misturar tribunais informados e `NA` no mesmo vetor.
#' @param sleep Tempo de espera (em segundos) entre as requisições, para evitar sobrecarga
#'              no servidor. O valor padrão é 0.1 segundos. Deve estar entre 0 e 60.
#'
#' @return Uma lista com uma resposta por processo. Respostas que falharem podem ser `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Criar o cliente e consultar um processo:
#' cliente <- datajud_cliente(api_key = "sua-chave")
#' datajud_consultar_processo("0000001-89.2020.8.26.0000", cliente, tribunal = "TJSP")
#' # Para consultar múltiplos processos com intervalo de espera customizado:
#' datajud_consultar_processo(processo = c("0000001-89.2020.8.26.0000", "0000002-30.2021.8.26.0000"),
#'                            cliente = cliente,
#'                            tribunal = c("TJSP", "TJSP"),
#'                            sleep = 1)
#' }

datajud_consultar_processo <- function(processo,
                                       cliente,
                                       tribunal = NA,
                                       sleep = 0.1) {

  # checar se processo foi informado
  processo <- as.character(processo)

  if (length(processo) < 1L || anyNA(processo) || any(!nzchar(trimws(processo)))) {
    cli::cli_abort("N\u00FAmero do processo n\u00E3o informado")
  }

  # checar se tribunal é null ou se têm o mesmo tamanho que processo
  if (length(tribunal) == 0L) {
    cli::cli_abort("Tribunal n\u00E3o informado.")
  }
  if (length(tribunal) > 1L && anyNA(tribunal)) {
    cli::cli_abort("Tribunal n\u00E3o pode misturar valores e NA.")
  }
  if (length(tribunal) > 1L && length(tribunal) != length(processo)) {
    cli::cli_abort("O campo Tribunal n\u00E3o tem o mesmo tamanho que o campo processo.")
  }
  if (length(tribunal) == 1L && !is.na(tribunal) && !nzchar(trimws(tribunal))) {
    cli::cli_abort("Tribunal n\u00E3o pode ser vazio.")
  }

  # checar se sleep é válido
  if(!is.numeric(sleep) || length(sleep) != 1L || !is.finite(sleep) || sleep < 0 || sleep > 60) {
    cli::cli_abort("Valor de sleep inv\u00E1lido. Informe n\u00FAmero positivo entre 0 e 60.")
  }

  validar_cliente(cliente)


  # informar que a requisição está sendo feita
  cli::cli_alert_info(
    glue::glue("Consultando {length(processo)} processo(s) no Datajud!")
    )

  # chamada segura da funcao
  safe_requisition <- purrr::possibly(datajud_requisition,
                                      otherwise = NULL,
                                      quiet = FALSE)
  # rodar loop
  tribunais <- if (length(tribunal) == 1L) rep(tribunal, length(processo)) else tribunal
  resposta <- purrr::map2(
    .x = processo,
    .y = tribunais,
    .f = ~ {

      resultado <- safe_requisition(.x, cliente, .y)

      Sys.sleep(sleep)

      return(resultado)
    },

    .progress = TRUE)

  # checar sucesso
  respostas_validas <- sum((resposta |> purrr::map_int(length)) > 0)

  if (respostas_validas == 0) {
    cli::cli_alert_danger("Nenhuma resposta v\u00E1lida encontrada.")
    return(NULL)
  }

  # informar que a requisição foi finalizada
  cli::cli_alert_info(
    glue::glue("Requisi\u00E7\u00E3o finalizada! {respostas_validas}/{length(processo)} processos consultados com sucesso!")
  )

  resposta
}
