## Funções auxiliares e de alto nível para extrair dados do Datajud, na modalidade de pesquisa por classe.codigo e/ou orgaoJulgador.codigo
## sabemos que o Datajud se baseia em consulta do Elastic e, em tese, qualquer campo pode ser buscado
## como TODO, uma função que permita busca por qualquer parâmetro

## Aqui a função de alto nível é a datajud_pesquisar_classe_orgao, as demais são auxiliares

monta_consulta_elasticsearch <- function(assunto_codigo = NULL,
                                         orgao_codigo = NULL,
                                         size = 1000) {

  # Partes da consulta para assuntos, se fornecidos
  clausula_assuntos <- ""
  if (!is.null(assunto_codigo) && length(assunto_codigo) > 0) {
    partes_assuntos <- purrr::map_chr(assunto_codigo,
                                      ~glue::glue('{{"match": {{"classe.codigo": {.x}}}}}'))
    consulta_should_assuntos <- paste(partes_assuntos, collapse = ", ")
    clausula_assuntos <- glue::glue('"should": [{consulta_should_assuntos}], "minimum_should_match": 1')
  }

  # Partes da consulta para unidades judiciárias, se fornecidas
  clausula_unidades <- ""
  if (!is.null(orgao_codigo) && length(orgao_codigo) > 0) {
    partes_unidades <- purrr::map_chr(orgao_codigo,
                                      ~glue::glue('{{"match": {{"orgaoJulgador.codigo": {.x}}}}}'))
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

  #print(consulta_completa)
  return(consulta_completa)
}


### funcao para requisicao por assunto / codigo
#' Pesquisa processos no Datajud por Classe e/ou Órgão Julgador
#'
#' Esta função realiza uma pesquisa no Datajud, permitindo filtrar processos por código de classe e/ou por código de órgão julgador.
#' É possível especificar um tamanho máximo para a amostra de resultados retornados.
#'
#' @param tribunal Identificador do tribunal a ser consultado.
#' @param classe_codigo Vetor opcional de códigos de classe para filtrar os processos.
#' @param orgao_codigo Vetor opcional de códigos de órgão julgador para filtrar os processos.
#' @param size Tamanho máximo da amostra de resultados a ser retornada, com um valor padrão de 100. O tamanho máximo permitido é 10000.
#'
#' @return A função não retorna um valor diretamente, mas atribui a variável de saída (contendo os resultados da pesquisa) ao ambiente global.
#' A função também emite mensagens de sucesso e informações sobre como verificar os resultados da pesquisa.
#'
#' @export
#'
#' @examples
#' # Pesquisar processos no TJSP por classe de assunto 1116 e tamanho da amostra de 100
#' datajud_pesquisar_classe_orgao(tribunal = "TJSP", classe_codigo = c(1116), size = 100)
#'
#' # Pesquisar processos no TJMG por órgão julgador 13597 com o tamanho padrão da amostra
#' datajud_pesquisar_classe_orgao(tribunal = "TJMG", orgao_codigo = c(13597))
#'
#' # Pesquisar processos no TJRJ por classe de assunto e órgão julgador especificados
#' datajud_pesquisar_classe_orgao(tribunal = "TJRJ", classe_codigo = c(1116), orgao_codigo = c(13597), size = 500)

datajud_pesquisar_classe_orgao <- function(
    tribunal = NA,
    classe_codigo = NULL,
    orgao_codigo = NULL,
    size = 100) {

  if(is.na(tribunal)) stop("Tribunal não informado")

  if(is.null(classe_codigo) & is.null(orgao_codigo)) stop("Nenhum assunto ou unidade informados")

  if(!is.numeric(size)) stop("Tamanho da amostra deve ser um número inteiro")

  if(size < 1 | size > 10000) stop("Tamanho da amostra deve ser um número inteiro entre 1 e 10000")

  # checa se há key definida
  key = get_key()


  # headers
  headers = c(
    'Authorization' = paste0('APIKey ', key),
    'Content-Type' = 'application/json'
  )


  # montar body
  body <- monta_consulta_elasticsearch(
    assunto_codigo = classe_codigo,
    orgao_codigo = orgao_codigo,
    size = round(size)
  )


  # tribunal
  url = aux_retorna_endpoint(tribunal)
  if(is.null(url)) stop("Tribunal não encontrado ou não disponível no Datajud")

  # realizar requisicao
  requisicao <- httr::POST(
    url = url,
    body = body,
    httr::add_headers(headers),
    datajud:::user()
  )

  # extrair conteudo
  conteudo <- requisicao |>
    httr::content() |>
    purrr::pluck("hits", "hits")

  # Verificar o resultado
  if (is.null(conteudo) |
      requisicao$status_code != "200") {

    info_erro <- requisicao |>
      httr::content() |>
      purrr::pluck("error", "root_cause", 1)

    cli::cli_alert_danger("Erro na requisição ou retorno vazio!")
    cli::cli_alert_warning(glue::glue("Status code: {requisicao$status_code}"))
    cli::cli_inform(glue::glue("Tipo de erro: {info_erro$type}"))
    cli::cli_inform(glue::glue("Razão: {info_erro$reason}"))
    cli::cli_inform(glue::glue("Linha: {info_erro$line} | Coluna: {info_erro$col}"))
    return()
  }

  # nomear a variável de saída
  nome_saida <- aux_nomeia_saida()

  cli::cli_alert_success(glue::glue("Variável de saída: {nome_saida}"))
  cli::cli_alert_info("Verifique a resposta da consulta com a função `datajud_ler_processo` ou `datajud_ler_movimentacoes`")


  assign(x = nome_saida,
         value = conteudo,
         envir = .GlobalEnv)

  invisible(conteudo)
}

