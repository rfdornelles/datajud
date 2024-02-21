
## função para checar se a identificação foi feita
checar_identificacao_valida <- function() {

  # puxa do environment o email e a key
  email_identificado <- Sys.getenv("datajud_email_user")

  # Padrão de expressão regular para validar email
  email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

  # checa se os valores estão definidos
  if (!grepl(email_pattern, email_identificado)) {
    return(FALSE)

  } else {
    return(email_identificado)
  }

}

#' Title
#'
#' @param tribunal
#'
#' @export
#'
#' @examples

## função para setar usuario

datajud_login <- function(email = NULL) {

  if(is.null(email)) {

    email <- rstudioapi::showPrompt(
      title = "Identificação",
      message = "Por favor, forneça um email para identificação junto ao CNJ:",
      default = ""
    )


  }
  # Padrão de expressão regular para validar email
  email_pattern <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"

  # Verificar se é um email válido
  if(grepl(email_pattern, email)) {
    # Se for, setar environment
    Sys.setenv(datajud_email_user = email)
    cat("Email configurado com sucesso:", email, "\n")
  } else {
    stop("Email inválido. Por favor, forneça um email válido.\n")
    return(FALSE)
  }

}

## função auxiliar para gerar user_agent

user <- function() {

  email_user <- checar_identificacao_valida()

  if(email_user == FALSE) {
    return(email_user)
  }

  return(
    httr::user_agent(paste("Pacote {datajud} para R - email:", email_user))
  )

}

## função para obter key do site do CNJ

obter_key_cnj <- function() {

  # checar se usuario foi identificado
  email_user <- checar_identificacao_valida()

  if(email_user == FALSE) {

    stop("Usuário não identificado.\nPor favor, identifique-se com um email válido usando datajud_login(seu_email).")
    return(FALSE)
  }

  # url do site do CNJ
  url <- "https://datajud-wiki.cnj.jus.br/api-publica/acesso/"

  # puxa a key do site
  key <- httr::GET(url,
                   user()) |>
    httr::content() |>
    xml2::xml_find_all("//strong") |>
    xml2::xml_text() |>
    purrr::pluck(3)

  # seta a key no environment
  Sys.setenv(datajud_key = key)

  cat("Key configurada com sucesso:", key, "\n")
  invisible(key)
}

## resgatar a key da memoria

get_key <- function() {

  key <- Sys.getenv("datajud_key")

  if(is.null(key) | key == "") {

    email_user <- checar_identificacao_valida()

    if(email_user == FALSE) {
      stop("Usuário não identificado.\nPor favor, identifique-se com um email válido usando datajud_login(seu_email).")
      return(FALSE)
    }

    key <- obter_key_cnj()

  }

  return(key)
}
