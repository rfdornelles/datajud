# .onLoad <- function(libname, pkgname) {
#
#   if(Sys.getenv("billing_project_id") == "") {
#
#     Sys.setenv(billing_project_set = FALSE)
#
#   } else {
#
#     Sys.setenv(billing_project_set = "user_has_set")
#
#   }
#
# }

.onLoad <- function(libname, pkgname) {
  packageStartupMessage(
    cli::cli({
      cli::cli_h1("Pacote Datajud for R")
      cli::cli_alert_info("Pacote não oficial para acessar a API Pública do Datajud - CNJ")
      cli::cli_end()
      cli::cli_h1("Termos de uso")
      cli::cli_alert("A utilização desse pacote implica na aceitação dos termos de uso do Conselho Nacional de Justiça e os detalhes do pacote. Dentre os quais incluem:")
      cli::cli_alert_success("1.2 O usuário manifestará tacitamente sua aceitação às condições deste termo ao utilizar a interface.")
      cli::cli_alert_success("3.2 O usuário da API pública se responsabiliza pelo uso da interface e das informações, pesquisas, documentos ou qualquer espécie de informação derivada de seu uso.")
      cli::cli_alert_success("3.3 A API é fornecida exclusivamente para fins legais, não comerciais e autorizados, sendo seu uso indevido, abusivo, ilegal, malicioso ou imoral estritamente proibido.")
      cli::cli_alert_success("3.6 O CNJ não garante a precisão, integridade ou atualidade dos dados fornecidos pela API.")
      cli::cli_alert_success("3.8 O usuário concorda em não modificar, distribuir, vender ou explorar comercialmente a API ou qualquer informação derivada dela.")
      cli::cli_alert_success("3.9 O usuário concorda em dar ciência ao CNJ de qualquer informação, notícia, estudo, relatório ou documento de qualquer natureza que seja disponibilizado ao público em geral.")
      cli::cli_end()
      cli::cli("")
      cli::cli_alert_info("Para mais informações, acesse os termos de uso completos em: https://datajud-wiki.cnj.jus.br/api-publica/termo-uso")
      cli::cli_end()
      cli::cli_h1("Utilização")
      cli::cli_alert_info("Identifique-se com seu email usando:")
      cli::cli_bullets("setar_email_identificacao(<seu email>)")
      cli::cli_end()
    })
  )
}

