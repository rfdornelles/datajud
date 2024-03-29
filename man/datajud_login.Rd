% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/login.R
\name{datajud_login}
\alias{datajud_login}
\title{Configura o email do usuário para identificação no Datajud}
\usage{
datajud_login(email = NULL)
}
\arguments{
\item{email}{Endereço de email para identificação do usuário junto ao CNJ.
Se NULL, uma caixa de diálogo será exibida (no RStudio) para que o usuário
possa fornecer seu email manualmente.}
}
\value{
Não retorna um valor diretamente, mas configura uma variável de ambiente
\code{datajud_email_user} com o email fornecido após validação.
}
\description{
Esta função permite ao usuário configurar um email para identificação junto ao CNJ,
facilitando o acesso e a utilização dos serviços do Datajud. O email fornecido é validado
através de uma expressão regular para garantir que está no formato correto. Uma vez validado,
o email é armazenado como uma variável de ambiente para uso posterior nas requisições ao Datajud.
}
\examples{
# Configurar o email do usuário manualmente
datajud_login(email = "seu.email@dominio.com")

# Solicitar que o usuário forneça o email via caixa de diálogo (RStudio)
datajud_login()
}
