% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/requisicao.R
\name{aux_identifica_tribunal}
\alias{aux_identifica_tribunal}
\title{Identifica o tribunal com base no número CNJ de um processo}
\usage{
aux_identifica_tribunal(cnj)
}
\arguments{
\item{cnj}{Um caractere que representa o número CNJ de um processo.}
}
\value{
Um vetor com duas posições: a sigla do tribunal e a URL do endpoint correspondente.
}
\description{
A função analisa o número CNJ de um processo e retorna a sigla do tribunal correspondente,
bem como o endpoint para consulta na API pública do DataJud.
}
\examples{
aux_identifica_tribunal("0000102-03.2004.8.26.0000")

}
