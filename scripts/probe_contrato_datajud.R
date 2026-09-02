#!/usr/bin/env Rscript

argumento_arquivo <- grep("^--file=", commandArgs(FALSE), value = TRUE)
arquivo_script <- normalizePath(
  sub("^--file=", "", argumento_arquivo[[1]]),
  mustWork = TRUE
)
raiz <- dirname(dirname(arquivo_script))
if (!requireNamespace("datajud", quietly = TRUE)) {
  cli::cli_abort(
    "Instale o pacote datajud antes de executar este probe opcional."
  )
}
argumentos <- commandArgs(trailingOnly = TRUE)
tribunal <- if (length(argumentos) >= 1L) argumentos[[1]] else "TJSP"
cliente <- datajud::datajud_cliente()
endpoint <- datajud::aux_retorna_endpoint(tribunal)
executar_probe <- getFromNamespace("executar_probe_contrato", "datajud")
imprimir_probe <- getFromNamespace("imprimir_probe_contrato", "datajud")
resultado <- executar_probe(endpoint, cliente)
imprimir_probe(resultado)
