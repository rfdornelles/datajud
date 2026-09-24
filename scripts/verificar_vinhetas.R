# Rscript scripts/verificar_vinhetas.R
# Instala a versão de trabalho em biblioteca temporária e renderiza em outra
# sessão R, sem objetos de desenvolvimento e com transporte de rede bloqueado.
raiz <- normalizePath(".")
temporario <- tempfile("datajud-vinhetas-", tmpdir = "/tmp")
dir.create(temporario)
biblioteca <- file.path(temporario, "library")
dir.create(biblioteca)
status <- system2(file.path(R.home("bin"), "R"),
  c("CMD", "INSTALL", "--no-docs", "--no-help", "--no-html",
    paste0("--library=", shQuote(biblioteca)), shQuote(raiz)))
stopifnot(status == 0L)
script <- file.path(temporario, "renderizar.R")
writeLines(c(
  "args <- commandArgs(TRUE)",
  ".libPaths(c(args[1], .libPaths()))",
  "library(datajud)",
  "local({",
  "  bloquear <- function(...) stop('Rede proibida na construção das vinhetas')",
  "  testthat::local_mocked_bindings(req_perform = bloquear, .package = 'httr2')",
  "  testthat::local_mocked_bindings(download.file = bloquear, .package = 'utils')",
  "  arquivos <- list.files(args[2], pattern = '[.]Rmd$', full.names = TRUE)",
  "  for (arquivo in arquivos) {",
  "    rmarkdown::render(arquivo, output_dir = args[2], quiet = TRUE, envir = new.env())",
  "  }",
  "  paginas <- list.files(args[2], pattern = '[.]html$', full.names = TRUE)",
  "  stopifnot(length(paginas) == length(arquivos))",
  "  for (pagina in paginas) {",
  "    doc <- xml2::read_html(pagina)",
  "    links <- xml2::xml_attr(xml2::xml_find_all(doc, '//a[@href]'), 'href')",
  "    locais <- links[grepl('^[a-z-]+[.]html$', links)]",
  "    stopifnot(all(file.exists(file.path(args[2], locais))))",
  "  }",
  "})",
  "cat('Vinhetas renderizadas sem rede e links internos verificados.\\n')"
), script)
fontes <- list.files("vignettes", pattern = "[.]Rmd$", full.names = TRUE)
stopifnot(all(file.copy(fontes, temporario)))
status <- system2(file.path(R.home("bin"), "Rscript"),
  c("--vanilla", shQuote(script), shQuote(biblioteca), shQuote(temporario)))
stopifnot(status == 0L)
cat("Artefatos para inspeção:", temporario, "\n")
