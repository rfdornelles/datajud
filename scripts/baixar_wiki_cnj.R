#!/usr/bin/env Rscript

argumento_arquivo <- grep("^--file=", commandArgs(FALSE), value = TRUE)
if (length(argumento_arquivo) != 1L) {
  cli::cli_abort(
    "Execute este arquivo com `Rscript scripts/baixar_wiki_cnj.R`."
  )
}
arquivo_script <- normalizePath(
  sub("^--file=", "", argumento_arquivo[[1]]),
  mustWork = TRUE
)
raiz <- dirname(dirname(arquivo_script))
source(file.path(raiz, "R", "wiki_cache.R"))

argumentos <- commandArgs(trailingOnly = TRUE)
sitemap_url <- if (length(argumentos) >= 1L) {
  argumentos[[1]]
} else {
  "https://datajud-wiki.cnj.jus.br/sitemap.xml"
}
diretorio <- if (length(argumentos) >= 2L) {
  argumentos[[2]]
} else {
  file.path(raiz, ".cache", "datajud-wiki")
}
manifesto_versionado <- if (length(argumentos) >= 3L) {
  argumentos[[3]]
} else {
  file.path(raiz, "inst", "extdata", "datajud-wiki-manifest.csv")
}

manifesto <- baixar_wiki_cnj(
  sitemap_url = sitemap_url,
  diretorio = diretorio,
  manifesto_versionado = manifesto_versionado
)
falhas <- sum(manifesto$resultado != "ok")
cli::cli_inform(c(
  "v" = "Cache concluído: {nrow(manifesto)} recursos registrados.",
  "i" = "Arquivos locais: {diretorio}",
  "i" = "Manifesto versionável: {manifesto_versionado}"
))
if (falhas > 0L) {
  cli::cli_abort(
    "{falhas} recurso(s) não puderam ser armazenados; consulte o manifesto.",
    class = "datajud_erro_cache_wiki"
  )
}
