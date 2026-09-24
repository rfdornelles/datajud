# Execute na raiz do repositório:
# Rscript data-raw/tpu.R --atualizar  (descobre a versão e baixa as fontes)
# Rscript data-raw/tpu.R             (reproduz o manifesto; usa cache ou baixa)
# Rscript data-raw/tpu.R --offline   (reproduz exclusivamente do cache)
source("R/tpu.R")
args <- commandArgs(trailingOnly = TRUE)
stopifnot(all(args %in% c("--atualizar", "--offline")))
if (all(c("--atualizar", "--offline") %in% args)) stop("Opções incompatíveis.")
cache <- ".cache/tpu"
dir.create(cache, recursive = TRUE, showWarnings = FALSE)
manifesto_path <- "inst/extdata/tpu-fontes.csv"
baixar <- function(url, caminho) {
  temporario <- tempfile(tmpdir = dirname(caminho))
  on.exit(unlink(temporario))
  httr2::request(url) |>
    httr2::req_user_agent("datajud-tpu/1.0") |>
    httr2::req_timeout(120) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform(path = temporario)
  if (!file.copy(temporario, caminho, overwrite = TRUE)) stop("Falha ao gravar fonte.")
}
if ("--atualizar" %in% args) {
  catalogos <- lapply(c("A", "C"), function(tipo) {
    url <- paste0("https://www.cnj.jus.br/sgt/versoes.php?tipo_tabela=", tipo)
    caminho <- file.path(cache, paste0("versoes-", tipo, ".html"))
    baixar(url, caminho)
    x <- dplyr::bind_rows(tpu_descobrir(caminho, tipo), tpu_descobrir_sql(caminho, tipo))
    if (length(unique(x$versao)) != 1L) stop("Versões XLS e SQL divergentes.")
    x$catalogo_url <- url
    x$catalogo_md5 <- unname(tools::md5sum(caminho))
    x
  })
  manifesto <- dplyr::bind_rows(catalogos)
  manifesto$baixado_em_utc <- NA_character_
  manifesto$md5 <- NA_character_
} else {
  manifesto <- utils::read.csv(manifesto_path, stringsAsFactors = FALSE, encoding = "UTF-8")
  manifesto$versao <- as.Date(manifesto$versao)
}
manifesto <- as.data.frame(manifesto)
fontes <- vector("list", nrow(manifesto))
for (i in seq_len(nrow(manifesto))) {
  caminho <- file.path(cache, manifesto$arquivo[i])
  if ("--atualizar" %in% args || !file.exists(caminho)) {
    if ("--offline" %in% args) stop("Fonte ausente no cache: ", caminho)
    baixar(manifesto$url[i], caminho)
  }
  hash <- unname(tools::md5sum(caminho))
  if ("--atualizar" %in% args) {
    manifesto$md5[i] <- hash
    manifesto$baixado_em_utc[i] <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else if (!identical(hash, manifesto$md5[i])) {
    stop("Hash divergente: ", caminho, ". Não é possível reproduzir esta fonte.")
  }
  message(i, "/", nrow(manifesto), " ", manifesto$arquivo[i])
  fontes[[i]] <- if (manifesto$formato[i] == "sql") {
    tpu_ler_ancestrais_sql(caminho, manifesto$tipo[i])
  } else {
    tpu_ler(caminho, manifesto$segmento[i], manifesto$grau[i])
  }
}
# Consolidar e validar ambos antes de substituir os artefatos publicados.
consolidar_tipo <- function(tipo) {
  usar <- manifesto$tipo == tipo
  tpu_consolidar(fontes[usar & manifesto$formato == "xls"],
    dplyr::bind_rows(fontes[usar & manifesto$formato == "sql"]))
}
datajud_assuntos <- consolidar_tipo("A")
datajud_classes <- consolidar_tipo("C")
for (tipo in c("A", "C")) {
  nome <- if (tipo == "A") "datajud_assuntos" else "datajud_classes"
  x <- get(nome)
  attr(x, "tpu_fontes") <- manifesto[manifesto$tipo == tipo, ]
  assign(nome, x)
}
save(datajud_assuntos, file = "data/datajud_assuntos.rda", compress = "xz", version = 2)
save(datajud_classes, file = "data/datajud_classes.rda", compress = "xz", version = 2)
utils::write.csv(manifesto, manifesto_path, row.names = FALSE, na = "")
message("Gerados ", nrow(datajud_assuntos), " assuntos e ", nrow(datajud_classes), " classes.")
