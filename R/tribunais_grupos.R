# Seleção local de tribunais por ramo da Justiça.

grupos_tribunais_datajud <- function() {
  list(
    todos = unique(datajud::tribunais$tipo),
    justica_comum = c("Estadual", "Federal"),
    estadual = "Estadual",
    federal = "Federal",
    eleitoral = "Eleitoral",
    superiores = "Superior",
    trabalho = "Trabalhista",
    militar_estadual = "Militar Estadual"
  )
}

#' Listar tribunais por grupo
#'
#' Seleciona localmente os endpoints de tribunais por ramo da Justiça. O grupo
#' `justica_comum` reúne Justiça Estadual e Justiça Federal. É possível informar
#' mais de um grupo; tribunais repetidos são devolvidos uma única vez.
#'
#' @param grupo Um ou mais grupos entre `todos`, `justica_comum`, `estadual`,
#'   `federal`, `eleitoral`, `superiores`, `trabalho` e `militar_estadual`.
#'
#' @return Tibble com nome, sigla, tipo e URL dos tribunais selecionados.
#' @export
#'
#' @examples
#' datajud_listar_tribunais("justica_comum")
#' datajud_listar_tribunais(c("eleitoral", "trabalho"))
datajud_listar_tribunais <- function(grupo = "todos") {
  grupos <- grupos_tribunais_datajud()
  valido <- is.character(grupo) &&
    length(grupo) > 0L &&
    !anyNA(grupo) &&
    all(nzchar(grupo))

  if (!valido) {
    cli::cli_abort("{.arg grupo} deve conter um ou mais nomes de grupos.")
  }

  desconhecidos <- setdiff(grupo, names(grupos))
  if (length(desconhecidos) > 0L) {
    cli::cli_abort(
      "Grupo{?s} desconhecido{?s}: {.val {desconhecidos}}.",
      class = "datajud_erro_grupo_tribunal"
    )
  }
  if ("todos" %in% grupo && length(unique(grupo)) > 1L) {
    cli::cli_abort("{.val todos} n\u00E3o pode ser combinado com outro grupo.")
  }

  tipos <- unique(unlist(grupos[unique(grupo)], use.names = FALSE))
  datajud::tribunais |>
    dplyr::filter(.data$tipo %in% tipos) |>
    tibble::as_tibble()
}
