normalizar_busca_tpu <- function(x) {
  x <- stringi::stri_trans_general(x, "NFD; [:Nonspacing Mark:] Remove; NFC")
  stringi::stri_trans_tolower(stringi::stri_trim_both(x), locale = "pt_BR")
}

buscar_tpu <- function(dados, termo, ativos, codigo_pai, limite) {
  if (!is.null(termo)) {
    valido <- !is.object(termo) && length(termo) == 1L && !anyNA(termo) &&
      (is.character(termo) || (is.numeric(termo) && !is.complex(termo) &&
        is.finite(termo) && termo > 0 && termo == floor(termo) &&
        termo <= .Machine$integer.max))
    if (!valido) cli::cli_abort("{.arg termo} deve ser um texto ou c\u00f3digo inteiro positivo \u00fanico, ou NULL.")
    if (is.numeric(termo)) termo <- format(termo, scientific = FALSE, trim = TRUE)
    termo <- normalizar_busca_tpu(termo)
    if (!nzchar(termo)) cli::cli_abort("{.arg termo} n\u00e3o pode ser vazio.")
  }
  if (!is.null(ativos)) ativos <- validar_exigir_todos(ativos, "ativos")
  if (!is.null(codigo_pai)) {
    if (length(codigo_pai) != 1L) cli::cli_abort("{.arg codigo_pai} deve ser um valor \u00fanico.")
    raiz <- identical(codigo_pai, NA) || identical(codigo_pai, NA_integer_) ||
      identical(codigo_pai, NA_real_)
    if (!raiz) {
      codigo_pai <- validar_codigos_consulta(codigo_pai, "codigo_pai")
      if (length(codigo_pai) != 1L || codigo_pai > .Machine$integer.max) {
        cli::cli_abort("{.arg codigo_pai} deve ser um c\u00f3digo inteiro positivo \u00fanico, NA ou NULL.")
      }
    }
  }
  valido <- is.numeric(limite) && !is.complex(limite) && !is.object(limite) &&
    length(limite) == 1L && !is.na(limite) && limite > 0 &&
    (identical(limite, Inf) || (is.finite(limite) && limite == floor(limite) &&
      limite <= .Machine$integer.max))
  if (!valido) cli::cli_abort("{.arg limite} deve ser um inteiro positivo ou Inf.")

  usar <- rep(TRUE, nrow(dados))
  if (!is.null(ativos)) usar <- usar & dados$ativo == ativos
  if (!is.null(codigo_pai)) {
    usar <- usar & if (is.na(codigo_pai)) is.na(dados$codigo_pai) else
      !is.na(dados$codigo_pai) & dados$codigo_pai == codigo_pai
  }
  relevancia <- rep(4L, nrow(dados))
  if (!is.null(termo)) {
    nomes <- normalizar_busca_tpu(dados$nome)
    relevancia[grepl(termo, nomes, fixed = TRUE)] <- 3L
    relevancia[startsWith(nomes, termo)] <- 2L
    relevancia[nomes == termo] <- 1L
    if (grepl("^[0-9]+$", termo)) {
      relevancia[dados$codigo == as.numeric(termo)] <- 0L
    }
    usar <- usar & relevancia < 4L
  }
  indices <- which(usar)
  indices <- indices[order(relevancia[indices], dados$codigo[indices], method = "radix")]
  if (is.finite(limite)) indices <- utils::head(indices, as.integer(limite))
  dados[indices, , drop = FALSE]
}

#' Buscar assuntos e classes nas TPU locais
#'
#' Localiza códigos nos datasets distribuídos com o pacote, sem acessar a rede.
#' A busca ignora caixa e acentos, inclusive acentos Unicode decompostos.
#'
#' @param termo Texto ou código inteiro positivo único. `NULL` lista os itens
#'   que satisfazem os filtros. Espaços nas extremidades são ignorados;
#'   texto vazio ou composto apenas por espaços é inválido.
#' @param ativos `TRUE` (padrão) retorna apenas ativos; `FALSE`, apenas inativos;
#'   `NULL`, ambos.
#' @param codigo_pai Código inteiro positivo único para listar filhos diretos.
#'   `NA` seleciona raízes; `NULL` não filtra por pai. O filtro não inclui
#'   descendentes indiretos e mantém o status original dos itens.
#' @param limite Número máximo de resultados, por padrão 20. Use `Inf` para
#'   retornar todos os resultados encontrados.
#'
#' @return Tibble com as mesmas colunas, tipos e atributos de proveniência de
#'   [datajud_assuntos] ou [datajud_classes]. A coluna `codigo` pode ser passada
#'   diretamente a [datajud_pesquisar_processos()]. Sem correspondência,
#'   retorna um tibble com zero linhas e o mesmo esquema.
#' @details
#' A ordenação prioriza código exato, nome exato, prefixo do nome e trecho do
#' nome, nessa ordem. Empates são resolvidos por código crescente. Sem termo,
#' a ordem é por código crescente. Os filtros são aplicados antes do limite.
#' Um termo numérico (ou texto contendo apenas dígitos) compara o código inteiro
#' exato e também procura esse texto no nome; códigos não são buscados por prefixo.
#' Caracteres como `.` e `*` são literais, não expressões regulares.
#'
#' A normalização usa Unicode NFD, remoção das marcas de acento, recomposição
#' NFC e conversão para minúsculas com locale explícito `pt_BR`. Os nomes
#' retornados preservam a grafia oficial. Não há busca aproximada.
#' Os atributos de proveniência descrevem a base distribuída, não apenas os
#' resultados selecionados.
#'
#' @export
#' @examples
#' datajud_buscar_assunto("educacao", limite = 5)
#' datajud_buscar_classe("procedimento comum")
#' datajud_buscar_assunto(899, ativos = NULL)
#' datajud_buscar_assunto(codigo_pai = 12775, ativos = NULL, limite = Inf)
#' datajud_buscar_classe(codigo_pai = NA, ativos = NULL)
#' \dontrun{
#' assuntos <- datajud_buscar_assunto("educacao", limite = 5)
#' processos <- datajud_pesquisar_processos(
#'   "TJSP", assunto_codigo = assuntos$codigo
#' )
#' classe <- datajud_buscar_classe("procedimento comum civel", limite = 1)
#' processos <- datajud_pesquisar_processos(
#'   "TJSP", classe_codigo = classe$codigo
#' )
#' }
datajud_buscar_assunto <- function(termo = NULL, ativos = TRUE,
                                   codigo_pai = NULL, limite = 20L) {
  buscar_tpu(datajud::datajud_assuntos, termo, ativos, codigo_pai, limite)
}

#' @rdname datajud_buscar_assunto
#' @export
datajud_buscar_classe <- function(termo = NULL, ativos = TRUE,
                                  codigo_pai = NULL, limite = 20L) {
  buscar_tpu(datajud::datajud_classes, termo, ativos, codigo_pai, limite)
}
