#!/usr/bin/env Rscript

# Publica o manifesto do roadmap no GitHub de forma idempotente.
# O modo padrão apenas valida e descreve as operações. Use --publicar para
# criar ou sincronizar recursos remotos e preencher os identificadores no JSON.

arquivo_manifesto <- file.path("roadmap", "issues.json")
publicar <- "--publicar" %in% commandArgs(trailingOnly = TRUE)

abortar <- function(...) {
  stop(paste0(...), call. = FALSE)
}

informar <- function(...) {
  message(paste0(...))
}

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  abortar("O pacote jsonlite é necessário para publicar o roadmap.")
}

if (!file.exists(arquivo_manifesto)) {
  abortar("Execute este script na raiz do repositório; não encontrei ", arquivo_manifesto, ".")
}

manifesto <- jsonlite::fromJSON(
  arquivo_manifesto,
  simplifyVector = FALSE
)

executar <- function(argumentos, permitir_falha = FALSE) {
  argumentos <- vapply(argumentos, shQuote, character(1), USE.NAMES = FALSE)
  saida <- suppressWarnings(system2("gh", argumentos, stdout = TRUE, stderr = TRUE))
  status <- attr(saida, "status")

  if (is.null(status)) {
    status <- 0L
  }

  resultado <- list(
    status = as.integer(status),
    saida = paste(saida, collapse = "\n")
  )

  if (resultado$status != 0L && !permitir_falha) {
    abortar(
      "O comando gh falhou (status ", resultado$status, "):\n",
      resultado$saida
    )
  }

  resultado
}

gh_json <- function(argumentos) {
  resultado <- executar(argumentos)

  if (!nzchar(resultado$saida)) {
    return(list())
  }

  jsonlite::fromJSON(resultado$saida, simplifyVector = FALSE)
}

colecao <- function(objeto, nome) {
  if (!is.null(objeto[[nome]])) {
    return(objeto[[nome]])
  }

  if (is.list(objeto) && is.null(names(objeto))) {
    return(objeto)
  }

  list()
}

valor_texto <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }

  as.character(x[[1L]])
}

validar_manifesto <- function(x) {
  campos <- c("versao_schema", "repositorio", "proprietario_projeto", "projeto", "issues")
  ausentes <- setdiff(campos, names(x))

  if (length(ausentes) > 0L) {
    abortar("Campos obrigatórios ausentes: ", paste(ausentes, collapse = ", "), ".")
  }

  ids <- vapply(x$issues, function(issue) issue$id, character(1))
  ordens <- vapply(x$issues, function(issue) as.integer(issue$ordem), integer(1))

  if (anyDuplicated(ids)) {
    abortar("Existem IDs de issue duplicados no manifesto.")
  }

  if (!identical(ordens, seq_along(ordens))) {
    abortar("O campo ordem deve formar a sequência de 1 até o total de issues.")
  }

  milestones <- vapply(x$milestones, function(item) item$titulo, character(1))
  labels <- vapply(x$labels, function(item) item$nome, character(1))

  for (posicao in seq_along(x$issues)) {
    issue <- x$issues[[posicao]]
    dependencias <- unlist(issue$dependencias, use.names = FALSE)

    if (!issue$milestone %in% milestones) {
      abortar(issue$id, " referencia milestone inexistente: ", issue$milestone, ".")
    }

    labels_invalidas <- setdiff(unlist(issue$labels, use.names = FALSE), labels)
    if (length(labels_invalidas) > 0L) {
      abortar(issue$id, " referencia labels inexistentes: ", paste(labels_invalidas, collapse = ", "), ".")
    }

    dependencias_invalidas <- setdiff(dependencias, ids)
    if (length(dependencias_invalidas) > 0L) {
      abortar(issue$id, " referencia dependências inexistentes: ", paste(dependencias_invalidas, collapse = ", "), ".")
    }

    if (length(dependencias) > 0L) {
      posicoes_dependencias <- match(dependencias, ids)
      if (any(posicoes_dependencias >= posicao)) {
        abortar(issue$id, " possui dependência posterior ou circular.")
      }
    }
  }

  invisible(TRUE)
}

validar_manifesto(manifesto)

if (!publicar) {
  informar("Manifesto válido: ", length(manifesto$issues), " issues.")
  informar("Projeto: ", manifesto$projeto$titulo)
  informar("Repositório: ", manifesto$repositorio)
  informar("Modo de simulação; nenhuma alteração local ou remota foi realizada.")

  for (issue in manifesto$issues) {
    informar(sprintf("%02d  %s  %s", issue$ordem, issue$id, issue$titulo))
  }

  quit(save = "no", status = 0L)
}

if (!nzchar(Sys.which("gh"))) {
  abortar("O GitHub CLI (gh) não está disponível.")
}

executar(c("auth", "status"))

repositorio <- manifesto$repositorio
proprietario <- manifesto$proprietario_projeto
titulo_projeto <- manifesto$projeto$titulo

informar("Sincronizando labels...")
for (label in manifesto$labels) {
  executar(c(
    "label", "create", label$nome,
    "--repo", repositorio,
    "--color", label$cor,
    "--description", label$descricao,
    "--force"
  ))
}

informar("Sincronizando milestones...")
milestones_existentes <- gh_json(c(
  "api", paste0("repos/", repositorio, "/milestones?state=all&per_page=100")
))

for (milestone in manifesto$milestones) {
  indice <- which(vapply(
    milestones_existentes,
    function(item) identical(item$title, milestone$titulo),
    logical(1)
  ))

  if (length(indice) == 0L) {
    criado <- gh_json(c(
      "api", "--method", "POST",
      paste0("repos/", repositorio, "/milestones"),
      "-f", paste0("title=", milestone$titulo),
      "-f", paste0("description=", milestone$descricao)
    ))
    milestones_existentes[[length(milestones_existentes) + 1L]] <- criado
  } else {
    numero <- milestones_existentes[[indice[[1L]]]]$number
    executar(c(
      "api", "--method", "PATCH",
      paste0("repos/", repositorio, "/milestones/", numero),
      "-f", paste0("title=", milestone$titulo),
      "-f", paste0("description=", milestone$descricao),
      "--silent"
    ))
  }
}

informar("Localizando ou criando o GitHub Project...")
projetos_resposta <- gh_json(c(
  "project", "list", "--owner", proprietario,
  "--limit", "100", "--format", "json"
))
projetos <- colecao(projetos_resposta, "projects")
indice_projeto <- which(vapply(
  projetos,
  function(projeto) identical(projeto$title, titulo_projeto),
  logical(1)
))
projeto_criado <- FALSE

if (length(indice_projeto) == 0L) {
  projeto <- gh_json(c(
    "project", "create", "--owner", proprietario,
    "--title", titulo_projeto, "--format", "json"
  ))
  projeto_criado <- TRUE
} else {
  projeto <- projetos[[indice_projeto[[1L]]]]
}

numero_projeto <- as.integer(projeto$number)

projeto <- gh_json(c(
  "project", "edit", as.character(numero_projeto),
  "--owner", proprietario,
  "--title", titulo_projeto,
  "--description", manifesto$projeto$descricao,
  "--readme", manifesto$politica_execucao$aprovacao,
  "--visibility", "PUBLIC",
  "--format", "json"
))

executar(c(
  "project", "link", as.character(numero_projeto),
  "--owner", proprietario,
  "--repo", repositorio
))

listar_campos <- function() {
  resposta <- gh_json(c(
    "project", "field-list", as.character(numero_projeto),
    "--owner", proprietario,
    "--limit", "100", "--format", "json"
  ))
  colecao(resposta, "fields")
}

nomes_opcoes <- function(campo) {
  if (is.null(campo$options)) {
    return(character())
  }

  vapply(campo$options, function(opcao) opcao$name, character(1))
}

informar("Configurando campos do Project...")
for (configuracao in manifesto$projeto$campos) {
  campos <- listar_campos()
  indice <- which(vapply(
    campos,
    function(campo) identical(campo$name, configuracao$nome),
    logical(1)
  ))
  desejadas <- unlist(configuracao$opcoes, use.names = FALSE)

  if (length(indice) > 0L) {
    campo <- campos[[indice[[1L]]]]
    existentes <- nomes_opcoes(campo)

    if (identical(existentes, desejadas)) {
      next
    }

    if (!projeto_criado) {
      abortar(
        "O campo ", configuracao$nome,
        " já existe, mas suas opções divergem do manifesto. Ajuste-o manualmente antes de repetir a publicação."
      )
    }

    executar(c("project", "field-delete", "--id", campo$id, "--format", "json"))
  }

  executar(c(
    "project", "field-create", as.character(numero_projeto),
    "--owner", proprietario,
    "--name", configuracao$nome,
    "--data-type", "SINGLE_SELECT",
    "--single-select-options", paste(desejadas, collapse = ","),
    "--format", "json"
  ))
}

manifesto$projeto$github$numero <- numero_projeto
manifesto$projeto$github$id <- valor_texto(projeto$id)
manifesto$projeto$github$url <- valor_texto(projeto$url)

salvar_manifesto <- function() {
  texto <- jsonlite::toJSON(
    manifesto,
    auto_unbox = TRUE,
    pretty = TRUE,
    null = "null",
    na = "null"
  )
  writeLines(texto, arquivo_manifesto, useBytes = TRUE)
}

salvar_manifesto()

informar("Carregando issues existentes...")
issues_existentes <- gh_json(c(
  "issue", "list", "--repo", repositorio,
  "--state", "all", "--limit", "1000",
  "--json", "number,title,body,url,id"
))

numeros <- setNames(rep(NA_integer_, length(manifesto$issues)), vapply(
  manifesto$issues,
  function(issue) issue$id,
  character(1)
))

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

informar("Carregando o estado atual dos itens no Project...")
saida_itens <- executar(c(
  "project", "item-list", as.character(numero_projeto),
  "--owner", proprietario,
  "--limit", "1000",
  "--field", "Etapa",
  "--field", "Decisão",
  "--field", "Fase",
  "--field", "Prioridade"
))$saida

itens_projeto <- list()
if (nzchar(saida_itens)) {
  linhas <- strsplit(saida_itens, "\n", fixed = TRUE)[[1L]]
  for (linha in linhas[nzchar(linhas)]) {
    colunas <- strsplit(linha, "\t", fixed = TRUE)[[1L]]
    length(colunas) <- max(length(colunas), 9L)
    numero_item <- colunas[[3L]]

    itens_projeto[[numero_item]] <- list(
      id = colunas[[5L]],
      campos = list(
        Etapa = colunas[[6L]],
        Decisão = colunas[[7L]],
        Fase = colunas[[8L]],
        Prioridade = colunas[[9L]]
      )
    )
  }
}

marcador <- function(id) {
  paste0("<!-- roadmap_id:", id, " -->")
}

localizar_issue <- function(id) {
  alvo <- marcador(id)
  which(vapply(
    issues_existentes,
    function(issue) grepl(alvo, issue$body %||% "", fixed = TRUE),
    logical(1)
  ))
}

lista_markdown <- function(itens, checklist = FALSE) {
  itens <- unlist(itens, use.names = FALSE)
  if (length(itens) == 0L) {
    return("- Nenhum item.")
  }

  prefixo <- if (checklist) "- [ ] " else "- "
  paste0(prefixo, itens, collapse = "\n")
}

corpo_issue <- function(issue) {
  dependencias <- unlist(issue$dependencias, use.names = FALSE)
  texto_dependencias <- if (length(dependencias) == 0L) {
    "- Nenhuma dependência."
  } else {
    paste(vapply(dependencias, function(id) {
      numero <- numeros[[id]]
      if (is.na(numero)) paste0("- `", id, "`") else paste0("- #", numero, " — `", id, "`")
    }, character(1)), collapse = "\n")
  }

  paste(
    marcador(issue$id),
    "## Objetivo",
    issue$motivacao,
    "## Escopo",
    lista_markdown(issue$escopo),
    "## Fora de escopo",
    lista_markdown(issue$fora_de_escopo),
    "## Critérios de aceite",
    lista_markdown(issue$criterios_aceite, checklist = TRUE),
    "## Testes esperados",
    lista_markdown(issue$testes, checklist = TRUE),
    "## Dependências",
    texto_dependencias,
    "## Política de entrega",
    paste0(
      "Esta issue pertence à fase ", issue$fase,
      ", prioridade ", issue$prioridade,
      ", e será implementada em uma branch e um pull request exclusivos. " ,
      "O trabalho só começa após a decisão **Aprovada** no Project."
    ),
    sep = "\n\n"
  )
}

informar("Criando ou sincronizando as issues...")
for (posicao in seq_along(manifesto$issues)) {
  issue <- manifesto$issues[[posicao]]
  titulo <- paste0("[", issue$id, "] ", issue$titulo)
  corpo <- corpo_issue(issue)
  indice <- localizar_issue(issue$id)

  if (length(indice) == 0L) {
    arquivo_corpo <- tempfile(fileext = ".md")
    writeLines(corpo, arquivo_corpo, useBytes = TRUE)
    on.exit(unlink(arquivo_corpo), add = TRUE)

    argumentos <- c(
      "issue", "create", "--repo", repositorio,
      "--title", titulo,
      "--body-file", arquivo_corpo,
      "--milestone", issue$milestone
    )

    for (label in unlist(issue$labels, use.names = FALSE)) {
      argumentos <- c(argumentos, "--label", label)
    }

    dependencias <- unlist(issue$dependencias, use.names = FALSE)
    if (length(dependencias) > 0L) {
      bloqueios <- unname(numeros[dependencias])
      argumentos <- c(argumentos, "--blocked-by", paste(bloqueios, collapse = ","))
    }

    resultado <- executar(argumentos)
    url <- tail(strsplit(resultado$saida, "\n", fixed = TRUE)[[1L]], 1L)
    criada <- gh_json(c(
      "issue", "view", url, "--repo", repositorio,
      "--json", "number,url,id,title,body"
    ))
    issues_existentes[[length(issues_existentes) + 1L]] <- criada
    indice <- length(issues_existentes)
  } else {
    remota_existente <- issues_existentes[[indice[[1L]]]]

    if (!identical(remota_existente$title, titulo) || !identical(remota_existente$body, corpo)) {
      arquivo_corpo <- tempfile(fileext = ".md")
      writeLines(corpo, arquivo_corpo, useBytes = TRUE)
      on.exit(unlink(arquivo_corpo), add = TRUE)

      argumentos <- c(
        "issue", "edit", as.character(remota_existente$number),
        "--repo", repositorio,
        "--title", titulo,
        "--body-file", arquivo_corpo,
        "--milestone", issue$milestone
      )
      for (label in unlist(issue$labels, use.names = FALSE)) {
        argumentos <- c(argumentos, "--add-label", label)
      }
      executar(argumentos)
      issues_existentes[[indice[[1L]]]]$title <- titulo
      issues_existentes[[indice[[1L]]]]$body <- corpo
    }
  }

  remota <- issues_existentes[[indice[[1L]]]]
  numeros[[issue$id]] <- as.integer(remota$number)
  manifesto$issues[[posicao]]$github$numero <- as.integer(remota$number)
  manifesto$issues[[posicao]]$github$url <- remota$url
  manifesto$issues[[posicao]]$github$id <- remota$id

  chave_item <- as.character(remota$number)
  registro_item <- itens_projeto[[chave_item]]

  if (is.null(registro_item)) {
    adicao <- executar(c(
      "project", "item-add", as.character(numero_projeto),
      "--owner", proprietario,
      "--url", remota$url,
      "--format", "json"
    ))
    item <- jsonlite::fromJSON(adicao$saida, simplifyVector = FALSE)
    registro_item <- list(
      id = valor_texto(item$id),
      campos = list(Etapa = "", Decisão = "", Fase = "", Prioridade = "")
    )
    itens_projeto[[chave_item]] <- registro_item
  } else {
    registro_item$id <- valor_texto(registro_item$id)
  }
  manifesto$issues[[posicao]]$github$item_id <- registro_item$id

  valores_campos <- list(
    Etapa = "Backlog",
    Decisão = "Aguardando aprovação",
    Fase = paste("Fase", issue$fase),
    Prioridade = issue$prioridade
  )

  for (nome_campo in names(valores_campos)) {
    atual <- registro_item$campos[[nome_campo]] %||% ""
    desejado <- valores_campos[[nome_campo]]

    if (!identical(atual, desejado)) {
      executar(c(
        "project", "item-edit", as.character(numero_projeto),
        "--owner", proprietario,
        "--url", remota$url,
        "--field", nome_campo,
        "--value", desejado
      ))
      registro_item$campos[[nome_campo]] <- desejado
    }
  }
  itens_projeto[[chave_item]] <- registro_item

  salvar_manifesto()
  informar(sprintf("[%02d/%02d] %s -> #%d", posicao, length(manifesto$issues), issue$id, remota$number))
}

salvar_manifesto()

informar("Roadmap publicado e manifesto atualizado em ", arquivo_manifesto, ".")
