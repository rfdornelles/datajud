# mensagens HTTP sanitizadas permanecem estáveis

    Code
      for (status in c(400L, 401L, 403L, 404L, 429L, 500L, 502L, 503L)) {
        erro <- erro_http_simulado(status, cliente)
        cat(paste(status, class(erro)[[1]], conditionMessage(erro), sep = " | "),
        "\n", sep = "")
      }
    Output
      400 | datajud_erro_requisicao | A API rejeitou a consulta (HTTP 400). Revise os filtros e os tipos informados.
      401 | datajud_erro_autenticacao | A API recusou a chave pública (HTTP 401). Reconfigure DATAJUD_API_KEY ou consulte a chave vigente na Wiki do CNJ.
      403 | datajud_erro_autenticacao | A API recusou a chave pública (HTTP 403). Reconfigure DATAJUD_API_KEY ou consulte a chave vigente na Wiki do CNJ.
      404 | datajud_erro_nao_encontrado | O recurso solicitado não foi encontrado (HTTP 404). Confirme o endereço e os parâmetros informados.
      429 | datajud_erro_limite | O limite de requisições foi atingido (HTTP 429). Aguarde antes de tentar novamente.
      500 | datajud_erro_servidor | O serviço remoto está temporariamente indisponível (HTTP 500). Tente novamente mais tarde.
      502 | datajud_erro_servidor | O serviço remoto está temporariamente indisponível (HTTP 502). Tente novamente mais tarde.
      503 | datajud_erro_servidor | O serviço remoto está temporariamente indisponível (HTTP 503). Tente novamente mais tarde.
