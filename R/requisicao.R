## requisicoes

datajud_requisition <- function(tribunal, processo, sleep = 0.1) {

  # checa se há key definida
  key = get_key()

  # headers
  headers = c(
    'Authorization' = paste0('APIKey ', key),
    'Content-Type' = 'application/json'
  )

  # body
  body = glue::glue(
    '{{
      "size": 10000,
      "query": {{
        "match": {{
          "numeroProcesso": "{processo}"
        }}
      }}
    }}'
  )

  # url
  url_tribunal <- switch(
    tribunal,
    "tst" = "https://api.datajud.com.br/v1/processos/tst/_search",
    "tse" = "https://api.datajud.com.br/v1/processos/tse/_search",
    "stj" = "https://api.datajud.com.br/v1/processos/stj/_search",
    "stm" = "https://api.datajud.com.br/v1/processos/stm/_search",
    "trf1" = "https://api.datajud.com.br/v1/processos/trf1/_search",
    "trf2" = "https://api.datajud.com.br/v1/processos/trf2/_search",
    "trf3" = "https://api.datajud.com.br/v1/processos/trf3/_search",
    "trf4" = "https://api.datajud.com.br/v1/processos/trf4/_search",
    "trf5" = "https://api.datajud.com.br/v1/processos/trf5/_search",
    "trf6" = "https://api.datajud.com.br/v1/processos/trf6/_search",
    "tjac" = "https://api.datajud.com.br/v1/processos/tjac/_search",
    "tjal" = "https://api.datajud.com.br/v1/processos/tjal/_search",
    "tjam" = "https://api.datajud.com.br/v1/processos/tjam/_search",
    "tjap" = "https://api.datajud.com.br/v1/processos/tjap/_search",
    "tjba" = "https://api.datajud.com.br/v1/processos/tjba/_search",
    "tjce" = "https://api.datajud.com.br/v1/processos/tjce/_search",
    "tjdf" = "https://api.datajud.com.br/v1/processos/tjdf/_search",
    "tjgo" = "https://api.datajud.com.br/v1/processos/tjgo/_search",
    "tjma" = "https://api.datajud.com.br/v1/processos/tjma/_search",
    "tjmg" = "https://api.datajud.com.br/v1/processos/tjmg/_search",
    "tjms" = "https://api.datajud.com.br/v1/processos/tjms/_search",
    "tjmt" = "https://api.datajud.com.br/v1/processos/tjmt/_search",
    "tjpa" = "https://api.datajud.com.br/v1/processos/tjpa/_search",
    "tjpb" = "https://api.datajud.com.br/v1/processos/tjpb/_search",
    "tjpe" = "https://api.datajud.com.br/v1/processos/tjpe/_search",
    "tjpi" = "https://api.datajud.com.br/v1/processos/tjpi/_search",
    "tjpr" = "https://api.datajud.com.br/v1/processos/tjpr/_search",
    "tjro" = "https://api.datajud.com.br/v1/processos/tjro/_search",
    "tjrr" = "https://api.datajud.com.br/v1/processos/tjrr/_search",
    "tjrs" = "https://api.datajud.com.br/v1/processos/tjrs/_search",
    "tjsc" = "https://api.datajud.com.br/v1/processos/tjsc/_search",
    "tjsp" = "https://api.datajud.com.br/v1/processos/tjsp/_search",
    "tjto" = "https://api.datajud.com.br/v1/processos/tjto/_search",
    "trt1" = "https://api.datajud.com.br/v1/processos/trt1/_search",
    "trt2" = "https://api.datajud.com.br/v1/processos/trt2/_search",
    "trt3" = "https://api.datajud.com.br/v1/processos/trt3/_search",
    "trt4" = "https://api.datajud.com.br/v1/processos/trt4/_search",
    "trt5" = "https://api.datajud.com.br/v1/processos/trt5/_search",
    "trt6" = "https://api.datajud.com.br/v1/processos/trt6/_search",
    "trt7" = "https://api.datajud.com.br/v1/processos/trt7/_search",
    "trt8" = "https://api.datajud.com.br/v1/processos/trt8/_search",
    "trt9" = "https://api.datajud.com.br/v1/processos/trt9/_search",
    "trt10" = "https://api.datajud.com.br/v1/processos/trt10/_search",
    "trt11" = "https://api.datajud.com.br/v1/processos/trt11/_search",
    "trt12" = "https://api.datajud.com.br/v1/processos/trt12/_search",
    "trt13" = "https://api.datajud.com.br/v1/processos/trt13/_search",
    "trt14" = "https://api.datajud.com.br/v1/processos/trt14/_search",
    "trt15" = "https://api.datajud.com.br/v1/processos/trt15/_search",
    "trt16" = "https://api.datajud.com.br/v1/processos/trt16/_search",
    "trt17" = "https://api.datajud.com.br/v1/processos/trt17/_search",
    "trt18" = "https://api.datajud.com.br/v1/processos/trt18/_search",
    "trt19" = "https://api.datajud.com.br/v1/processos/trt19/_search",
    "trt20" = "https://api.datajud.com.br/v1/processos/trt20/_search",
    "trt21" = "https://api.datajud.com.br/v1/processos/trt21/_search",
    "trt22" = "https://api.datajud.com.br/v1/processos/trt22/_search",
    "trt23" = "https://api.datajud.com.br/v1/processos/trt23/_search",
    "trt24" = "https://api.datajud.com.br/v1/processos/trt24/_search",
    "tre-ac" = "https://api.datajud.com.br/v1/processos/tre-ac/_search",
    "tre-al" = "https://api.datajud.com.br/v1/processos/tre-al/_search",
    "tre-am" = "https://api.datajud.com.br/v1/processos/tre-am/_search",
    "tre-ap" = "https://api.datajud.com.br/v1/processos/tre-ap/_search",
    "tre-ba" = "https://api.datajud.com.br/v1/processos/tre-ba/_search",
    "tre-ce" = "https://api.datajud.com.br/v1/processos/tre-ce/_search",
    "tre-df" = "https://api.datajud.com.br/v1/processos/tre-df/_search",
    "tre-es" = "https://api.datajud.com.br/v1/processos/tre-es/_search",
    "tre-go" = "https://api.datajud.com.br/v1/processos/tre-go/_search",
    "tre-ma" = "https://api.datajud.com.br/v1/processos/tre-ma/_search",
    "tre-mg" = "https://api.datajud.com.br/v1/processos/tre-mg/_search",
    "tre-ms" = "https://api.datajud.com.br/v1/processos/tre-ms/_search",
    "tre-mt" = "https://api.datajud.com.br/v1/processos/tre-mt/_search",
    "tre-pa" = "https://api.datajud.com.br/v1/processos/tre-pa/_search",
    "tre-pb" = "https://api.datajud.com.br/v1/processos/tre-pb/_search",
    "tre-pe" = "https://api.datajud.com.br/v1/processos/tre-pe/_search",
    "tre-pi" = "https://api.datajud.com.br/v1/processos/tre-pi/_search",
    "tre-pr" = "https://api.datajud.com.br/v1/processos/tre-pr/_search",
    "tre-rj" = "https://api.datajud.com.br/v1/processos/tre-rj/_search",
    "tre-rn" = "https://api.datajud.com.br/v1/processos/tre-rn/_search",
    "tre-ro" = "https://api.datajud.com.br/v1/processos/tre-ro/_search",
    "tre-rr" = "https://api.datajud.com.br/v1/processos/tre-rr/_search",
    "tre-rs" = "https://api.datajud.com.br/v1/processos/tre-rs/_search",
    "tre-sc" = "https://api.datajud.com.br/v1/processos/tre-sc/_search",
    "tre-se" = "https://api.datajud.com.br/v1/processos/tre-se/_search",
    "tre-sp" = "https://api.datajud.com.br/v1/processos/tre-sp/_search",
    "tre-to" = "https://api.datajud.com.br/v1/processos/tre-to/_search",
    "tjmmg" = "https://api.datajud.com.br/v1/processos/tjmmg/_search",
    "tjmrs" = "https://api.datajud.com.br/v1/processos/tjmrs/_search",
    "tjmsp" = "https://api.datajud.com.br/v1/processos/tjmsp/_search",
  )



  # requisicao
  requisicao <- httr::POST(
    url = url_tribunal,
    body = body,
    httr::add_headers(headers),
    user()
  )

}
