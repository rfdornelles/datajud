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

  return(body)
}
