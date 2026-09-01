# datajud (desenvolvimento)

* A API pública passa a usar `datajud_cliente()` e deixa de criar ou consultar
  objetos no `.GlobalEnv`.
* `datajud_login()` foi removida. Configure a chave por argumento ou pela
  variável `DATAJUD_API_KEY`.
* Consultas e leitores retornam valores diretamente e aceitam composição com
  pipes.
