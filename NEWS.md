# datajud (desenvolvimento)

* A API pública passa a usar `datajud_cliente()` e deixa de criar ou consultar
  objetos no `.GlobalEnv`.
* `datajud_login()` foi removida. Configure a chave por argumento ou pela
  variável `DATAJUD_API_KEY`.
* Consultas e leitores retornam valores diretamente e aceitam composição com
  pipes.
* A leitura de processos preserva o `id` como chave interna, mantém assuntos
  estruturados em list-column e oferece `datajud_desaninhar_assuntos()` para
  obter uma linha por assunto.
* Leitores de movimentos retornam um esquema vazio estável quando o processo
  não possui movimentos ou quando o campo é omitido.
