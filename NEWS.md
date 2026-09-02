# datajud 0.0.0.9000

* A API pública passa a usar `datajud_cliente()` e deixa de criar ou consultar
  objetos no `.GlobalEnv`.
* `datajud_login()` foi removida. Configure a chave por argumento ou pela
  variável `DATAJUD_API_KEY`; na ausência de ambas, o pacote consulta a chave
  pública na Wiki do CNJ e possui uma cópia vigente como contingência.
* `datajud_consultar_processo()` e `datajud_pesquisar_classe_orgao()` agora
  exigem um `cliente` explícito e retornam os resultados diretamente.
* `datajud_ler_processo()` e `datajud_ler_movimentacoes()` agora exigem a lista
  de respostas em `base`, retornam tibbles diretamente e não imprimem o objeto.
* A leitura de processos preserva o `id` como chave interna, mantém assuntos
  estruturados em list-column e oferece `datajud_desaninhar_assuntos()` para
  obter uma linha por assunto.
* Leitores de movimentos retornam um esquema vazio estável quando o processo
  não possui movimentos ou quando o campo é omitido.
