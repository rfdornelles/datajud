# Contrato observado da API Pública do Datajud

Registro realizado em **2 de setembro de 2026** a partir da Wiki oficial do
CNJ e de probes mínimos contra o endpoint público do TJSP. Este documento
separa deliberadamente o que está documentado do que foi apenas observado.

## Decisões estáveis para o pacote

| Tema | Decisão | Evidência |
| --- | --- | --- |
| Consulta individual | A pesquisa de processo é feita por `numeroProcesso`, sem formatação. | O exemplo 1 usa `match` sobre `numeroProcesso`. |
| Chave interna | `id` é a chave única usada pelo pacote para identidade, junções e deduplicação. | O glossário define `id` como a composição Tribunal_Classe_Grau_OrgaoJulgador_NumeroProcesso. O probe confirmou `_id == _source.id` no hit observado. |
| Assunto | `assuntos.codigo`, tipo `long`; `assuntos` é um array. | Glossário oficial. |
| Classe | `classe.codigo`, tipo `long`; `classe` é um objeto. | Glossário oficial. |
| Órgão | `orgaoJulgador.codigo`, tipo `long`; `orgaoJulgador` é um objeto. | Glossário oficial. |
| Operadores | `match` e `bool.must` são documentados. `term` e `terms` responderam HTTP 200 nos probes, mas não aparecem nos exemplos oficiais consultados. | Exemplos 1 e 2; probe de 02/09/2026. |
| Combinação futura | OR dentro de cada vetor de códigos e AND entre assunto, classe e órgão. | Decisão de interface do pacote; não é uma regra declarada pela Wiki. |
| Tamanho | Padrão documentado de 10 e máximo de 10.000 hits. | Exemplo 3. O probe aceitou `size` 0 e 1 e rejeitou 10.001 com HTTP 400; portanto a frase “variando de 10 até 10.000” não deve ser tratada como mínimo técnico. |
| Total | `hits.total` contém `value` e `relation`; a relação observada/documentada pode ser `eq` ou `gte`. | Respostas dos exemplos 1 e 2. `gte` significa que `value` é limite inferior, não total exato. |
| Paginação | Usar `search_after`, sempre com `sort`, repetindo o array `sort` do último hit como cursor. | Exemplo 3. |
| Erros | Não há esquema de erro formal na documentação pública consultada. Uma consulta inválida retornou HTTP 400 e JSON com campos de topo `error` e `status`. | Probe de 02/09/2026; o corpo e a chave não são persistidos. |

## Divergência de ordenação

O exemplo 3 da API Pública determina ordenação por `@timestamp`. A página
“API Elastic”, voltada aos tribunais e a outro endpoint, recomenda
`id.keyword`. No endpoint público observado em 02/09/2026:

- `@timestamp` retornou HTTP 200;
- `id.keyword` retornou HTTP 200 e um cursor textual;
- `id` sem o subcampo `.keyword` retornou HTTP 400.

Como timestamps podem se repetir, o pacote usa a ordenação composta por
`@timestamp` e `id.keyword`. O primeiro campo segue a recomendação oficial; o
segundo funciona como desempate determinístico com base no comportamento
observado. Cada próxima requisição repete os dois valores do `sort` anterior em
`search_after`.

## Cache reproduzível da Wiki

A cópia integral fica em `.cache/datajud-wiki/`, diretório ignorado pelo Git.
Somente o manifesto é versionado em
`inst/extdata/datajud-wiki-manifest.csv`, com URL, horário UTC, status HTTP,
URL canônica obtida, tipo de conteúdo, tamanho e hash MD5 de cada recurso. O
sitemap atualmente publica parte das rotas sem a barra final, embora o servidor
redirecione para a forma canônica com barra; ambas ficam registradas. MD5 é
usado somente para detecção reprodutível de alterações, não para segurança
criptográfica.

Para atualizar:

```sh
Rscript scripts/baixar_wiki_cnj.R
```

O downloader lê o sitemap oficial, baixa sequencialmente todas as páginas e
atualiza tanto o manifesto local quanto o versionado. Uma nova execução pode
ser comparada pelo diff do manifesto sem adicionar o conteúdo da Wiki ao Git.

## Probe real opcional

Com o pacote instalado:

```sh
Rscript scripts/probe_contrato_datajud.R TJSP
```

O probe faz três requisições pequenas: ordenação por `id.keyword`, ordenação
por `@timestamp` e consulta propositalmente inválida. A saída contém apenas
status e nomes/tipos estruturais. A chave pública, headers, URLs completas,
corpos e registros processuais nunca são impressos ou gravados.

## Fontes oficiais

- [Glossário de Dados](https://datajud-wiki.cnj.jus.br/api-publica/glossario/)
- [Exemplo 1 — número do processo](https://datajud-wiki.cnj.jus.br/api-publica/exemplos/exemplo1/)
- [Exemplo 2 — classe e órgão](https://datajud-wiki.cnj.jus.br/api-publica/exemplos/exemplo2/)
- [Exemplo 3 — paginação](https://datajud-wiki.cnj.jus.br/api-publica/exemplos/exemplo3/)
- [API Elastic — área dos tribunais](https://datajud-wiki.cnj.jus.br/para-tribunais/Datajud/Api-elastic/)
