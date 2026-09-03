# Contrato da coleta incremental NDJSON

Este documento descreve o esquema 1 da coleta criada por
`datajud_coletar_processos()`. O formato é interno ao pacote, mas é documentado
para permitir auditoria, retomada segura e evolução explícita do esquema.

## Por que NDJSON

Cada linha de um arquivo NDJSON é um hit Elasticsearch completo e independente.
Essa organização foi escolhida porque:

- permite gravar somente uma página de cada vez;
- evita formar um único objeto JSON proporcional ao tamanho total da coleta;
- permite leitura incremental e inspeção com ferramentas comuns;
- preserva `_source`, `sort` e outros campos brutos necessários à auditoria;
- não adiciona Arrow ou outro sistema de armazenamento como dependência
  obrigatória.

NDJSON não pretende substituir formatos colunares para análise intensiva. Ele é
o formato transacional da coleta: simples, portável e adequado à retomada. Uma
etapa posterior pode converter os arquivos para Parquet, banco de dados ou
tibbles sem alterar o protocolo de download.

## Arquivos

- `manifesto.json`: estado e metadados da coleta;
- `pagina-000001.ndjson`, `pagina-000002.ndjson`, ...: páginas concluídas;
- arquivos iniciados por `.pagina-` ou `.manifesto-`: temporários, nunca são
  considerados páginas válidas.

Uma página só passa a existir depois que seu temporário é fechado e renomeado.
Se o processo for interrompido entre essa renomeação e a atualização do
manifesto, a próxima execução reconcilia a única página órfã sequencial antes
de acessar a rede.

## Manifesto do esquema 1

O manifesto contém:

- versão do esquema e versão do pacote;
- tribunal;
- consulta sanitizada e seu hash MD5;
- estado, limites vigentes e horários UTC;
- contagens de registros, páginas e requisições concluídas;
- próximo cursor;
- para cada página: número, arquivo, quantidade, primeiro e último `id`, cursor
  de entrada, cursor de saída, checksum MD5 e indicação de reconciliação;
- página e mensagem sanitizada quando ocorre uma falha.

Os estados possíveis são `em_andamento`, `parcial`, `completa`,
`limite_registros` e `limite_paginas`.

## Compatibilidade e integridade

Uma retomada somente ocorre quando tribunal, hash da consulta e versão do
esquema são compatíveis. Antes de qualquer nova requisição, o pacote verifica a
numeração das páginas, a cadeia de cursores, as contagens e todos os checksums.
Nenhum arquivo incompatível é sobrescrito automaticamente.

O manifesto nunca armazena a chave pública, headers de autenticação ou o objeto
do cliente HTTP. Mensagens de falha também removem a chave antes da gravação.
