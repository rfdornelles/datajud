# Atualização das TPU

Os datasets `datajud_assuntos` e `datajud_classes` são cópias locais das
planilhas oficiais do [SGT/CNJ](https://www.cnj.jus.br/sgt/versoes.php).
O pacote nunca consulta o SGT durante o carregamento ou uso dos datasets.

Na raiz do repositório, com as dependências do pacote instaladas:

```sh
Rscript data-raw/tpu.R --atualizar
Rscript data-raw/tpu.R --offline
```

O primeiro comando descobre a versão publicada para assuntos e classes,
seleciona todas as planilhas de dados (exclui versões para impressão), baixa
as fontes e a tabela SQL completa da mesma versão em `.cache/tpu/` e consolida
os registros. O segundo reproduz os
artefatos usando exclusivamente o cache, conferindo os hashes registrados.
Sem argumentos, o script reproduz o manifesto e baixa apenas fontes ausentes.
Se o CNJ substituir um arquivo na mesma URL, o hash divergente impede que a
reprodução use uma fonte diferente silenciosamente. Preserve o cache para
reprodução futura: a disponibilidade dos bytes originais no CNJ não é garantida.

`inst/extdata/tpu-fontes.csv` registra uma linha por fonte e tipo, com URL, formato,
versão, segmento, rótulo completo do grau/órgão, nome de arquivo, data de download
UTC, hash MD5 e URL/hash do catálogo utilizado na descoberta. O atributo
`tpu_fontes` de cada dataset contém as respectivas linhas do manifesto.
Os hashes são checksums de reprodução, não assinaturas de autenticidade.

As planilhas `.xls` são HTML em ISO-8859-1. O parser usa somente as linhas
externas da tabela, preserva células raiz sem `tr` e expande `colspan`.
Tabelas de glossário aninhadas e legendas não geram registros. Separadores de
ancestrais sem nome são ignorados. Pais ausentes são recuperados recursivamente
da tabela `ITENS` do SQL oficial, filtrada por tipo (assunto ou classe), sem
executar comandos SQL. Esse complemento fornece somente código, nome, pai e
status: as datas SQL têm semântica diferente das datas publicadas no XLS e não
são copiadas. Datas ficam `NA`, e segmentos/graus ficam `character(0)` nesses
ancestrais, sem inferir aplicabilidade a partir dos filhos. O atributo
`tpu_ancestrais_sql` identifica os códigos complementados. Na versão 83 são 25
assuntos, incluindo dois ancestrais encontrados recursivamente. A geração
falha se mesmo a tabela completa não contiver um pai necessário.

Códigos e pais são inteiros, nomes são UTF-8, status é lógico e datas são
`Date` (horários da exportação são descartados). O texto riscado na exportação
indica inatividade; a ausência de data de inativação não implica atividade.
Segmentos e graus são listas ordenadas sem duplicatas. Listas vazias indicam
aplicabilidade não informada para ancestrais recuperados do SQL. Os graus preservam os
rótulos completos do SGT para não perder o vínculo com os segmentos.
Nomes, pais e status devem coincidir entre fontes. Datas ausentes podem ser
completadas por outra fonte. Para publicação, a coluna registra a menor data
informada entre os segmentos; quando há divergência, todas as variantes com
segmento e grau ficam no atributo `tpu_publicacoes_divergentes`. Isso ocorre,
por exemplo, no assunto 15405 da versão 83 (Eleitoral e STF). Nas demais datas,
valores preenchidos divergentes causam erro.

Ambos os datasets são consolidados e validados antes da gravação: códigos
únicos, tipos, pais existentes e ausência de ciclos. Após atualizar, regenere
a documentação, rode a suíte offline e `R CMD check` e revise o diff dos
artefatos e do manifesto no pull request. Não versione o cache de downloads.
