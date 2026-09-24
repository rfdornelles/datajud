# Checklist de revisão e release

Use este roteiro em uma branch exclusiva e copie a evidência para o PR. Marque
itens não aplicáveis com justificativa. Este documento define o processo; não
executa nem autoriza a publicação de um release.

## Contrato e documentação

- [ ] Issue aprovada e dependências concluídas no Project.
- [ ] Assinaturas, padrões, classes, campos, tipos e cardinalidades revisados.
- [ ] Classificação da mudança e eventual migração seguem [COMPATIBILIDADE.md](COMPATIBILIDADE.md).
- [ ] `NEWS.md`, ajuda roxygen, README, vinhetas e dicionário estão coerentes.
- [ ] `Rscript -e 'devtools::document()'` não produz alterações inesperadas.
- [ ] Exemplos de rede protegidos; exemplos offline executáveis e links válidos.

## Testes e pacote

- [ ] `Rscript -e 'devtools::test()'` aprovado; snapshots revisados.
- [ ] `Rscript scripts/verificar_vinhetas.R` aprovado em sessão limpa.
- [ ] Cobertura >= 90% com as exclusões explícitas da CI.
- [ ] `R CMD check` sem erros ou warnings; notes investigadas e justificadas.
- [ ] Checks obrigatórios de R 4.2 e cobertura aprovados no commit atual do PR.
- [ ] Para release: validar também o R mínimo declarado e o R atual, em bibliotecas
  temporárias com dependências instaladas explicitamente, registrando versões e
  plataforma. CI verde em R 4.2 não comprova execução no R mínimo 4.1.

Comandos de check na raiz:

```sh
Rscript -e 'rcmdcheck::rcmdcheck(args="--no-manual", error_on="note")'
```

Na preparação de release, execute também o modo `--as-cran` com acesso aos
serviços necessários às verificações remotas. A suíte comum e a construção de
vinhetas continuam independentes da API do CNJ. As duas execuções têm finalidades
diferentes; não repita a suíte sem uma mudança ou falha que justifique.

## Dados, segredos e artefatos

- [ ] Nenhuma credencial pessoal, `.Renviron`, `.env`, cliente serializado ou dado
  real identificável foi incluído. Inspecione arquivos e diff sem imprimir segredos.
- [ ] Nenhum cache de downloads ou artefato de build entrou no Git.
- [ ] Arquivos grandes novos têm necessidade e tamanho revisados.
- [ ] Quando TPU mudou: fonte oficial, versão, instante e hashes registrados;
  reprodução offline validada conforme [README-tpu](data-raw/README-tpu.md).
- [ ] Quando fixtures mudaram: exemplos regenerados com
  `Rscript data-raw/exemplos.R`, dicionário e testes atualizados.
- [ ] No release: revalidar o contrato remoto e a versão TPU em tarefa explícita,
  com resultados sanitizados e sem misturar a validação remota aos testes comuns.

## Revisão e proteção da main

- [ ] `git diff --check` aprovado e diretório de trabalho revisado.
- [ ] Branch atualizada com `main`; mudanças limitadas à entrega descrita.
- [ ] Revisão manual do mantenedor concluída e conversas resolvidas.
- [ ] Rulesets ativos conferidos conforme [.github/rulesets/README.md](.github/rulesets/README.md).
- [ ] Checks obrigatórios sem bypass e `allow_auto_merge = false` confirmados.
- [ ] Permissões conferidas: acesso de escrita/merge limitado ao mantenedor;
  bots e agentes não recebem permissão adicional de merge.

## Preparar e publicar

- [ ] Versão em `DESCRIPTION` e seção correspondente do `NEWS.md` propostas no PR.
- [ ] Mantenedor revisou o resultado e fez o merge manual.
- [ ] **Somente o mantenedor**, em ação posterior e explícita, decide criar a tag,
  publicar o release e eventualmente enviar ao CRAN.

O agente encerra a entrega com o PR e as evidências. Não use `gh pr merge`,
`gh release create`, push de tags ou auto-merge como parte de um pedido genérico
para continuar o desenvolvimento. Se surgir uma falha depois da publicação,
registre a correção e sua versão; não mova tags publicadas silenciosamente.
