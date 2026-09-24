# Proteção da branch principal

Esta pasta registra a configuração esperada do repositório `rfdornelles/datajud`.
Os JSON são corpos de requisição da API de rulesets; versioná-los não altera as
configurações remotas automaticamente. Consulte os
[rulesets ativos](https://github.com/rfdornelles/datajud/rules).

## Regras e exceções

- `protect.json` mantém as regras existentes de bloqueio de exclusão e force push,
  commits assinados, revisão de CODEOWNERS, conversas resolvidas, CI e revisão
  automática do Copilot. Exige uma aprovação e invalida aprovações após novos
  commits. O único ator de bypass é `rfdornelles`, limitado a `pull_request`.
- `main-ci-obrigatoria.json` exige `R 4.2` e `Medir cobertura no R 4.2`, enviados
  pelo GitHub Actions (integration ID 15368), com branch atualizada. Sua lista de
  bypass é vazia: a exceção de `protect` não dispensa esses checks.
- Ambos são ativos e atingem `~DEFAULT_BRANCH`, atualmente `main`.
- `allow_auto_merge` permanece `false`. Merge é manual e pertence ao mantenedor.

Rulesets que atingem a mesma branch são aplicados em conjunto. O modo de bypass
`pull_request` obriga o ator a usar um PR, em vez de liberar push direto. Veja a
[composição de regras](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-rulesets/about-rulesets)
e a [configuração de bypass](https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-rulesets/creating-rulesets-for-a-repository).

O GitHub não permite que alguém aprove seu próprio PR. Como o mantenedor é o
único colaborador com escrita e pode ser autor dos PRs preparados com sua conta,
ele conserva bypass de `protect` **somente em PRs**, após revisão manual do diff.
Essa exceção abrange o ruleset `protect`, inclusive suas regras de aprovação e
assinatura; não é uma aprovação técnica independente. A CI continua obrigatória
pelo segundo ruleset. Para exigir revisão independente também nesses PRs, seria
necessário outro revisor autorizado, fora do escopo desta entrega. Consulte as
[restrições de aprovação](https://docs.github.com/en/pull-requests/how-tos/review-pull-requests/approving-a-pull-request-with-required-reviews).

Agente e pessoa usando a mesma credencial não são identidades distintas para o
GitHub. A obrigação de o agente não realizar merge é uma política operacional,
documentada em [CONTRIBUTING.md](../../CONTRIBUTING.md), e não uma permissão nova
concedida ao agente. A auditoria verifica que não há outros colaboradores com
acesso de escrita e que auto-merge está desativado. Um administrador ainda pode
editar os próprios rulesets; mudanças de configuração precisam de revisão.

## Auditar

Na raiz, sem modificar o GitHub:

```sh
python3 scripts/verificar_politicas.py
python3 scripts/verificar_politicas.py --github
```

O primeiro comando verifica JSON, checks existentes e links locais dos guias.
O segundo exige GitHub CLI autenticado e rede: compara a configuração com os
rulesets ativos, confere auto-merge, colaboradores e as regras efetivas da main.
Não faz tentativas de push ou merge na branch protegida.

## Atualizar ou restaurar

Antes de uma alteração, exporte os rulesets atuais com `gh api` e preserve o
resultado fora do Git, em diretório temporário. O mantenedor deve revisar o diff
do JSON antes de aplicar um PUT ao ID existente. Para criar uma regra adicional,
confira primeiro se o nome já existe e use POST apenas se estiver ausente.
Depois, execute a auditoria remota. Não substitua regras de outros propósitos
nem apague a configuração anterior sem comparação.

Se precisar restaurar, prepare o corpo do PUT a partir do backup, usando apenas
`name`, `target`, `enforcement`, `conditions`, `rules` e `bypass_actors`, e revise
as diferenças. Não restaure bypass irrestrito ou remova checks como parte de uma
correção de código; mudanças de proteção devem ser tratadas explicitamente.
