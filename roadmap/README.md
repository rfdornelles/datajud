# Roadmap de melhorias

O arquivo `issues.json` é o manifesto versionado das melhorias planejadas para
o pacote Datajud. A aprovação e o andamento de cada item são mantidos no
[GitHub Project](https://github.com/users/rfdornelles/projects/4); o JSON não é
usado como cópia do estado dinâmico do quadro.

## Fluxo de aprovação

Cada issue entra no Project com:

- `Etapa`: `Backlog`;
- `Decisão`: `Aguardando aprovação`;
- fase e prioridade definidas pelo manifesto.

Uma implementação só pode começar quando `Decisão` estiver como `Aprovada` e
todas as dependências da issue estiverem concluídas. Cada entrega usa uma
branch e um pull request exclusivos. O mantenedor é sempre responsável pela
revisão e pelo merge.

O campo customizado se chama `Etapa` porque `Status` é um campo nativo reservado
do GitHub Projects e suas opções não podem ser substituídas pelo publicador.

## Publicação reproduzível

Para validar o manifesto sem alterar o GitHub:

```sh
Rscript scripts/publicar_roadmap.R
```

Para criar ou sincronizar labels, milestones, Project e issues:

```sh
Rscript scripts/publicar_roadmap.R --publicar
```

O publicador usa o marcador `roadmap_id` no corpo das issues, compara os campos
existentes e grava um checkpoint após cada item. Assim, uma execução
interrompida pode ser retomada sem duplicar recursos.

É necessário autenticar o GitHub CLI com os escopos `repo`, `workflow` e
`project`. A pasta `.cache/datajud-wiki/` permanece ignorada e será usada mais
adiante para a cópia local reproduzível da documentação oficial do CNJ.
