# Compatibilidade e ciclo de vida

## Escopo do contrato público

São públicos os objetos exportados em `NAMESPACE`, seus argumentos documentados,
classes retornadas, campos e tipos descritos no dicionário, classes de erro
anunciadas na ajuda, e o formato versionado de coletas persistidas. Os prefixos
`datajud_` ajudam na descoberta; não tornam públicas funções internas acessadas
com `:::`. Use nomes nos argumentos opcionais, especialmente `cliente`.

O conteúdo da API depende do CNJ. Mudanças remotas de disponibilidade, nomes,
contagens e registros não são garantias de estabilidade do pacote. O pacote deve
validar respostas incompatíveis e apresentar erro claro, sem mudar silenciosamente
o significado dos retornos. Mudanças das TPU devem registrar versão e proveniência.

## Antes de 1.0.0

A versão atual de desenvolvimento é experimental. Mudanças incompatíveis podem
ocorrer em versões de desenvolvimento, mas exigem registro no `NEWS.md`, exemplos
de migração e revisão do mantenedor. A política abaixo passa a reger releases
estáveis a partir de 1.0.0; sua publicação não declara a versão atual estável.

## Depois de 1.0.0

O projeto adota `MAJOR.MINOR.PATCH` como política de release:

| Mudança | Versão mínima |
|:--|:--|
| Correção que mantém o contrato e a intenção documentada | PATCH |
| Nova função ou argumento opcional compatível, com padrão preservado | MINOR |
| Remoção/renomeação pública, novo argumento obrigatório ou mudança incompatível de tipo, cardinalidade ou padrão | MAJOR |
| Elevação do R mínimo suportado ou quebra de leitura de arquivos persistidos | MAJOR |

Adicionar colunas ou classes de erro pode afetar consumidores que dependem de
nomes exatos. Avalie o impacto, documente-o e só classifique como MINOR quando o
contrato admitir a extensão e preservar os usos documentados. Correções de bugs
que alterem resultados precisam explicar a diferença, mesmo sem mudança de assinatura.
Atualizações de TPU podem alterar linhas, nomes e status: são anunciadas no NEWS
com a versão dos dados e preservam esquema e proveniência.

## Deprecar antes de remover

1. Introduza e documente a alternativa, com exemplos equivalentes.
2. Anuncie a depreciação em uma versão MINOR, com aviso identificável e caminho
   de migração. Preserve o comportamento antigo quando for viável e seguro.
3. Mantenha a compatibilidade por pelo menos uma versão MINOR publicada e seis
   meses, contados do primeiro aviso. Se ambos ainda não foram cumpridos, adie.
4. Remova apenas em uma versão MAJOR e registre o que deixou de funcionar.

Falhas de segurança ou imposições da API podem impedir a transição normal.
Nesses casos o mantenedor documenta a justificativa, o impacto, a alternativa
possível e a versão escolhida. Não esconda uma quebra no changelog como simples
refatoração. Esta é uma política do projeto; não exige adotar uma biblioteca
específica de depreciação.

## Dados e coletas persistidas

`versao_esquema` identifica o formato do manifesto NDJSON, independentemente da
versão do pacote. Mudanças incompatíveis no formato incrementam esse número e
precisam de leitor compatível ou migração explícita. Nunca reinterprete um
manifesto antigo usando silenciosamente outro esquema, nem sobrescreva uma
coleta durante sua migração sem instrução do usuário.

O dicionário e os testes de fixtures acompanham cada alteração. As saídas vazias,
valores ausentes, ordenação e unidade/fuso das datas fazem parte da revisão de
compatibilidade. Teste também os arquivos sintéticos distribuídos. Não prometa
compatibilidade para toda a estrutura bruta da API: os campos remotos adicionais
são preservados, e os campos consumidos pelo pacote têm contrato documentado.

## Evidência no PR

Uma mudança pública deve indicar: contrato anterior, contrato novo, impacto nos
usuários, versão planejada e exemplo de migração quando necessário. Na ausência
de quebra, declare o motivo da compatibilidade. A aprovação e o merge continuam
com o mantenedor, conforme [CONTRIBUTING.md](CONTRIBUTING.md).
