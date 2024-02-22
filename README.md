
# Pacote Datajud

O pacote Datajud é uma ferramenta não oficial desenvolvida para
facilitar o acesso à API Pública do Datajud, disponibilizada pelo
Conselho Nacional de Justiça (CNJ). Este documento fornece um guia sobre
como instalar o pacote no R, aceitar os termos de uso do CNJ, e um fluxo
básico de utilização do pacote.

O pacota ainda está em desenvolvimento e é normal que existam erros ou
problemas. Por favor, reporte caso haja algum funcionamento inesperado
ou indesejado.

## Termos de Uso

A utilização do pacote Datajud implica na aceitação dos termos de uso
estabelecidos pelo CNJ. Os principais pontos incluem:

- **Aceitação Tacitamente**: Ao utilizar o pacote, você aceita
  tacitamente as condições estabelecidas pelo CNJ.
- **Responsabilidade do Usuário**: Você se responsabiliza pelo uso da
  interface e das informações obtidas através dela.
- **Uso Legal e Não Comercial**: O pacote deve ser utilizado
  exclusivamente para fins legais e não comerciais.
- **Proibição de Modificação e Exploração Comercial**: É proibido
  modificar, distribuir, vender, ou explorar comercialmente a API ou
  qualquer informação derivada dela.
- **Obrigação de Notificação ao CNJ**: O usuário deve notificar o CNJ
  sobre qualquer publicação de informação, notícia, estudo, ou documento
  obtido através do uso da API.

Para mais detalhes, visite os [termos de uso
completos](https://datajud-wiki.cnj.jus.br/api-publica/termo-uso).

## Instalação

Para instalar o pacote Datajud do GitHub, você precisará ter o pacote
`devtools` instalado no R. Se ainda não tiver, pode instalá-lo
utilizando o comando `install.packages("devtools")`.

``` r
 devtools::install_github("rfdornelles/datajud")
```

# Carregar o pacote

``` r
library(datajud)
```

# Fluxo Básico de Utilização

## 1. Identificação do Usuário

Antes de utilizar as funcionalidades do pacote, é necessário realizar a
identificação com seu email através do datajud_login.

``` r
datajud_login("seu.email@dominio.com")
```

## 2. Pesquisar Processos

Você pode iniciar a pesquisa de processos judiciais especificando o
número do processo e o tribunal. Se não souber o Tribunal, o código
tentará advinhar automaticamente

``` r
datajud_consultar_processo(processo = "numero_do_processo", tribunal = "sigla_do_tribunal")
```

Para pesquisar por classe e órgão no tribunal especificado:

``` r
datajud_pesquisar_classe_orgao(tribunal = "sigla_do_tribunal", lista_classe = c(1116), lista_orgao = c(13597))
```

As funções de pesquisa irão criar um objeto no seu ambiente chamado
datajud_resposta. Caso já exista algum com esse nome, ele criará um
sequencial como datajud_resposta_1, etc.

## 3. Ler Dados dos Processos

Após a pesquisa, você pode ler os dados dos processos ou as
movimentações obtidas.

``` r
datajud_ler_processo()
datajud_ler_movimentacoes()
```

# Autorização e Licença

O autor do pacote Datajud, autoriza o uso não comercial do mesmo, nos
termos da licença GPL-3.

Não há qualquer compromisso ou responsabilidade do autor do pacote sobre
sua utilização e/ou sobre os dados obtidos. Utilize com responsabilidade
e consulte sempre uma pessoa jurista para melhor entender o contexto
jurídico.

# Contribuições

Contribuições são bem-vindas, assim como sugestões, críticas e PR! O
pacote foi desenvolvido com foco na usabilidade e, por isso, algumas
práticas não ideais foram seguidas (tais como criar objetos no global
environment). A ideia é facilitar o uso dos dados públicos do CNJ por
pessoas que não têm tanto conhecimento técnico.

Por outro lado, fui bastante rigoroso com boas práticas de consumo de
API pública exigindo a identificação da pessoa, impedindo paralelismo e
pausando as requisições. O Datajud é uma demanda antiga e, por isso, é
importante utilizá-lo com parcimônia e respeito!

Em caso de contribuições, tenha isso em mente.