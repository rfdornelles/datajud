###
## Tabela com os tribunais e endpoints

### scrap Datajud
url <- "https://datajud-wiki.cnj.jus.br/api-publica/endpoints/"

r0 <- httr::GET(url) |>
  httr::content(encoding = "UTF-8")

## ler tabelas de Tribunais

tabelas <- r0 |>
  rvest::html_table()

## empilhar tabelas
df_endpoints <- dplyr::bind_rows(tabelas) |>
  janitor::clean_names()

## extrair siglas
tribunais <- df_endpoints |>
  dplyr::mutate(
    sigla = stringr::str_extract(url,
                                 "(?<=api_publica_)([^\\/]+)"),
    # classificar ramo da justiça
    tipo = dplyr::case_when(
      stringr::str_detect(tribunal, "Superior") ~ "Superior",
      stringr::str_detect(url, "trt") ~ "Trabalhista",
      stringr::str_detect(url, "tre") ~ "Eleitoral",
      stringr::str_detect(url, "trf") ~ "Federal",
      stringr::str_detect(url, "tjm") &
        stringr::str_detect(tribunal, "Militar") ~ "Militar Estadual",
      # não possuem endpoint mas podem vir a ter
      stringr::str_detect(url, "cjm") ~ "Militar Federal",
      TRUE ~ "Estadual"
    ),

    ## arrumar tribunais que estão com o mesmo nome
    tribunal = dplyr::case_when(
      sigla == "tjdft" ~ "Tribunal de Justiça do Distrito Federal e dos Territórios",
      sigla == "tre-dft" ~ "Tribunal Regional Eleitoral do Distrito Federal",
      sigla == "tre-ba" ~ "Tribunal Regional Eleitoral da Bahia",
      TRUE ~ tribunal
    ),

    ## arrumar as siglas "TJ",
    tribunal = stringr::str_replace(tribunal, "TJ", "Tribunal de Justiça"),
) |>
  dplyr::relocate(sigla, tipo, .after = tribunal) |>
  tibble::as_tibble()

## exportar o dado
usethis::use_data(tribunais, overwrite = TRUE)
