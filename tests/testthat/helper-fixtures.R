carregar_fixture <- function(nome) {
  jsonlite::read_json(
    testthat::test_path("fixtures", nome),
    simplifyVector = FALSE
  )
}

texto_fixture <- function(nome) {
  paste(
    readLines(testthat::test_path("fixtures", nome), encoding = "UTF-8"),
    collapse = "\n"
  )
}
