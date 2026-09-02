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

chave_publica_teste <- function(variante = 1L) {
  c(
    "QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo=",
    "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=",
    "MDEyMzQ1Njc4OUFCQ0RFRkdISUpLTE1OT1A="
  )[[variante]]
}
