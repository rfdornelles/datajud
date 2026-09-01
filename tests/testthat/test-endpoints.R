test_that("endpoint de tribunal conhecido é resolvido sem rede", {
  endpoint <- datajud::aux_retorna_endpoint("TJSP")

  expect_type(endpoint, "character")
  expect_length(endpoint, 1L)
  expect_match(endpoint, "^https://")
})

test_that("tribunal desconhecido gera erro acionável", {
  expect_error(
    datajud::aux_retorna_endpoint("TRIBUNAL_INEXISTENTE"),
    "não encontrado"
  )
})
