test_that("function works", {
  expect_setequal(convert_state_names_FIPS(c('nj', 'pa', 'NY')), c("34", "36", "42"))

  expect_equal(convert_state_names_FIPS('new york'), '36')

  expect_equal(convert_state_names_FIPS('New york'), '36')
})
