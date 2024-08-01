test_that("multiplication works", {
  expect_length(get_county_fips(c('CA', 'oregon', 'nj')), 115)

  expect_length(get_county_fips('california'), 58)

  expect_length(get_county_fips('new york'), 62)
})
