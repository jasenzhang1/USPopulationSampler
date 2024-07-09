test_that("simple case", {
  expect_equal(jasenfirst(5), 25)
  expect_equal(jasenfirst(-5), 25)
  expect_equal(jasenfirst(4.2), 17.64)
})

test_that('no strings', {
  expect_error(jasenfirst('a'), 'non-numeric argument')
})


