test_that("multiplication works", {
  expect_equal(capitalize_first('ny'), 'Ny')

  expect_equal(capitalize_first('aBCDEF'), 'Abcdef')

  expect_equal(capitalize_first('a3490shc'), 'A3490shc')
})
