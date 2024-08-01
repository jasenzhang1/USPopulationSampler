test_that("function works", {
  expect_setequal(convert_state_names(c('nj', 'pa', 'NY')), c("New Jersey", "Pennsylvania", "New York"))

  expect_equal(convert_state_names('md'), 'Maryland')

  expect_equal(convert_state_names('alabama'), 'Alabama')

  expect_equal(convert_state_names('CA'), 'California')
})
