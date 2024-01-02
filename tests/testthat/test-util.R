test_that("null-or-value works", {
  expect_equal(1 %||% NULL, 1)
  expect_equal(1 %||% 2, 1)
  expect_equal(NULL %||% NULL, NULL)
  expect_equal(NULL %||% 2, 2)
})


test_that("can convert a vector to string representation", {
  expect_equal(vector_to_str("x"), '"x"')
  expect_equal(vector_to_str(c("x", "y")), 'c("x", "y")')
})
