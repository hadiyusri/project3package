data("mtcars")
formula <- mpg ~ hp + wt
data <- mtcars

test_that("Check for error for output type", {
  expect_type(my_lm(formula, data), "list")
})

test_that("Check for error if wrong input type", {
  expect_error(my_lm("string", y ~ x))
})
