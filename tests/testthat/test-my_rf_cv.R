test_that("Non-numeric error check for input", {
  expect_error(my_rf_cv("String"))
})

test_that("zero error check for input", {
  expect_error(my_rf_cv(0))
})

test_that("Output type of data check", {
  expect_type(my_rf_cv(5), "double")
})

test_that("negative error check for input", {
  expect_error(my_rf_cv(-1))
})
