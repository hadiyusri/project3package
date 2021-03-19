#
# Two-sided test
test_that("my_t.test works mathematically with two.sided",{
  my_value <- my_t.test(c(0.5, 1.0, 1.5, 2.0), alternative = "two.sided", mu = 1.0)
  expect_value <- t.test(c(0.5, 1.0, 1.5, 2.0), alternative = "two.sided", mu = 1.0)
  expect_true(my_value$test_stat == expect_value$statistic)
  expect_true(my_value$df == expect_value$parameter)
  expect_true(my_value$p_val == expect_value$p.value)
})

# Greater
test_that("my_t.test works mathematically with greater",{
  my_value <- my_t.test(c(1.0, 4.1, 4.3, 4.2, 2.8), "greater", 2.0)
  expect_value <- t.test(c(1.0, 4.1, 4.3, 4.2, 2.8), "greater", 2.0)
  expect_true(my_value$test_stat == expect_value$statistic)
  expect_true(my_value$df == expect_value$parameter)
  expect_true(my_value$p_val == expect_value$p.value)
})

# Less
test_that("my_t.test works mathematically with less",{
  my_value <- my_t.test(c(1.9, 2.3, 4.8, 3.0, 4.2), "less", 1.2)
  expect_value <- t.test(c(1.9, 2.3, 4.8, 3.0, 4.2), "less", 1.2)
  expect_true(my_value$test_stat == expect_value$statistic)
  expect_true(my_value$df == expect_value$parameter)
  expect_true(my_value$p_val == expect_value$p.value)
})

# error check
test_that("my_t.test show error with invalid alternatives", {
  expect_error(my_t.test(c(1.0, 4.0, 2.0, 3.0), "one.sided",  0.4))
})
