library(tidyverse)
library(gapminder)
data("my_gapminder")
x <- my_gapminder %>% pull(lifeExp)
test_that("t.test works", {
  expect_output(my_t.test(x, "two.sided", 60))
  expect_output(my_t.test(x, "less", 60))
  expect_output(my_t.test(x, "greater", 60))
})
