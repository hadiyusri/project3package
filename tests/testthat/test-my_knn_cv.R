test_that("Error check if contain NA in data", {
  expect_error((my_knn_cv(my_penguins, my_penguins[["species"]], 1, 2)))
})

my_penguins <- my_penguins %>% drop_na()

test_that("Output check for type of data", {
  expect_type(my_knn_cv(my_penguins, my_penguins[["species"]], 1, 2), "list")
})
